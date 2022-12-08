// Package actions contains helper functions for implementing actions (plugins) in a program using
// Z3S5 Lisp. The Go wrapper of an action takes a base directory, a prefix, and the name of an action. The last two must
// be strings containing only unicode letters, digits, and the underscore character. The action directory
// is created at relpath/prefix/action-name/. This directory must contain a file program.lisp that can be loaded
// as a library using `(load 'action-name)` and must register the action under its name when `(<action-name>.run)`
// is called. The function `action.Load()` does exactly this. The function `action.Run()` runs the action
// by looking it up in the action registry `*actions*` and starting it via `action-start`.
//
// Each action must for itself call the respective action.progress and action.result functions of the host
// system to allow Go feedback. Note that `action-start` executes an action asynchronously in a task. The
// action's `proc` should check and react to the 'stop signal using `task-recv`.
//
// The action system assumes that other information such as an action's icon is available in the action
// directory. All data of an action must be kept within the action's local directory.
//
// Actions reside within the base directory within directories with the prefix, and under this directory in
// a directory with the action name (case insensitive). When an action is renamed, it is both renamed in the Lisp system where
// it is registered and on disk by renaming the directory. Action prefixes can also be changed, which also requires
// changing the action directory and the action prefixes on the Lisp side.
//
// Although this is cumbersome and a bit more error-prone, this and additional checks guarantee that
// each action resides in a directory of its own name and has a unique name within the prefix directory. The action id is only
// used for faster access internally. This keeps action directories user-maintainable and readable.
package actions

import (
	"errors"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	z3 "github.com/rasteric/z3s5-lisp"
)

var ErrInvalidActionName = errors.New("a new action could not be created because its name is not valid")
var ErrDuplicateActionDir = errors.New("an action could not be created or renamed because the action directory is already in use")
var ErrDuplicateActionName = errors.New("an action with that name is already registered")
var ErrRenameActionFailed = errors.New("renaming an action has failed")
var ErrInvalidPrefixName = errors.New("a new action could not be created because the prefix is invalid")

// Action is a struct holding all data for an action, some of which is cached because
// retrieving it from the interpreter every time is too inefficient.
type Action struct {
	name   string
	prefix string
	id     string
	path   string
}

// Actions is an array of actions with some extra convenience methods.
type Actions []*Action

// IsValidActionName returns true if the given name is a valid name for an action.
// This function is very strict and only allows unicode letters, digits, _ and -.
func IsValidActionName(name string) bool {
	for _, r := range name {
		if !IsValidActionNameRune(r) {
			return false
		}
	}
	return true
}

// IsValidFilenameRune returns true if the given rune is a valid file name component,
// according to some very strict rules that only allows letters, digits, score, and the underscore.
func IsValidActionNameRune(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsNumber(r) || r == '_' || r == '-'
}

// IsValidPrefixName returns true if the given prefix string contains valid characters.
// This currently has the same rules as action names, see IsValidActionName() for more info.
func IsValidPrefixName(prefix string) bool {
	return IsValidActionName(prefix)
}

// SubdirAvailable returns true if a subdir of root with the given name already exists.
// This function checks for any case-variants of name even on case-sensitive filesystems.
// If a filesystem error occurs, this function returns true and the error.
func SubdirNameTaken(root, name string) (bool, error) {
	infos, err := os.ReadDir(root)
	if err != nil {
		return true, err
	}
	found := false
	for _, info := range infos {
		if strings.EqualFold(info.Name(), name) {
			found = true
			break
		}
	}
	if found {
		return true, nil
	}
	return false, nil
}

// HasAction returns true if the given interpreter has an action registered with the given
// name. An error and true is returned when an error occurs.
func HasAction(interp *z3.Interp, prefix, name string) (bool, error) {
	b, err := interp.EvalString(fmt.Sprintf("(has-action? \"%v\" \"%v\")", prefix, name))
	if err != nil {
		return true, err
	}
	hasAction, ok := b.(bool)
	if ok && hasAction {
		return true, ErrDuplicateActionName
	}
	return false, nil
}

// ======
// Action
// ======

// ComposeActionName returns a canonical action name composed out of prefix and name.
// The name is specific to a particular platform.
func ComposeActionName(prefix, name string) string {
	return prefix + string(os.PathSeparator) + name
}

// StopActionTask request the interpreter to stop an action task by sending the 'stop signal.
// This is a shortcut to avoid having to find the running copy of the action itself.
// Use action.Stop() if you have the action available.
func StopActionTask(interp *z3.Interp, taskid string) {
	if _, err := strconv.Atoi(taskid); err != nil {
		log.Printf("WARN StopActionTask: action taskid is not valid, given %v\n", taskid)
		return
	}
	interp.EvalString(fmt.Sprintf("(task-send %v 'stop)", taskid))
}

// NewAction creates a new action based on basedir and the given name, which must be a valid
// subdirectory in basedir that must contain a proper program.lisp file containing
// a matching action registered with a name that is case-insensitive equal to the directory name.
// In other words, the action in Lisp must be defined with a name that matches the subdirectory name.
// This makes renaming actions harder but makes the directory structure much more readable and human
// editable. It is always basedir/prefix/action1, basedir/prefix/action2, and so on, and each action name
// and prefix may only contain alphanumeric characters, _, and -. See action.Rename() for the renaming of
// an action, which involves renaming the action's directory.
func NewAction(interp *z3.Interp, basedir, prefix, name string) (*Action, error) {
	if !IsValidActionName(name) {
		return nil, ErrInvalidActionName
	}
	if !IsValidPrefixName(prefix) {
		return nil, ErrInvalidPrefixName
	}
	if ok, _ := HasAction(interp, prefix, name); ok {
		return nil, ErrDuplicateActionName
	}
	dir := filepath.Join(basedir, prefix, name)
	a := &Action{
		name:   name,
		prefix: prefix,
		path:   dir,
	}
	if err := a.load(interp); err != nil {
		return a, err
	}
	return a, nil
}

// Name returns the name of the action.
func (a *Action) Name() string {
	return a.name
}

// Path returns the action's path.
func (a *Action) Path() string {
	return a.path
}

// Prefix returns the action's prefix.
func (a *Action) Prefix() string {
	return a.prefix
}

// PrefixName returns the prefix plus the name as a path string
// of the form prefix/name.
func (a *Action) PrefixName() string {
	return ComposeActionName(a.prefix, a.name)
}

// Id returns the action's identity string, a GUI nonce.
func (a *Action) Id() string {
	return a.id
}

// Rename renames the action to the given name. This changes the actions file path and may fail if the new
// name is not valid.
func (a *Action) Rename(interp *z3.Interp, newName string) error {
	// check the new name is valid
	if !IsValidActionName(newName) {
		return ErrInvalidActionName
	}

	// check the new name isn't alrady taken on the file system
	dir := filepath.Dir(a.path)
	taken, err := SubdirNameTaken(dir, newName)
	if err != nil {
		return err
	}
	if taken {
		return ErrDuplicateActionDir
	}

	// check the new name isn't already taken on the Lisp side
	hasAction, err := HasAction(interp, a.Prefix(), newName)
	if err != nil {
		return err
	}
	if hasAction {
		return ErrDuplicateActionName
	}

	// rename on the Lisp side
	result, err := interp.EvalString(fmt.Sprintf("(rename-action \"%v\" \"%v\")", a.id, newName))
	if err != nil {
		return err
	}
	renameSuccess, ok := result.(bool)
	if !(ok && renameSuccess) {
		return ErrRenameActionFailed
	}

	// rename on the file system
	newPath := filepath.Join(dir, newName)
	err = os.Rename(a.path, newPath)
	if err != nil {
		return err
	}
	a.path = newPath

	// we got here, so it's a success
	a.name = newName
	return nil
}

// load loads the action into the given interpreter, making it available for running it.
// This function loads the source code of the action and makes it ready for execution.
func (a *Action) load(interp *z3.Interp) error {
	path := filepath.Join(a.path, "program.lisp")
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return fmt.Errorf("invalid action, missing program file \"%v\": %w", path, err)
	}
	// execute the file
	if _, err := interp.EvalString(fmt.Sprintf("(load '%v \"%v\")", a.Name(), path)); err != nil {
		return fmt.Errorf("loading action \"%v\" failed; the action is invalid because a script error occurred", a.path)
	}
	if _, err := interp.EvalString(fmt.Sprintf("(%v.run)", a.Name())); err != nil {
		return fmt.Errorf("loading action \"%v\" failed; no run function or error in run function", a.path)
	}
	// load the action's name and id (cached because access is very slow)
	n, err := interp.EvalString("(prop *provided-action* 'name)")
	if err != nil {
		return fmt.Errorf("invalid action \"%v\"; the name could not be retrieved: %w", a.path, err)
	}
	name, ok := n.(string)
	if !ok {
		return fmt.Errorf("invalid action \"%v\"; the name could not be retrieved", a.path)
	}
	if !strings.EqualFold(name, a.name) {
		log.Printf("WARN the action \"%v\" has a path that does not match its name (this shoudl be avoided): %v", name, a.name)
	}
	a.name = name
	id, err := interp.EvalString("(prop *provided-action* 'id)")
	if err != nil {
		return fmt.Errorf("invalid action \"%v\"; the ID could not be retrieved: %w", name, err)
	}
	idstr, ok := id.(string)
	if !ok {
		return fmt.Errorf("invalid action \"%v\"; the ID could not be retrieved", name)
	}
	a.id = idstr
	return nil
}

// Run runs the action, which must take care of progress and reporting the result.
func (a *Action) Run(interp *z3.Interp) error {
	_, err := interp.EvalString(fmt.Sprintf("(action-start (get-action \"%v\"))", a.id))
	return err
}

// Stop stops the action, which sends the 'stop message to the Lisp task.
func (a *Action) Stop(interp *z3.Interp) error {
	_, err := interp.EvalString(fmt.Sprintf("(action-stop (get-action \"%v\"))", a.id))
	return err
}

// =======
// Actions
// =======

func strAdd(s1, s2, sep string) string {
	if s1 == "" {
		return s2
	}
	s1 += sep
	s1 += s2
	return s1
}

// NewActions creates a new Actions array from all actions in the given basedir.
// The directory structure is basedir/prefix/action. Any directory that doesn't have
// a valid prefix name is skipped, e.g. directories with a dot in the name are ignored.
// This function returns the Actions or nil, an error string, a string containing the
// names of the actions that caused the error (if any, otherwise ""), and an error
// count.
func NewActions(interp *z3.Interp, basedir string) (Actions, string, string, int) {
	prefixes, err := os.ReadDir(basedir)
	if err != nil {
		return nil, err.Error(), "", 1
	}
	actions := make(Actions, 0)
	errcount := 0
	errstr := ""
	erractions := ""
	for _, prefix := range prefixes {
		if !IsValidPrefixName(prefix.Name()) {
			continue
		}
		infos, err := os.ReadDir(filepath.Join(basedir, prefix.Name()))
		if err != nil {
			errcount++
			errstr = strAdd(errstr, err.Error(), "; ")
			return nil, errstr, erractions, errcount
		}
		for _, info := range infos {
			path := filepath.Join(basedir, prefix.Name(), info.Name())
			fileInfo, err := os.Stat(path)
			if err != nil {
				errcount++
				errstr = strAdd(errstr, err.Error(), "; ")
				erractions = strAdd(erractions, info.Name(), ", ")
				return nil, errstr, erractions, errcount
			}
			if !fileInfo.IsDir() {
				continue
			}
			action, err := NewAction(interp, basedir, prefix.Name(), info.Name())
			if err != nil {
				errcount++
				errstr = strAdd(errstr, err.Error(), "; ")
				erractions = strAdd(erractions, info.Name(), ", ")
				continue
			}
			actions = append(actions, action)
		}
	}
	return actions, errstr, erractions, errcount
}
