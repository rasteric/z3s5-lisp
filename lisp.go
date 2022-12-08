/*
  Z3S5 Lisp by Erich Rast.
  A Lisp-1 for Go based on Nukata Lisp 2.0 by SUZUKI Hisao.
  MIT License, see the accompanying LICENSE file.
*/
package z3s5

import (
	"bufio"
	"bytes"
	_ "embed"
	"errors"
	"fmt"
	"io"
	"log"
	"math/big"
	"math/rand"
	"os"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/nukata/goarith"
)

//go:embed embed/init.lisp
var initFile []byte

//go:embed embed/help.lisp
var helpFile []byte

//go:embed embed/version.lisp
var versionFile []byte

const DEFAULT_BUFFSIZE = 32768  // default buffer size for reader
const DEFAULT_BUFFMAX = 1048576 // default max buffer size

type Externalizable interface {
	Externalize(interp *Interp, env *Cell) string
}

// Cell represents a cons cell.
// &Cell{car, cdr} works as the "cons" operation.
type Cell struct {
	Car any
	Cdr any
}

// Nil is a nil of type *Cell and it represents the empty list.
var Nil *Cell = nil

// CdrCell returns cdr of the cell as a *Cell or Nil.
func (j *Cell) CdrCell() *Cell {
	if c, ok := j.Cdr.(*Cell); ok {
		return c
	}
	panic(NewEvalError("proper list expected", j))
}

// (a b c).FoldL(x, fn) returns fn(fn(fn(x, a), b), c)
func (j *Cell) FoldL(x any,
	fn func(any, any) any) any {
	for j != Nil {
		x = fn(x, j.Car)
		j = j.CdrCell()
	}
	return x
}

// Len returns the length of list j
func (j *Cell) Len() int {
	return j.FoldL(0, func(i, e any) any {
		return i.(int) + 1
	}).(int)
}

// (a b c).MapCar(fn) returns (fn(a) fn(b) fn(c))
func (j *Cell) MapCar(fn func(any) any) any {
	if j == Nil {
		return Nil
	}
	a := fn(j.Car)
	d := j.Cdr
	if cdr, ok := d.(*Cell); ok {
		d = cdr.MapCar(fn)
	}
	if Same(j.Car, a) && Same(j.Cdr, d) {
		return j
	}
	return &Cell{a, d}
}

// Same is true if two Lisp interface{} have the same values, false otherwise.
// This is needed because map[interface{}]interface{} cannot be compared!
func Same(a, b any) bool {
	if m1, ok := a.(map[any]any); ok {
		if m2, ok := b.(map[any]any); ok {
			if len(m1) != len(m2) {
				return false
			}
			for k := range m1 {
				if !Same(m1[k], m2[k]) {
					return false
				}
			}
			return true
		}
		return false
	}
	if a1, ok := a.([]any); ok {
		if a2, ok := b.([]any); ok {
			if len(a1) != len(a2) {
				return false
			}
			for i := range a1 {
				if a1[i] != a2[i] {
					return false
				}
			}
			return true
		}
		return false
	}
	return a == b
}

// Reverse the list non-destructively (creating a copy first).
func (j *Cell) Reverse() *Cell {
	return ArrayToList(ReverseArray(ListToArray(j)))
}

// String returns a raw textual representation of j for debugging.
func (j *Cell) String() string {
	return fmt.Sprintf("(%v . %v)", j.Car, j.Cdr)
}

//----------------------------------------------------------------------

// Sym represents a symbol (or an expression keyword) in Lisp.
// &Sym{name, false} constructs a symbol which is not interned yet.
type Sym struct {
	Name      string
	IsKeyword bool
}

var Void = NewSym("<void>")
var ErrorPrinter = NewSym("*error-printer*")
var ErrorHandler = NewSym("*error-handler*")
var ReflectSym = NewSym("*reflect*")

// Boxed types (mostly handled externally).
var BoxedBlob = NewSym("blob")

// NewSym constructs an interned symbol for name.
func NewSym(name string) *Sym {
	return NewSym2(name, false)
}

// symbols is a table of interned symbols.
var symbols = make(map[string]*Sym)

// symLock is an exclusive lock for the table.
var symLock sync.RWMutex

// NewSym2 constructs an interned symbol (or an expression keyword
// if isKeyword is true on its first construction) for name.
func NewSym2(name string, isKeyword bool) *Sym {
	symLock.Lock()
	sym, ok := symbols[name]
	if !ok {
		sym = &Sym{name, isKeyword}
		symbols[name] = sym
	}
	symLock.Unlock()
	return sym
}

// IsInterned returns true if sym is interned.
func (sym *Sym) IsInterned() bool {
	symLock.RLock()
	s, ok := symbols[sym.Name]
	symLock.RUnlock()
	return ok && s == sym
}

// String returns a textual representation of sym.
func (sym *Sym) String() string {
	return sym.Name
}

// Defined symbols

var BackQuoteSym = NewSym("`")
var CommaAtSym = NewSym(",@")
var CommaSym = NewSym(",")
var DotSym = NewSym(".")
var LeftParenSym = NewSym("(")
var RightParenSym = NewSym(")")
var SingleQuoteSym = NewSym("'")
var HashSym = NewSym("#")

var AppendSym = NewSym("append")
var ConsSym = NewSym("cons")
var ListSym = NewSym("list")
var RestSym = NewSym("&rest")
var UnquoteSym = NewSym("unquote")
var UnquoteSplicingSym = NewSym("unquote-splicing")
var ArraySym = NewSym("array")
var ApplySym = NewSym("apply")

// Expression keywords

var CondSym = NewSym2("cond", true)
var FutureSym = NewSym2("future", true)
var TaskSym = NewSym2("systask", true)
var LambdaSym = NewSym2("lambda", true)
var MacroSym = NewSym2("macro", true)
var ProgNSym = NewSym2("progn", true)
var QuasiquoteSym = NewSym2("quasiquote", true)
var QuoteSym = NewSym2("quote", true)
var SetqSym = NewSym2("setq", true)

//----------------------------------------------------------------------

// Func is a common base type of Lisp functions.
type Func struct {
	// Carity is a number of arguments, made negative if the func has &rest.
	Carity int
	// Args is the formal argument list of the function when it was originally defined, or nil
	// for a builin function. This is used by Externalize.
	Args *Cell
}

// hasRest returns true if fn has &rest.
func (fn *Func) hasRest() bool {
	return fn.Carity < 0
}

// fixedArgs returns the number of fixed arguments.
func (fn *Func) fixedArgs() int {
	if c := fn.Carity; c < 0 {
		return -c - 1
	} else {
		return c
	}
}

// MakeFrame makes a call-frame from a list of actual arguments.
// Argument x will be used instead of fn only in error messages.
func (fn *Func) MakeFrame(arg *Cell, x any) []any {
	arity := fn.Carity // number of arguments, counting the whole rests as one
	if arity < 0 {
		arity = -arity
	}
	frame := make([]any, arity)
	n := fn.fixedArgs()
	i := 0
	for i < n && arg != Nil { // Set the list of fixed arguments.
		frame[i] = arg.Car
		arg = arg.CdrCell()
		i++
	}
	if i != n || (arg != Nil && !fn.hasRest()) {
		panic(NewEvalError("arity not matched", x))
	}
	if fn.hasRest() {
		frame[n] = arg
	}
	return frame
}

// EvalFrame evaluates each expression of frame with interp in env.
func (fn *Func) EvalFrame(frame []any, interp *Interp, env *Cell) {
	n := fn.fixedArgs()
	for i := 0; i < n; i++ {
		frame[i] = interp.Eval(frame[i], env)
	}
	if fn.hasRest() {
		if j, ok := frame[n].(*Cell); ok {
			z := Nil
			y := Nil
			for j != Nil {
				e := interp.Eval(j.Car, env)
				x := &Cell{e, Nil}
				if z == Nil {
					z = x
				} else {
					y.Cdr = x
				}
				y = x
				j = j.CdrCell()
			}
			frame[n] = z
		}
	}
}

//----------------------------------------------------------------------

// Macro represents a compiled macro expression.
type Macro struct {
	Func
	// body is a list which will be used as the function body.
	body *Cell
}

// NewMacro constructs a Macro.
func NewMacro(carity int, args *Cell, body *Cell, env *Cell) any {
	return &Macro{Func{carity, args}, body}
}

// ExpandWith expands the macro with a list of actual arguments.
func (x *Macro) ExpandWith(interp *Interp, arg *Cell) any {
	frame := x.MakeFrame(arg, x)
	env := &Cell{frame, Nil}
	var y any = Nil
	for j := x.body; j != Nil; j = j.CdrCell() {
		y = interp.Eval(j.Car, env)
	}
	return y
}

// String returns a textual representation of the macro.
func (x *Macro) String() string {
	return fmt.Sprintf("#<macro:%d:%s>", x.Carity, Str(x.body))
}

// Externalize returns a readable external representation of the macro.
func (x *Macro) Externalize(interp *Interp, env *Cell) string {
	body := interp.Externalize(env, x.body)
	// remove outer parens
	if body[0] == '(' {
		body = body[1 : len(body)-1]
	}
	var args string
	if x.Args == Nil {
		args = "()"
	} else {
		args = interp.Externalize(env, x.Args)
	}
	return fmt.Sprintf("(macro %v %v)", args, body)
}

// Lambda represents a compiled lambda expression (within another function).
type Lambda struct {
	Func
	// Body is a list which will be used as the function body.
	Body *Cell
}

// NewLambda constructs a Lambda.
func NewLambda(carity int, args *Cell, body *Cell, env *Cell) any {
	return &Lambda{Func{carity, args}, body}
}

// String returns a textual representation of the lambda.
func (x *Lambda) String() string {
	return fmt.Sprintf("#<lambda:%d:%s>", x.Carity, Str(x.Body))
}

// Externalize returns a readable external representation of the lambda term.
func (x *Lambda) Externalize(interp *Interp, env *Cell) string {
	var args string
	if x.Args == Nil {
		args = "()"
	} else {
		args = interp.Externalize(env, x.Args)
	}
	body := interp.Externalize(env, x.Body)
	body = body[1 : len(body)-1] // remove outer parens
	return fmt.Sprintf("(lambda %v %v)", args, body)
}

// Closure represents a compiled lambda expression with its own environment.
type Closure struct {
	Lambda
	// Env is the closure's own environment.
	Env *Cell
}

// NewClosure constructs a Closure.
func NewClosure(carity int, args *Cell, body *Cell, env *Cell) any {
	return &Closure{Lambda{Func{carity, args}, body}, env}
}

// MakeEnv makes a new environment from a list of actual arguments,
// which will be used in evaluation of the body of the closure.
func (x *Closure) MakeEnv(interp *Interp, arg *Cell, interpEnv *Cell) *Cell {
	frame := x.MakeFrame(arg, x)
	x.EvalFrame(frame, interp, interpEnv)
	return &Cell{frame, x.Env} // Prepend the frame to Env of the closure.
}

// String returns a textual representation of the closure.
func (x *Closure) String() string {
	return fmt.Sprintf("#<closure:%d:%s:%s>",
		x.Carity, Str(x.Env), Str(x.Body))
}

// Externalize returns an external representation suitable for reading.
func (x *Closure) Externalize(interp *Interp, env *Cell) string {
	lam := interp.Externalize(x.Env, x.Body)
	lam = lam[1 : len(lam)-1] // remove outer parens
	var args string
	if x.Args == Nil {
		args = "()"
	} else {
		args = interp.Externalize(x.Env, x.Args)
	}
	if x.Env == Nil {
		return fmt.Sprintf("(lambda %v %v)", args, lam)
	}
	table := make(map[string]bool)
	ev := ListToArray(env)
	env2 := &Cell{ev, x.Env}
	s := interp.externalizeBindings(env, env2, x.Body, "", 0, table)
	return fmt.Sprintf("(letrec (%v) (lambda %v %v))", s, args, lam)
}

//----------------------------------------------------------------------

// BuiltInFunc represents a built-in function.
type BuiltInFunc struct {
	Func
	name string
	body func([]any) any
}

// NewBuiltInFunc constructs a BuiltInFunc.
func NewBuiltInFunc(name string, carity int,
	body func([]any) any) *BuiltInFunc {
	return &BuiltInFunc{Func{carity, nil}, name, body}
}

// EvalWith invokes the built-in function with a list of actual arguments.
func (x *BuiltInFunc) EvalWith(interp *Interp, arg *Cell,
	interpEnv *Cell) any {
	frame := x.MakeFrame(arg, x)
	x.EvalFrame(frame, interp, interpEnv)
	defer func() {
		if err := recover(); err != nil {
			if _, ok := err.(*EvalError); ok {
				panic(err)
			} else {
				msg := fmt.Sprintf("%v -- %s", err, x.name)
				panic(NewEvalError(msg, frame))
			}
		}
	}()
	return x.body(frame)
}

// String returns a textual representation of the BuiltInFunc.
func (x *BuiltInFunc) String() string {
	return fmt.Sprintf("#<%s:%d>", x.name, x.Carity)
}

// Externalize returns a textual representation suitable for reading.
func (x *BuiltInFunc) Externalize(interp *Interp, env *Cell) string {
	return fmt.Sprintf("(intrinsic (quote %v))", x.name)
}

//----------------------------------------------------------------------

// Arg represents a bound variable in a compiled lambda/macro expression.
// It is constructed with &Arg{level, offset, symbol}.
type Arg struct {
	// Level is a nesting level of the lexical scope.
	// 0 for the innermost scope.
	Level int

	// Offset is an offset of the variable within the frame of the Level.
	// 0 for the first variable within the frame.
	Offset int

	// Sym is a symbol which represented the variable before compilation.
	Symbol *Sym
}

// GetValue gets a value from the location corresponding to the variable x
// within an environment env.
func (x *Arg) GetValue(env *Cell) any {
	for i := 0; i < x.Level; i++ {
		env = env.Cdr.(*Cell)
	}
	return (env.Car.([]any))[x.Offset]
}

// GetValueSafe gets the value from the location corresponding to the variable x
// within an environment env and returns true, or it returns nil and false if the value
// cannot be found in env.
func (x *Arg) GetValueSafe(env *Cell) (any, bool) {
	n := env.Len()
	if n < x.Level {
		return nil, false
	}
	for i := 0; i < x.Level; i++ {
		env = env.Cdr.(*Cell)
	}
	arr := env.Car.([]any)
	if x.Offset >= len(arr) {
		return nil, false
	}
	return arr[x.Offset], true
}

// HasValue returns true if the env has the value of arg, false otherwise.
func (x *Arg) HasValue(env *Cell) bool {
	n := env.Len()
	if x.Level > n {
		return false
	}
	for i := 0; i < x.Level; i++ {
		env = env.Cdr.(*Cell)
	}
	return x.Offset < len(env.Car.([]any))
}

// SetValue sets a value y to the location corresponding to the variable x
// within an environment env.
func (x *Arg) SetValue(y any, env *Cell) {
	for i := 0; i < x.Level; i++ {
		env = env.Cdr.(*Cell)
	}
	(env.Car.([]any))[x.Offset] = y
}

// String returns a textual representation of the Arg.
func (x *Arg) String() string {
	return fmt.Sprintf("#%d:%d:%v", x.Level, x.Offset, x.Symbol)
}

// Externalize returns the external representation of the argument (assuming it's name
// has been resolved in higher environments).
func (x *Arg) Externalize(interp *Interp, env *Cell) string {
	return x.Symbol.Name
	// return x.String()
}

//----------------------------------------------------------------------

// EvalError represents an error in evaluation.
type EvalError struct {
	Message string
	Trace   []string
}

// NewEvalError constructs an EvalError.
func NewEvalError(msg string, x any) *EvalError {
	return &EvalError{msg + ": " + Str(x), nil}
}

// NewNotVariableError constructs an EvalError which indicates an absence
// of variable.
func NewNotVariableError(x any) *EvalError {
	return NewEvalError("variable expected", x)
}

// NewProtectedVariableError constructs an EvalError which indicates
// that the given variable is protected against global mutation.
func NewProtectedVariableError(x any) *EvalError {
	return NewEvalError("variable is protected", x)
}

// Error returns a textual representation of the error.
// It is defined in compliance with the error type.
func (err *EvalError) Error() string {
	s := "EvalError: " + err.Message
	for _, line := range err.Trace {
		s += "\n\t" + line
	}
	return s
}

// EofToken is a token which represents the end of file.
var EofToken error = io.EOF

// ValueHolder is a miscellaneous structure to hold interface{} values with
// symbols as keys. The additional indirection is tolerated to avoid having to
// use two map lookups, one for the value and one for the Protected feature.
// Performance: The performance impact should be analyzed.
type ValueHolder struct {
	Value     any
	Protected bool
}

//----------------------------------------------------------------------

// Interp represents a core of the interpreter.
type Interp struct {
	globals   sync.Map       // global symbol table
	lock      sync.RWMutex   // global lock, sometimes needed despite use of sync.Map
	inputLock sync.Mutex     // for input
	inputCond *sync.Cond     // condition to wait for when inputting
	pc        Runtime        // the Runtime support, used by z3s5 engine
	runtime   any            // the same as pc but as unrestricted interface, used by the client
	cancel    uint32         // 1 == cancel current evaluation
	streams   sync.Map       // a map containing any open streams for reading and writing
	prng      [10]*rand.Rand // 10 different PRNGs
}

// Future represents a "promise" for future/force.
type Future struct {
	// Chan is a channel which transmits a pair of result and error.
	// The pair is represented by Cell.
	Chan <-chan Cell

	// Result is a pair of the result (in a narrow meaning) and the error.
	Result Cell

	// Lock is an exclusive lock to receive the result at "force".
	Lock sync.Mutex
}

// String returns a textual representation of the Future.
func (fu *Future) String() string {
	return fmt.Sprintf("#<future:%v:%s:%v>",
		fu.Chan, Str(&fu.Result), &fu.Lock)
}

// Lock locks the interpreter's internal RWMutex. Be careful not to create deadlocks using this mechanism and bear
// in mind that some calls internally use this mutex.
func (interp *Interp) Lock() {
	interp.lock.Lock()
}

// Unlock unlocks the interpreter's internal RWMutex.
func (interp *Interp) Unlock() {
	interp.lock.Unlock()
}

// RLock read-locks the interpreter's internal RWMutex. Be careful not to create deadlocks using this mechanism and bear
// in mind that some calls internally use this mutex.
func (interp *Interp) RLock() {
	interp.lock.RLock()
}

// RUnlock unlocks a read-lock of the interpreter's internal RWMutex.
func (interp *Interp) RUnlock() {
	interp.lock.RUnlock()
}

// InputLock returns a pointer to a mutex used for locking on certain input conditions.
// It is recommended not to use this mutex unless you know what you're doing.
func (interp *Interp) InputLock() *sync.Mutex {
	return &interp.inputLock
}

// Break attempts to globally cancel a running process and restart the read-eval-print loop using StartREPL.
// It is like a soft reset function and would usually be bound to a function (break).
func (interp *Interp) Break() {
	atomic.StoreUint32(&interp.cancel, 1)
	interp.StartREPL()
}

// InputCond returns a pointer to an sync.Cond condition used for waiting for input. This is used
// for asynchronously signaling EndInput.
func (interp *Interp) InputCond() *sync.Cond {
	return interp.inputCond
}

// Streams returns a pointer to a sync.Map containing the i/o streams managed by the interpreter.
func (interp *Interp) Streams() *sync.Map {
	return &interp.streams
}

// GetGlobalVar gets a global value of symbol sym within the interpreter.
func (interp *Interp) GetGlobalVar(sym *Sym) (any, bool) {
	val, ok := interp.globals.Load(sym)
	if !ok {
		return nil, false
	}
	return val.(ValueHolder).Value, true
}

// SetGlobalVar sets a global value of symbol sym within the interpreter.
func (interp *Interp) SetGlobalVar(sym *Sym, val any) {
	v, ok := interp.globals.Load(sym)
	if ok {
		holder := v.(ValueHolder)
		if holder.Protected {
			fmt.Printf("security violation: attempt to mutate protected symbol '%v\n", Str(sym))
			return
		}
	}
	interp.globals.Store(sym, ValueHolder{Value: val, Protected: false})
}

// UnbindGlobalVar removes the value of symbol, making the symbol unbound (not defined).
func (interp *Interp) UnbindGlobalVar(sym *Sym) {
	interp.globals.Delete(sym)
}

// DefBoxed defines the standard functions for a boxed value.
func (interp *Interp) DefBoxed(sort *Sym) {
	interp.Def(sort.String()+"?", 1, func(a []any) any {
		if item, ok := a[0].(*Boxed); ok {
			if item.Sort == sort {
				return true
			}
			return Nil
		}
		return Nil
	})
}

var Number0 = goarith.AsNumber(0)
var Number1 = goarith.AsNumber(1)

// NewInterp constructs an interpreter and sets built-in functions etc. as
// the global values of symbols within the interpreter.
func NewInterp(pc any) (*Interp, error) {
	interp := Interp{}
	interp.inputCond = sync.NewCond(&interp.inputLock)
	interp.pc = pc.(Runtime)
	interp.runtime = pc
	for i := range interp.prng {
		interp.prng[i] = rand.New(rand.NewSource(int64(i + 5589)))
	}

	interp.SetGlobalVar(ReflectSym, Nil) // initially *reflect* is empty, each of the definitions below populate it

	interp.Define_Base()       // base definitions
	interp.Define_Console()    // line-based console i/o
	interp.Define_Float()      // floating point package with prefix fl
	interp.Define_Ling()       // linguistic helpers such as Levenshtein distance
	interp.Define_Decimal()    // decimal arithmetics
	interp.Define_StyledText() // colors in terminal
	interp.Define_FileIO()     // file i/o with build tag "fileio"
	interp.Define_DB()         // sqlite interface with build tag "db"

	if supportsSound {
		reflect, ok := interp.GetGlobalVar(ReflectSym)
		if !ok {
			reflect = Nil
		}
		interp.SetGlobalVar(ReflectSym, &Cell{NewSym("beep"), reflect})
	}

	return &interp, nil
}

// Runtime returns the runtime support system provided when a new interpreter was created, as an
// unrestricted any interface, so the client can type cast it to whatever runtime structure they use.
func (interp *Interp) Runtime() any {
	return interp.runtime
}

// ExpectInts returns a slice of n ints decoded from a[offset], or panics with an error.
func (interp *Interp) ExpectInts(caller string, a []any, offset, n int) []int {
	if offset+n-1 > len(a)-1 {
		panic(fmt.Sprintf(`%s: syntax error, not enough arguments, expected %d integers`, caller, n))
	}
	ints := make([]int, n)
	c := 0
	for i := offset; i < offset+n; i++ {
		n, exact := goarith.AsNumber(a[i]).Int()
		if !exact {
			panic(fmt.Sprintf(`%s: type error, argument %d must be an integer`, caller, i))
		}
		ints[c] = n
		c++
	}
	return ints
}

// MustParseBlobArgs parses the Lisp arguments for a blob and optional lower and upper bound.
// If the offset and upper bound are not provided, 0 and the length of the blob are used.
func (interp *Interp) MustParseBlobArgs(caller string, a []any) ([]byte, int, int) {
	li := a[0].(*Cell)
	blob := MustGetBoxed(caller, li.Car, BoxedBlob)
	b := blob.Datum.([]byte)
	var n, m int
	if li.CdrCell() != Nil {
		arr := ListToArray(li.CdrCell())
		if len(arr) > 1 {
			narr := interp.ExpectInts(caller, arr, 0, 2)
			n = narr[0]
			m = narr[1]
		} else {
			narr := interp.ExpectInts(caller, arr, 0, 1)
			n = narr[0]
			m = len(b)
		}
	} else {
		n = 0
		m = len(b)
	}
	return b, n, m
}

// equal checks for equality recursively
func (interp *Interp) equal(a, b any) bool {
	// array
	if _, ok := a.([]any); ok {
		if _, ok := b.([]any); !ok {
			return false
		}
		a1 := a.([]any)
		a2 := b.([]any)
		if len(a1) != len(a2) {
			return false
		}
		for i := range a1 {
			if !interp.equal(a1[i], a2[i]) {
				return false
			}
		}
		return true
	}
	// map (dict)
	if m1, ok := a.(map[any]any); ok {
		if m2, ok := b.(map[any]any); ok {
			if len(m1) != len(m2) {
				return false
			}
			for k, v := range m1 {
				if !interp.equal(v, m2[k]) {
					return false
				}
			}
			return true
		} else {
			return false
		}
	}
	// *Boxed value (opaque, non-writable with custom type)
	if b1, ok := a.(*Boxed); ok {
		if b2, ok := b.(*Boxed); ok {
			if b1.Sort == b2.Sort {
				if b1.Equal != nil {
					return b1.Equal(b2)
				}
				return b1.Datum == b2.Datum
			}
		} else {
			return false
		}
	}
	// other values except numbers
	if a == b {
		return true
	}
	// check number equality
	if x := goarith.AsNumber(a); x != nil {
		if y := goarith.AsNumber(b); y != nil {
			if x.Cmp(y) == 0 {
				return true
			}
		}
	}
	return false
}

// Def defines a built-in function by giving a name, arity, and body.
func (interp *Interp) Def(name string, carity int,
	body func([]any) any) {
	sym := NewSym(name)
	fnc := NewBuiltInFunc(name, carity, body)
	interp.SetGlobalVar(sym, fnc)
}

// Eval evaluates a Lisp expression in a given environment env.
func (interp *Interp) Eval(expression any, env *Cell) any {
	defer func() {
		if err := recover(); err != nil {
			if ex, ok := err.(*EvalError); ok {
				if ex.Trace == nil {
					ex.Trace = make([]string, 0, 10)
				}
				if len(ex.Trace) < 10 {
					ex.Trace = append(ex.Trace, Str(expression))
				}
			}
			panic(err)
		}
	}()
	for {
		// global hardcore break, produces errors
		if atomic.LoadUint32(&interp.cancel) > 0 {
			atomic.StoreUint32(&interp.cancel, 0)
			return nil
		}
		switch x := expression.(type) {
		case *Arg:
			return x.GetValue(env)
		case *Sym:
			r, ok := interp.GetGlobalVar(x)
			if ok {
				return r
			}
			panic(NewEvalError("void variable", x))
		case *Cell:
			if x == Nil {
				return x // an empty list
			}
			fn := x.Car
			arg := x.CdrCell()
			sym, ok := fn.(*Sym)
			if ok && sym.IsKeyword {
				switch sym {
				case QuoteSym:
					if arg != Nil && arg.Cdr == Nil {
						return arg.Car
					}
					panic(NewEvalError("bad quote", x))
				case ProgNSym:
					expression = interp.evalProgN(arg, env)
				case CondSym:
					expression = interp.evalCond(arg, env)
				case SetqSym:
					return interp.evalSetQ(arg, env)
				case LambdaSym:
					return interp.compile(arg, env, NewClosure)
				case MacroSym:
					if env != Nil {
						panic(NewEvalError("nested macro", x))
					}
					return interp.compile(arg, Nil, NewMacro)
				case QuasiquoteSym:
					if arg != Nil && arg.Cdr == Nil {
						expression = QqExpand(arg.Car)
					} else {
						panic(NewEvalError("bad quasiquote", x))
					}
				case FutureSym:
					ch := make(chan Cell)
					go interp.futureTask(arg, env, ch)
					return &Future{Chan: ch}
				case TaskSym:
					go interp.normalTask(arg, env)
					return Void
				default:
					panic(NewEvalError("bad keyword", fn))
				}
			} else { // Apply fn to arg.
				// Expand fn = interp.Eval(fn, env) here on Sym for speed.
				if ok {
					fn, ok = interp.GetGlobalVar(sym)
					if !ok {
						panic(NewEvalError("undefined", x.Car))
					}
				} else {
					fn = interp.Eval(fn, env)
				}
				switch f := fn.(type) {
				case *Closure:
					env = f.MakeEnv(interp, arg, env)
					expression = interp.evalProgN(f.Body, env)
				case *Macro:
					expression = f.ExpandWith(interp, arg)
				case *BuiltInFunc:
					return f.EvalWith(interp, arg, env)
				default:
					panic(NewEvalError("not applicable", fn))
				}
			}
		case *Lambda:
			return &Closure{*x, env}
		default:
			return x // numbers, strings etc.
		}
	}
}

// SafeEval evaluates a Lisp expression in a given environment env and
// returns the result and nil.
// If an error happens, it returns Nil and the error
func (interp *Interp) SafeEval(expression any, env *Cell) (
	result any, err any) {
	defer func() {
		if e := recover(); e != nil {
			var ok bool
			result, ok = interp.HandleError(e)
			if !ok {
				result, err = Nil, e
			}
		}
	}()
	return interp.Eval(expression, env), nil
}

// evalProgN evaluates E1, E2, .., E(n-1) and returns the tail expression En.
func (interp *Interp) evalProgN(j *Cell, env *Cell) any {
	if j == Nil {
		return Nil
	}
	for {
		x := j.Car
		j = j.CdrCell()
		if j == Nil {
			return x // The tail expression will be evaluated at the caller.
		}
		interp.Eval(x, env)
	}
}

// futureTask is a task for goroutine to deliver the "promise" of Future.
// It returns the En value of (future E1 E2 .. En) via the channel and
// closes the channel.
func (interp *Interp) futureTask(j *Cell, env *Cell, ch chan<- Cell) {
	defer close(ch)
	result, err := interp.safeProgN(j, env)
	ch <- Cell{result, err}
}

// normalTask is a normal task for goroutine that is not forced. It handles errors
// by calling the error handler, which uses the goroutine ID hack. The evaluation
// result is ignored.
func (interp *Interp) normalTask(j *Cell, env *Cell) {
	defer func() {
		e := recover()
		if e != nil {
			interp.HandleError(e)
		}
	}()
	x := interp.evalProgN(j, env)
	interp.Eval(x, env)
}

// safeProgN evaluates E1, E2, .. En and returns the value of En and nil.
// If an error happens, it returns Nil and the error.
func (interp *Interp) safeProgN(j *Cell, env *Cell) (result any,
	err any) {
	defer func() {
		if e := recover(); e != nil {
			result, err = Nil, e
		}
	}()
	x := interp.evalProgN(j, env)
	return interp.Eval(x, env), nil
}

// evalCond evaluates a conditional expression and returns the selection
// unevaluated.
func (interp *Interp) evalCond(j *Cell, env *Cell) any {
	for j != Nil {
		clause, ok := j.Car.(*Cell)
		if ok {
			if clause != Nil {
				result := interp.Eval(clause.Car, env)
				if result != Nil { // If the condition holds...
					body := clause.CdrCell()
					if body == Nil {
						return QqQuote(result)
					} else {
						return interp.evalProgN(body, env)
					}
				}
			}
		} else {
			panic(NewEvalError("cond test expected", j.Car))
		}
		j = j.CdrCell()
	}
	return Nil // No clause holds.
}

// evalSeqQ evaluates each Ei of (setq .. Vi Ei ..) and assigns it to Vi
// repectively.  It returns the value of the last expression En.
func (interp *Interp) evalSetQ(j *Cell, env *Cell) any {
	var result any = Nil
	for j != Nil {
		lval := j.Car
		j = j.CdrCell()
		if j == Nil {
			panic(NewEvalError("right value expected", lval))
		}
		result = interp.Eval(j.Car, env)
		switch v := lval.(type) {
		case *Arg:
			v.SetValue(result, env)
		case *Sym:
			if v.IsKeyword {
				panic(NewNotVariableError(lval))
			}
			interp.SetGlobalVar(v, result)
		default:
			panic(NewNotVariableError(lval))
		}
		j = j.CdrCell()
	}
	return result
}

// compile compiles a Lisp list (macro ...) or (lambda ...).
func (interp *Interp) compile(arg *Cell, env *Cell,
	factory func(int, *Cell, *Cell, *Cell) any) any {
	if arg == Nil {
		panic(NewEvalError("arglist and body expected", arg))
	}
	table := make(map[*Sym]*Arg)
	hasRest := makeArgTable(arg.Car, table)
	arity := len(table)
	body := arg.CdrCell()
	body = scanForArgs(body, table).(*Cell)
	body = interp.ExpandMacros(body, 200).(*Cell) // Expand up to 200 nestings.
	body = interp.compileInners(body).(*Cell)
	if hasRest {
		arity = -arity
	}
	return factory(arity, arg.Car.(*Cell), body, env)
}

// ExpandMacros expands macros and quasi-quotes in x up to count nestings.
func (interp *Interp) ExpandMacros(x any, count int) any {
	if count > 0 {
		if j, ok := x.(*Cell); ok {
			if j == Nil {
				return Nil
			}
			switch k := j.Car; k {
			case QuoteSym, LambdaSym, MacroSym:
				return j
			case QuasiquoteSym:
				d := j.CdrCell()
				if d != Nil && d.Cdr == Nil {
					z := QqExpand(d.Car)
					return interp.ExpandMacros(z, count)
				}
				panic(NewEvalError("bad quasiquote", j))
			default:
				if sym, ok := k.(*Sym); ok {
					if v, ok := interp.GetGlobalVar(sym); ok {
						k = v
					}
				}
				if f, ok := k.(*Macro); ok {
					d := j.CdrCell()
					z := f.ExpandWith(interp, d)
					return interp.ExpandMacros(z, count-1)
				} else {
					return j.MapCar(func(y any) any {
						return interp.ExpandMacros(y, count)
					})
				}
			}
		}
	}
	return x
}

// compileInners replaces inner lambda-expressions with Lambda instances.
func (interp *Interp) compileInners(x any) any {
	if j, ok := x.(*Cell); ok {
		if j == Nil {
			return Nil
		}
		switch k := j.Car; k {
		case QuoteSym:
			return j
		case LambdaSym:
			d := j.CdrCell()
			return interp.compile(d, Nil, NewLambda)
		case MacroSym:
			panic(NewEvalError("nested macro", j))
		default:
			return j.MapCar(func(y any) any {
				return interp.compileInners(y)
			})
		}
	}
	return x
}

//----------------------------------------------------------------------

// makeArgTable makes an argument-table.  It returns true if x has &rest.
func makeArgTable(x any, table map[*Sym]*Arg) bool {
	arg, ok := x.(*Cell)
	if !ok {
		panic(NewEvalError("arglist expected", x))
	}
	if arg == Nil {
		return false
	} else {
		offset := 0 // offset value within the call-frame
		hasRest := false
		for arg != Nil {
			j := arg.Car
			if hasRest {
				panic(NewEvalError("2nd rest", j))
			}
			if j == RestSym { // &rest var
				arg = arg.CdrCell()
				if arg == Nil {
					panic(NewNotVariableError(arg))
				}
				j = arg.Car
				if j == RestSym {
					panic(NewNotVariableError(j))
				}
				hasRest = true
			}
			var sym *Sym
			switch v := j.(type) {
			case *Sym:
				sym = v
			case *Arg:
				sym = v.Symbol
			default:
				panic(NewNotVariableError(j))
			}
			if _, ok := table[sym]; ok {
				panic(NewEvalError("duplicated argument name", sym))
			}
			table[sym] = &Arg{0, offset, sym}
			offset++
			arg = arg.CdrCell()
		}
		return hasRest
	}
}

// scanForArgs scans x for formal arguments in table and replaces them
// with Args.
// Also it scans x for free Args not in table and promotes their levels.
func scanForArgs(x any, table map[*Sym]*Arg) any {
	switch j := x.(type) {
	case *Sym:
		if a, ok := table[j]; ok {
			return a
		}
		return j
	case *Arg:
		if a, ok := table[j.Symbol]; ok {
			return a
		}
		return &Arg{j.Level + 1, j.Offset, j.Symbol}
	case *Cell:
		if j == Nil {
			return Nil
		}
		switch j.Car {
		case QuoteSym:
			return j
		case QuasiquoteSym:
			return &Cell{QuasiquoteSym, scanForQQ(j.Cdr, table, 0)}
		default:
			return j.MapCar(func(y any) any {
				return scanForArgs(y, table)
			})
		}
	default:
		return j
	}
}

// scanForQQ scans x for quasi-quotes and executes scanForArgs on the
// nesting level of quotes.
func scanForQQ(x any, table map[*Sym]*Arg,
	level int) any {
	j, ok := x.(*Cell)
	if ok {
		if j == Nil {
			return Nil
		}
		switch k := j.Car; k {
		case QuasiquoteSym:
			return &Cell{k, scanForQQ(j.Cdr, table, level+1)}
		case UnquoteSym, UnquoteSplicingSym:
			var d any
			if level == 0 {
				d = scanForArgs(j.Cdr, table)
			} else {
				d = scanForQQ(j.Cdr, table, level-1)
			}
			if d == j.Cdr {
				return j
			}
			return &Cell{k, d}
		default:
			return j.MapCar(func(y any) any {
				return scanForQQ(y, table, level)
			})
		}
	} else {
		return x
	}
}

//----------------------------------------------------------------------
// Quasi-Quotation

// QqExpand expands x of any quasi-quote `x into the equivalent S-expression.
func QqExpand(x any) any {
	return qqExpand0(x, 0) // Begin with the nesting level 0.
}

// QqQuote quotes x so that the result evaluates to x.
func QqQuote(x any) any {
	if x == Nil {
		return Nil
	}
	switch x.(type) {
	case *Sym, *Cell:
		return &Cell{QuoteSym, &Cell{x, Nil}}
	default:
		return x
	}
}

func qqExpand0(x any, level int) any {
	if j, ok := x.(*Cell); ok {
		if j == Nil {
			return Nil
		}
		if j.Car == UnquoteSym { // ,a
			if level == 0 {
				return j.CdrCell().Car // ,a => a
			}
		}
		t := qqExpand1(j, level)
		if t.Cdr == Nil {
			if k, ok := t.Car.(*Cell); ok {
				if k.Car == ListSym || k.Car == ConsSym {
					return k
				}
			}
		}
		return &Cell{AppendSym, t}
	} else {
		return QqQuote(x)
	}
}

// qqExpand1 expands x of `x so that the result can be used as an argument of
// append.  Example 1: (,a b) => ((list a 'b))
//          Example 2: (,a ,@(cons 2 3)) => ((cons a (cons 2 3)))
func qqExpand1(x any, level int) *Cell {
	if j, ok := x.(*Cell); ok {
		if j == Nil {
			return &Cell{Nil, Nil}
		}
		switch j.Car {
		case UnquoteSym: // ,a
			if level == 0 {
				return j.CdrCell() // ,a => (a)
			}
			level--
		case QuasiquoteSym: // `a
			level++
		}
		h := qqExpand2(j.Car, level)
		t := qqExpand1(j.Cdr, level) // != Nil
		if t.Car == Nil && t.Cdr == Nil {
			return &Cell{h, Nil}
		} else if hc, ok := h.(*Cell); ok {
			if hc.Car == ListSym {
				if tcar, ok := t.Car.(*Cell); ok {
					if tcar.Car == ListSym {
						hh := qqConcat(hc, tcar.Cdr)
						return &Cell{hh, t.Cdr}
					}
				}
				if hcdr, ok := hc.Cdr.(*Cell); ok {
					hh := qqConsCons(hcdr, t.Car)
					return &Cell{hh, t.Cdr}
				}
			}
		}
		return &Cell{h, t}
	} else {
		return &Cell{QqQuote(x), Nil}
	}
}

// (1 2), (3 4) => (1 2 3 4)
func qqConcat(x *Cell, y any) any {
	if x == Nil {
		return y
	} else {
		return &Cell{x.Car, qqConcat(x.CdrCell(), y)}
	}
}

// (1 2 3), "a" => (cons 1 (cons 2 (cons 3 "a")))
func qqConsCons(x *Cell, y any) any {
	if x == Nil {
		return y
	} else {
		return &Cell{ConsSym, &Cell{x.Car,
			&Cell{qqConsCons(x.CdrCell(), y), Nil}}}
	}
}

// qqExpand2 expands x.car (= y) of `x so that the result can be used as an
// argument of append.
// Examples: ,a => (list a); ,@(foo 1 2) => (foo 1 2); b => (list 'b)
func qqExpand2(y any, level int) any {
	if j, ok := y.(*Cell); ok {
		if j == Nil {
			return &Cell{ListSym, &Cell{Nil, Nil}} // (list nil)
		}
		switch j.Car {
		case UnquoteSym: // ,a
			if level == 0 {
				return &Cell{ListSym, j.Cdr} // ,a => (list a)
			}
			level--
		case UnquoteSplicingSym: // ,@a
			if level == 0 {
				return j.CdrCell().Car // ,@a => a
			}
			level--
		case QuasiquoteSym: // `a
			level++
		}
	}
	return &Cell{ListSym, &Cell{qqExpand0(y, level), Nil}}
}

//----------------------------------------------------------------------

// Reader represents a reader of Lisp expressions.
type Reader struct {
	scanner *bufio.Scanner
	reader  io.Reader
	token   any      // the current token
	tokens  []string // tokens read from the current line
	index   int      // the next index of tokens
	line    string   // the current line
	lineNo  int      // the current line number
	erred   bool     // a flag if an error has happened
}

// NewReader constructs a reader which will read Lisp expressions from r.
func NewReader(r io.Reader) *Reader {
	scanner := bufio.NewScanner(r)
	buff := make([]byte, DEFAULT_BUFFSIZE)
	scanner.Buffer(buff, DEFAULT_BUFFMAX)
	return &Reader{scanner, r, nil, nil, 0, "", 0, false}
}

// IOReader returns the underlying Go io.Reader for direct use.
// This is used by binary operations that don't use the *bufio.scanner.
func (rr *Reader) IOReader() io.Reader {
	return rr.reader
}

// Read reads a Lisp expression and returns the expression and nil.
// If the input runs out, it returns EofToken and nil.
// If an error happens, it returns Nil and the error.
func (rr *Reader) Read() (result any, err any) {
	defer func() {
		if e := recover(); e != nil {
			result, err = Nil, e
		}
	}()
	rr.readToken()
	return rr.parseExpression(), nil
}

func (rr *Reader) newSynatxError(msg string, arg any) *EvalError {
	rr.erred = true
	s := fmt.Sprintf("syntax error: %s -- %d: %s",
		fmt.Sprintf(msg, arg), rr.lineNo, rr.line)
	return &EvalError{s, nil}
}

func (rr *Reader) parseExpression() any {
	switch rr.token {
	case LeftParenSym: // (a b c)
		rr.readToken()
		return rr.parseListBody()
	case SingleQuoteSym: // 'a => (quote a)
		rr.readToken()
		return &Cell{QuoteSym, &Cell{rr.parseExpression(), Nil}}
	case BackQuoteSym: // `a => (quasiquote a)
		rr.readToken()
		return &Cell{QuasiquoteSym, &Cell{rr.parseExpression(), Nil}}
	case CommaSym: // ,a => (unquote a)
		rr.readToken()
		return &Cell{UnquoteSym, &Cell{rr.parseExpression(), Nil}}
	case CommaAtSym: // ,@a => (unquote-splicing a)
		rr.readToken()
		return &Cell{UnquoteSplicingSym, &Cell{rr.parseExpression(), Nil}}
	case HashSym:
		rr.readToken()
		return ListToArray(rr.parseExpression().(*Cell))
	case DotSym, RightParenSym:
		panic(rr.newSynatxError("unexpected \"%v\"", rr.token))
	default:
		return rr.token
	}
}

func (rr *Reader) parseListBody() *Cell {
	if rr.token == EofToken {
		panic(rr.newSynatxError("unexpected EOF%s", ""))
	} else if rr.token == RightParenSym {
		return Nil
	} else {
		e1 := rr.parseExpression()
		rr.readToken()
		var e2 any
		if rr.token == DotSym { // (a . b)
			rr.readToken()
			e2 = rr.parseExpression()
			rr.readToken()
			if rr.token != RightParenSym {
				panic(rr.newSynatxError("\")\" expected: %v", rr.token))
			}
		} else {
			e2 = rr.parseListBody()
		}
		return &Cell{e1, e2}
	}
}

// readToken reads the next token and set it to rr.token.
func (rr *Reader) readToken() {
	// Read the next line if the line ends or an error happened last time.
	for len(rr.tokens) <= rr.index || rr.erred {
		rr.erred = false
		if rr.scanner.Scan() {
			rr.line = rr.scanner.Text()
			rr.lineNo++
		} else {
			if err := rr.scanner.Err(); err != nil {
				panic(err)
			}
			rr.token = EofToken
			return
		}
		mm := tokenPat.FindAllStringSubmatch(rr.line, -1)
		tt := make([]string, 0, len(mm)*3/5) // Estimate 40% will be spaces.
		for _, m := range mm {
			if m[1] != "" {
				tt = append(tt, m[1])
			}
		}
		rr.tokens = tt
		rr.index = 0
	}
	// Read the next token.
	s := rr.tokens[rr.index]
	rr.index++
	if s[0] == '"' {
		n := len(s) - 1
		if n < 1 || s[n] != '"' {
			panic(rr.newSynatxError("bad string: '%s'", s))
		}
		s = s[1:n]
		s = escapePat.ReplaceAllStringFunc(s, func(t string) string {
			r, ok := escapes[t] // r, err := strconv.Unquote("'" + t + "'")
			if !ok {
				r = t // Leave any invalid escape sequence as it is.
			}
			return r
		})
		rr.token = s
		return
	}
	if f, ok := tryToReadNumber(s); ok {
		rr.token = f
		return
	}
	if s == "nil" {
		rr.token = Nil
		return
	} else if s == "t" {
		rr.token = true
		return
	}
	rr.token = NewSym(s)
}

func tryToReadNumber(s string) (goarith.Number, bool) {
	z := new(big.Int)
	if _, ok := z.SetString(s, 0); ok {
		return goarith.AsNumber(z), true
	}
	if f, err := strconv.ParseFloat(s, 64); err == nil {
		return goarith.AsNumber(f), true
	}
	return nil, false
}

// tokenPat is a regular expression to split a line to Lisp tokens.
var tokenPat = regexp.MustCompile(`\s+|;.*$|("(\\.?|.)*?"|,@?|[^()'` +
	"`" + `~"; \t]+|.)`)

// escapePat is a reg. expression to take an escape sequence out of a string.
var escapePat = regexp.MustCompile(`\\(.)`)

// escapes is a mapping from an escape sequence to its string value.
var escapes = map[string]string{
	`\\`: `\`,
	`\"`: `"`,
	`\n`: "\n", `\r`: "\r", `\f`: "\f", `\b`: "\b", `\t`: "\t", `\v`: "\v",
}

//----------------------------------------------------------------------

// Str returns a textual representation of any Lisp expression x.
func Str(x any) string {
	return Str2(x, true)
}

// Str2 returns a textual representation of any Lisp expression x.
// If quoteString is true, any strings in the expression are represented
// with enclosing quotes respectively.
func Str2(x any, quoteString bool) string {
	return str4(x, quoteString, -1, nil)
}

// quotes is a mapping from a quote symbol to its string representation.
var quotes = map[*Sym]string{
	QuoteSym:           "'",
	QuasiquoteSym:      "`",
	UnquoteSym:         ",",
	UnquoteSplicingSym: ",@",
}

func str4(a any, quoteString bool, count int,
	printed map[*Cell]bool) string {
	if a == true {
		return "t"
	}
	switch x := a.(type) {
	case *Cell:
		if x == Nil {
			return "nil"
		}
		if s, ok := x.Car.(*Sym); ok {
			if q, ok := quotes[s]; ok {
				if d, ok := x.Cdr.(*Cell); ok && d != nil {
					if d.Cdr == Nil {
						return q + str4(d.Car, true, count, printed)
					}
				}
			}
		}
		return "(" + strListBody(x, count, printed) + ")"
	case string:
		if quoteString {
			return strconv.Quote(x)
		}
		return x
	case []any:
		s := make([]string, len(x))
		for i, e := range x {
			s[i] = str4(e, true, count, printed)
		}
		return "#(" + strings.Join(s, " ") + ")"
	case *Sym:
		if x.IsInterned() {
			return x.Name
		}
		return "#:" + x.Name
	case *sync.RWMutex:
		return fmt.Sprintf("#<mutex: %p>", a)
	case *Dict:
		return fmt.Sprintf("#<dict: %p>", a)
	case *Boxed:
		return a.(*Boxed).Print()
	}
	return fmt.Sprintf("%v", a)
}

// strListBody makes a string representation of a list, omitting its parens.
func strListBody(x *Cell, count int, printed map[*Cell]bool) string {
	if printed == nil {
		printed = make(map[*Cell]bool)
	}
	if count < 0 {
		count = 4 // threshold of ellipsis for circular lists
	}
	s := make([]string, 0, 10)
	y := x
	for y != Nil {
		if _, ok := printed[y]; ok {
			count--
			if count < 0 {
				s = append(s, "...") // ellipsis for a circular list
				return strings.Join(s, " ")
			}
		} else {
			printed[y] = true
			count = 4
		}
		s = append(s, str4(y.Car, true, count, printed))
		if cdr, ok := y.Cdr.(*Cell); ok {
			y = cdr
		} else {
			s = append(s, ".")
			s = append(s, str4(y.Cdr, true, count, printed))
			break
		}
	}
	y = x
	for y != Nil {
		delete(printed, y)
		if cdr, ok := y.Cdr.(*Cell); ok {
			y = cdr
		} else {
			break
		}
	}
	return strings.Join(s, " ")
}

// Externalize bindEnv as bindings of the form "((arg1 value) (arg2 value))", given the interpretative
// environment interpEnv
// lookupArgs recursively looks up args in the list and adds them to string
func (interp *Interp) externalizeBindings(interpEnv, env, body *Cell, s string, level int,
	table map[string]bool) string {
	arr := ListToArray(body)
	for _, a := range arr {
		switch a.(type) {
		case *Arg:
			arg := a.(*Arg)
			if _, ok := table[arg.Symbol.Name]; !ok {
				v, ok := arg.GetValueSafe(env)
				if ok {
					s = s + "(" + arg.Symbol.Name + " " + interp.Externalize(interpEnv, v) + ")"
					table[arg.Symbol.Name] = true
				}
			}
		case *Cell:
			s = s + interp.externalizeBindings(interpEnv, env, a.(*Cell), "", level+1, table)
		}
	}
	return s
}

// Return a representation of a that is suitable for reading.
// Like Str and derived directly from it - see externalize5 and externalizeListBody -, but
// this procedure creates externalizable representations for all arguments.
// Changes must be reflected in CanExternalize.
func (interp *Interp) Externalize(env *Cell, a any) string {
	return interp.externalize5(env, a, true, -1, nil)
}

func (interp *Interp) externalize5(env *Cell, a any, quoteString bool, count int,
	printed map[*Cell]bool) string {
	if a == true {
		return "t"
	}
	switch x := a.(type) {
	case *Cell:
		if x == Nil {
			return "nil"
		}
		if s, ok := x.Car.(*Sym); ok {
			if q, ok := quotes[s]; ok {
				if d, ok := x.Cdr.(*Cell); ok {
					if d.Cdr == Nil {
						return q + interp.externalize5(env, d.Car, true, count, printed)
					}
				}
			}
		}
		return "(" + interp.externalizeListBody(env, x, count, printed) + ")"
	case string:
		if quoteString {
			return strconv.Quote(x)
		}
		return x
	case []any:
		s := make([]string, len(x))
		for i, e := range x {
			s[i] = interp.externalize5(env, e, true, count, printed)
		}
		return "#(" + strings.Join(s, " ") + ")"
	case *Sym:
		return x.Name
	case *Dict:
		arr := DictToArray(a)
		li := ArrayToList(arr)
		return "(dict '" + interp.Externalize(env, li) + ")"
	case *sync.RWMutex:
		return "(make-mutex)"
	case goarith.Number:
		return fmt.Sprintf("%v", a)
	case Externalizable:
		return x.Externalize(interp, env)
	}
	panic(fmt.Sprintf("externalize: value is not externalizable: %v", a))
}

// CanExternalize returns true if the value in a can be externalized, false otherwise.
// Changes must be reflected in Externalize.
func (interp *Interp) CanExternalize(a any) bool {
	if a == true {
		return true
	}
	switch a.(type) {
	case *Cell, string, []any, *Sym, *Dict, *sync.RWMutex, goarith.Number, Externalizable:
		return true
	default:
		return false
	}
}

// strListBody makes a string representation of a list, omitting its parens.
func (interp *Interp) externalizeListBody(env *Cell, x *Cell, count int, printed map[*Cell]bool) string {
	if printed == nil {
		printed = make(map[*Cell]bool)
	}
	if count < 0 {
		count = 4 // threshold of ellipsis for circular lists
	}
	s := make([]string, 0, 10)
	y := x
	for y != Nil {
		if _, ok := printed[y]; ok {
			count--
			if count < 0 {
				s = append(s, "...") // ellipsis for a circular list
				return strings.Join(s, " ")
			}
		} else {
			printed[y] = true
			count = 4
		}
		s = append(s, interp.externalize5(env, y.Car, true, count, printed))
		if cdr, ok := y.Cdr.(*Cell); ok {
			y = cdr
		} else {
			s = append(s, ".")
			s = append(s, interp.externalize5(env, y.Cdr, true, count, printed))
			break
		}
	}
	y = x
	for y != Nil {
		delete(printed, y)
		if cdr, ok := y.Cdr.(*Cell); ok {
			y = cdr
		} else {
			break
		}
	}
	return strings.Join(s, " ")
}

//----------------------------------------------------------------------

// Writer is the counterpart to Reader for writing lisp expressions
// to streams.
type Writer struct {
	iowriter io.Writer
}

// NewWriter returns a new lisp writer.
func NewWriter(writer io.Writer) *Writer {
	return &Writer{iowriter: writer}
}

// Write a lisp expression to this stream.
func (w *Writer) Write(x any) (int, error) {
	return w.iowriter.Write([]byte(Str(x)))
}

// IOWriter retrieves the underlying io.Writer from Go for direct use.
func (w *Writer) IOWriter() io.Writer {
	return w.iowriter
}

// Seeker is a minor abstraction wrapper for io.Seeker.
type Seeker struct {
	ioseeker io.Seeker
}

// NewSeeker returns a new Seeker based on given io.Seeker.
func NewSeeker(seeker io.Seeker) *Seeker {
	return &Seeker{ioseeker: seeker}
}

// IOSeeker returns the underlying io.Seeker for the seekable port.
func (s *Seeker) IOSeeker() io.Seeker {
	return s.ioseeker
}

// WriterAt is a writer that can write to particular positions.
type WriterAt struct {
	iowriterat io.WriterAt
}

// NewWriterAt creates a new WriterAt for writing to particular positions.
func NewWriterAt(writer io.WriterAt) *WriterAt {
	return &WriterAt{iowriterat: writer}
}

// IOWriterAt returns the wrapped io.writerat in this stream.
func (w *WriterAt) IOWriterAt() io.WriterAt {
	return w.iowriterat
}

// BufferedReader is a buffered reader.
type BufferedReader struct {
	buffioreader *bufio.Reader
}

// NewBufferedReader returns a buffered reader based on an io.Reader.
func NewBufferedReader(reader io.Reader) *BufferedReader {
	return &BufferedReader{buffioreader: bufio.NewReader(reader)}
}

// BuffIOReader returns the bufio.Reader encapsulated in the BufferedReader.
func (r *BufferedReader) BuffIOReader() *bufio.Reader {
	return r.buffioreader
}

// Stream is a Lisp wrapper for a lisp stream, reader and writer.
// It is used in combination with the FileManager to manage i/o streams.
type Stream struct {
	LispReader   *Reader
	LispWriter   *Writer
	LispSeeker   *Seeker
	LispWriterAt *WriterAt
	BuffReader   *BufferedReader
}

func NewStream(ioreader io.Reader, iowriter io.Writer, ioseeker io.Seeker, iowriterat io.WriterAt) *Stream {
	return &Stream{LispReader: NewReader(ioreader), LispWriter: NewWriter(iowriter),
		LispSeeker: NewSeeker(ioseeker), LispWriterAt: NewWriterAt(iowriterat), BuffReader: NewBufferedReader(ioreader)}
}

// Boot loads the standard prelude and any other init files and ensures
// that the interpreter is in a ready state.
func (interp *Interp) Boot() error {
	p := interp.pc.Perm()
	if p.LoadPrelude {
		ss := strings.NewReader(Prelude)
		if !interp.Run(ss) {
			return errors.New(`Z3S5 Lisp standard prelude boot sequence failed`)
		}
		preamble := bytes.NewReader(initFile)
		if !interp.Run(preamble) {
			return errors.New(`Z3S5 Lisp preamble failed`)
		}
		help := bytes.NewReader(helpFile)
		if !interp.Run(help) {
			return errors.New(`Z3S5 Lisp help definitions failed`)
		}
		version := bytes.NewReader(versionFile)
		if !interp.Run(version) {
			return errors.New(`Z3S5 Lisp could not read the version information`)
		}
	}
	if p.LoadUserInit {
		file, err := os.Open(`init.lisp`)
		if err != nil {
			if !os.IsNotExist(err) {
				return fmt.Errorf("Z3S5 Lisp could not open the init.lisp file: %w", err)
			}
			return nil
		}
		defer file.Close()
		if !interp.Run(file) {
			return errors.New(`Z3S5 Lisp encountered an error in init.lisp.`)
		}
	}
	return nil
}

// SetInteractive sets the Lisp *interactive-session* global variable to indicate a session is interactive or not.
func (interp *Interp) SetInteractive(on bool) {
	interp.SetGlobalVar(NewSym("*interactive-session*"), AsLispBool(on))
}

// StartREPL marks the start of the line input in the current editor. The line can then be
// retrieved with EndLineInput for use in the Repl loop.
func (interp *Interp) StartREPL() {
	atomic.StoreUint32(&interp.cancel, 0)
	interp.inputCond.Signal() // end any active input
	interp.pc.EditorInterface().StartInput(interp.EndLineInput)
}

func (interp *Interp) EndLineInput() {
	in, ok := interp.pc.EditorInterface().EndInput()
	if !ok {
		in = `(beep 'error)`
	}
	// cur := interp.pc.VRAM().HiddenCursor()
	// interp.pc.VRAM().SetHideCursor(true)
	reader := NewReader(strings.NewReader(in))
	x, err := reader.Read()
	if err != nil {
		interp.PrintError(err)
	} else if x != EofToken {
		x, err = interp.SafeEval(x, Nil)
		if err != nil {
			interp.PrintError(err)
		} else {
			if s, ok := x.(*Sym); ok && s == Void {
				// interp.pc.EditorInterface().Print("<void>")
			} else {
				interp.pc.EditorInterface().Print(fmt.Sprintf("%v\n", Str(x)))
			}
			interp.pc.SoundInterface().SystemSound(SND_READY)
		}
	}
	// interp.pc.VRAM().SetHideCursor(cur)
	interp.StartREPL()
}

// PrintError prints the given error using the *error-printer* global variable if it has been set.
// The variable *error-printer* must contain a closure that takes an error or string as argument.
// If *error-printer* is unbound, then the built-in error printer is called.
func (interp *Interp) PrintError(err any) {
	errorPrinter, ok := interp.GetGlobalVar(ErrorPrinter)
	var printer *Closure
	if ok {
		printer, ok = errorPrinter.(*Closure)
		if ok {
			_, err := interp.SafeEval(&Cell{printer, &Cell{err, Nil}}, Nil)
			if err == nil {
				return
			}
		}
	}
	interp.DefaultPrintError(err)
}

// DefaultPrintError is the default error printer that simply outputs the error string.
func (interp *Interp) DefaultPrintError(err any) {
	interp.pc.EditorInterface().Print(fmt.Sprintf("%v\n", err))
	interp.pc.SoundInterface().SystemSound(SND_ERROR)
}

// HandleError attempts to handle the error by invoking *error-handler* if it is defined.
// If there is no error handler or an error occurs in it, then the default error printer is called
// and Void, false is returned. If the error is handled successfuly, the handler's result and true is returned.
func (interp *Interp) HandleError(err any) (any, bool) {
	errorHandler, ok := interp.GetGlobalVar(ErrorHandler)
	if ok {
		dict, ok := errorHandler.(*Dict)
		if ok && dict != nil {
			var gid, hdl any
			gid = int64(GetGID())
			hdl, ok = dict.Data.Load(goarith.AsNumber(gid))
			if !ok {
				hdl, ok = dict.Data.Load(goarith.AsNumber(0))
			}
			// We're evaluating both lists and closures here, since both are allowed in an error handler.
			// Lists are simply interpreted as function calls as usual.
			if ok {
				cell, ok := hdl.(*Cell)
				if ok {
					closure, ok := cell.Car.(*Closure)
					if ok {
						result, err := interp.SafeEval(&Cell{closure, &Cell{err, Nil}}, Nil)
						if err == nil {
							return result, true
						}
					}
					// it wasn't a closure, so let's just call the list as a function call.
					result, err := interp.SafeEval(&Cell{cell, &Cell{err, Nil}}, Nil)
					if err == nil {
						return result, true
					}
				}
			}
		}
	}
	interp.PrintError(err)
	return Void, false
}

// Run executes REPL (Read-Eval-Print Loop).
// It returns false if REPL was ceased by an error.
// It returns true if REPL was finished normally.
func (interp *Interp) Run(input io.Reader) bool {
	interactive := (input == nil)
	if interactive {
		input = os.Stdin
	}
	reader := NewReader(input)
	for {
		if interactive {
			interp.pc.EditorInterface().Print("> ")
		}
		x, err := reader.Read()
		if err == nil {
			if x == EofToken {
				return true // Finished normally.
			}
			x, err = interp.SafeEval(x, Nil)
			if err == nil {
				if interactive {
					if s, ok := x.(*Sym); ok && s == Void {
						interp.pc.EditorInterface().Print("")
					} else {
						interp.pc.EditorInterface().Print(Str(x) + "\n")
					}
				}
			}
		}
		if err != nil {
			interp.HandleError(err)
			if !interactive {
				return false // Ceased by an error.
			}
		}
	}
}

// EvalString evaluates a string and returns the result or an error. If string contains more than one expression, then
// only the first expression is executed. To prevent this, you may wrap several expressions into (progn expr ...).
// If the expression is empty, io.EOF error is returned. Use Str(result) to print the result in human-readable form.
func (interp *Interp) EvalString(s string) (any, error) {
	reader := NewReader(strings.NewReader(s))
	x, err := reader.Read()
	if err != nil {
		interp.HandleError(err)
		return Void, err.(error)
	}
	if x == EofToken {
		return Void, io.EOF
	}
	result, err := interp.SafeEval(x, Nil)
	if err != nil {
		return Void, err.(error)
	}
	return result, nil
}

// EvalFile evaluates a file, returns true if no error has occurred and false if an error has occurred.
func (interp *Interp) EvalFile(path string) bool {
	input, err := os.Open(path)
	if err != nil {
		log.Println(err)
		return false
	}
	defer input.Close()
	return interp.Run(input)
}

// Print outputs a string according using the runtime EditorInterface Print function. This is equivalent to
// using (out datum) on the Lisp side.
func (interp *Interp) Print(s string) {
	interp.pc.EditorInterface().Print(s)
}

// Main runs each element of args as a name of Lisp script file.
// It ignores args[0].
// If it does not have args[1] or some element is "-", it begins REPL.
// func Main(args []string) int {
// 	interp := NewInterp()
// 	ss := strings.NewReader(Prelude)
// 	if !Run(interp, ss) {
// 		return 1
// 	}
// 	if len(args) < 2 {
// 		args = []string{args[0], "-"}
// 	}
// 	for i, fileName := range args {
// 		if i == 0 {
// 			continue
// 		}
// 		if fileName == "-" {
// 			Run(interp, nil)
// 			fmt.Println("Goodbye")
// 		} else {
// 			file, err := os.Open(fileName)
// 			if err != nil {
// 				fmt.Println(err)
// 				return 1
// 			}
// 			if !Run(interp, file) {
// 				return 1
// 			}
// 		}
// 	}
// 	return 0
// }

// func main() {
// 	os.Exit(Main(os.Args))
// }

// Prelude is an initialization script of Lisp.
// Each "~" is replaced by "`" at runtime.
var Prelude = strings.Replace(`
(setq defmacro
      (macro (name args &rest body)
             ~(progn (setq ,name (macro ,args ,@body))
                     ',name)))

(defmacro defun (name args &rest body)
  ~(progn (setq ,name (lambda ,args ,@body))
          ',name))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun not (x) (eq? x nil))
(defun cons? (x) (not (atom? x)))
(defun print (x) (prin1 x) (terpri) x)
(defun identity (x) x)

(defun feature? (sym)
 (if (member sym *reflect*) t nil))

(setq
 = eql?
 null? not
 setcar rplaca
 setcdr rplacd)

(defun > (x y) (< y x))
(defun >= (x y) (not (< x y)))
(defun <= (x y) (not (< y x)))
(defun /= (x y) (not (= x y)))

(defun equal? (x y)
  (cond ((atom? x) (eql? x y))
        ((atom? y) nil)
        ((equal? (car x) (car y)) (equal? (cdr x) (cdr y)))))

(defmacro if (test then &rest else)
  ~(cond (,test ,then)
         ,@(cond (else ~((t ,@else))))))

(defmacro when (test &rest then)
  ~(cond (,test ,@then)
         ,@(cond (t ~((t (void)))))))

(defmacro unless (test &rest then)
  ~(cond ((not ,test) ,@then)
         ,@(cond (t ~((t (void)))))))

;; original definition returns nil instead of void:
;; (defmacro when (test &rest body)
;;  ~(cond (,test ,@body)))

(defmacro let (args &rest body)
  ((lambda (vars vals)
     (defun vars (x)
       (cond (x (cons (if (atom? (car x))
                          (car x)
                        (caar x))
                      (vars (cdr x))))))
     (defun vals (x)
       (cond (x (cons (if (atom? (car x))
                          nil
                        (cadar x))
                      (vals (cdr x))))))
     ~((lambda ,(vars args) ,@body) ,@(vals args)))
   nil nil))

(defmacro letrec (args &rest body)      ; (letrec ((v e) ...) body...)
  (let (vars setqs)
    (defun vars (x)
      (cond (x (cons (caar x)
                     (vars (cdr x))))))
    (defun sets (x)
      (cond (x (cons ~(setq ,(caar x) ,(cadar x))
                     (sets (cdr x))))))
    ~(let ,(vars args) ,@(sets args) ,@body)))

(defun _append (x y)
  (if (null? x)
      y
    (cons (car x) (_append (cdr x) y))))

(defmacro append (x &rest y)
  (if (null? y)
      x
    ~(_append ,x (append ,@y))))

(defmacro and (x &rest y)
  (if (null? y)
      x
    ~(cond (,x (and ,@y)))))

(defun mapcar (x f)
  (and x (cons (f (car x)) (mapcar (cdr x) f))))

(defmacro or (x &rest y)
  (if (null? y)
      x
    ~(cond (,x)
           ((or ,@y)))))

(defun list? (x)
  (or (null? x) (cons? x)))    ; NB (list? (lambda (x) (+ x 1))) => nil

(defun memq (key x)
  (cond ((null? x) nil)
        ((eq? key (car x)) x)
        (t (memq key (cdr x)))))

(defun member (key x)
  (cond ((null? x) nil)
        ((equal? key (car x)) x)
        (t (member key (cdr x)))))

(defun assq (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (cons? e) (eq? key (car e)))
                     e
                   (assq key (cdr alist)))))))

(defun assoc (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (cons? e) (equal? key (car e)))
                     e
                   (assoc key (cdr alist)))))))

(defun _nreverse (x prev)
  (let ((next (cdr x)))
    (setcdr x prev)
    (if (null? next)
        x
      (_nreverse next x))))
(defun nreverse (list)            ; (nreverse '(a b c d)) => (d c b a)
  (cond (list (_nreverse list nil))))

(defun list-last (list)
  (if (atom? (cdr list))
      list
    (list-last (cdr list))))

(defun nconc (&rest lists)
  (if (null? (cdr lists))
      (car lists)
    (if (null? (car lists))
        (apply nconc (cdr lists))
      (setcdr (list-last (car lists))
              (apply nconc (cdr lists)))
      (car lists))))

(defmacro while (test &rest body)
  (let ((loop (gensym)))
    ~(letrec ((,loop (lambda () (cond (,test ,@body (,loop))(t (void))))))
       (,loop))))

(defmacro dolist (spec &rest body) ; (dolist (name list [result]) body...)
  (let ((name (car spec))
        (list (gensym)))
    ~(let (,name
           (,list ,(cadr spec)))
       (while ,list
         (setq ,name (car ,list))
         ,@body
         (setq ,list (cdr ,list)))
       ,@(if (cddr spec)
             ~((setq ,name nil)
               ,(caddr spec))))))

(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)
  (let ((name (car spec))
        (count (gensym)))
    ~(let ((,name 0)
           (,count ,(cadr spec)))
       (while (< ,name ,count)
         ,@body
         (setq ,name (+ ,name 1)))
       ,@(if (cddr spec)
             ~(,(caddr spec))))))

(setq *mutable-toplevel-symbols* (dict))

(defun declare-unprotected (sym)
  (set *mutable-toplevel-symbols* sym t))

(setq *volatile-toplevel-symbols* (dict))

(defun declare-volatile (sym)
  (set *volatile-toplevel-symbols* sym t))

(declare-volatile '*volatile-toplevel-symbols*)
(declare-volatile '*mutable-toplevel-symbols*)
(declare-unprotected 'vars) ; fixes unclean macro definitions using defun 
(declare-unprotected 'vals) ; so these do not get protected later
(declare-unprotected 'sets) ; unbind does not do the job since init sequence uses the macros later

`, "~", "`", -1)

/*
  Copyright (c) 2015, 2016 OKI Software Co., Ltd.
  Copyright (c) 2019 SUZUKI Hisao
  Copyright (c) 2019-2022 Erich Rast

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
*/
