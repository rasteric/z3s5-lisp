# Version History

## Version 2.4.2 

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

- Some additions to the Fyne GUI support. It is now fairly complete.

- Various bug fixes.

- Initial support for the zedit editor, which will pave the way for embedded editing in `z3g` and later use in the Z3S5 Lisp machine.

## Version 2.4

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

- Many bug fixes.

- Added initial support for GUI in subpackage GUI. This needs to be imported separately instead of using a build tag in order to minimize dependencies of the base language.

- Added GUI exectuable in `cmd/z3g/z3g.go`. Check this file on how to create a GUI capable Z3S5 Lisp, see the demos for examples.

- Work on editor.lisp to create a GUI editor. This is a step towards a new Lisp machine, since the old attempt using Raylib was too crufty.

## Version 2.3.11

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

- Fixed various bugs with actions. However, the API is not yet final.

- Added EvalString to evaluate strings.

- There is an open problem with (load ...) requiring fileio. This might change in future to allow loading libraries from local relative paths directly even when general fileio is not allowed.


## Version 2.3.10

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

- Added OOP module 'oop, a simple object-oriented framework.

- Added experimental support for actions, which provide an OOP way to interface with the Go host implementation.

- Fixed bugs in the OOP implementation.

## Version 2.3.8

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

- Library system ported from Z3S5 Machine, providing the (load prefix) command. See (help load).

- Object-oriented programming: An `'oop` extension is embedded into the Lisp, ensuring that it is always available. Check out the help entries for symbols in `(help-about 'oop)`.

- Fixed bugs in OOP, wrote tests for OOP module.


## Version 2.3.7

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

This is a maintenance release, recompiled with Go 1.19.

## Version 2.3.6

Build tags: `db fts5` for Sqlite3 support, `fileio` for file access.

This is a maintenance release, updating the manual.

## Version 2.3.5

Build Tags: `db fts5` Sqlite3 database support, `fileio` file access

- Added kvdb module (Lisp code) and support for `remember`. These require the `db` module, hence both build tags `db` and `fts5` must be used.

- Added shutdown hook in z3 executable. Other built-in hooks are defined but not yet called. (Most of them are reserved for the Z3S5 Machine.)

**Warning: If you're running the embedded interpreter you have to call the shutdown hook manually or create your own runtime that does that!**

## Version 2.3.4

Modules: `(zimage tasks help beep db fileio decimal ling float console base)`

Build Tags: `db fts5` Sqlite3 database support, `fileio` file access

- Further improvements to the documentation.

- Fixed a rare bug with recursive printing of lists.

- Added zimage module for saving, loading, and running Lisp images. These literally overwrite the global symbol of the live system.

- Added an `-e <exprs>` option to the z3 interpreter to execute expressions `<expr>` from the command-line 

## Version 2.3.3

Modules: `(db tasks help beep fileio decimal ling float console base)`

Build tags: `db` Sqlite3 database support, `fileio` file access

- Various small improvements and bug fixes.

- Vastly improved *Reference Manual* with auto-generated index.

- Sqlite3 support with the `db` build tag.

## Version 2.3.2

This is the first release with fileio and manual. It supports modules `(tasks help beep fileio decimal ling float console base)`, where `fileio` is only included with the build tag `fileio`.
