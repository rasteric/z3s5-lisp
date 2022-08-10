# Version History

## Version 2.3.5

Build Tags: `db fts5` Sqlite3 database support, `fileio` file access

- Added kvdb module (Lisp code) and support for `remember`. These require the `db` module, hence both build tags `db` and `fts5` must be used.

- Added shutdown hook in z3 executable. Other built-in hooks are defined but not yet called. (Most of them are reserved for the Z3S5 Machine.)

**Warning: If you're running the embedded interpreter you have to call the shutdown hook manually or create your own runtime that does that!**

Bugs: Known regression bug in zimages. We need to check for global DB values and gracefully handle their initialization by calling a start hook once an image is loaded.

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
