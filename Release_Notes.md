# Version History

## Version 2.3.4

Modules: `(zimage tasks help beep db fileio decimal ling float console base)`

Build Tags: `db` Sqlite3 database support, `fileio` file access

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
