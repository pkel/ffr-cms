# To-Do

- speed: In-memory Irmin store + background sync with remote git
- ux: stage changes in on-disk repo before pushing to remote git
- data: migrate old posts
  * we have the data in `migrate/_kats`
  * next, we'll need an OCaml script that adds posts/pictures to the
    irmin db
- ux: support einsaetze und neuigkeiten
  * add category field to post header
  * derive path/key from this field
  * provide dropdown on frontend
- ux: allow configuration of remote/local git
- ux: read passwords from remote git + tool for writing these passwords
- speed: irmin middleware with support for caching.
  * lazy version: use a git checkout
