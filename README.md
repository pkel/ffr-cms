# To-Do

- speed: In-memory Irmin store + background sync with remote git
- ux: stage changes in on-disk repo before pushing to remote git
- admin: allow configuration of remote/local git
- admin: read passwords from remote git + tool for writing these passwords
- speed: incremental import / do not re-encode images
- ux: confirmation before deletion of images/posts
- fix: Return proper Etags as soon as opium supports Lwt.t etags
  https://github.com/rgrinberg/opium/issues/265

# Image strategy proposal

Netlify seems to support git-lfs for images. It can also rescale them
for us (2.5k images per month should be enough for our needs).

We could hook up git-lfs command line tools from OCaml. Saving images
would be piping to disk. Requesting images would be via opium static
file middleware. Image-magick conversion might not be needed on the
server. We might run into repo size limits (Gitlab 10GB).

To avoid the repo size limit, we could do a mixed approach: Shrink
images to the lower 100k file size. Maybe 1280x1280. Then store them in
lfs.

For now, we should be fine with 1280x1280 and aggressive compression,
directly tracked in git.
