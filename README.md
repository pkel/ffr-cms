# To-Do

- ux: process images in background. Return thumbnails first.
- speed: In-memory Irmin store + background sync with remote git
- ux: stage changes in on-disk repo before pushing to remote git
- admin: allow configuration of remote/local git
- admin: read passwords from remote git + tool for writing these passwords
- speed: irmin middleware with support for caching.
  * lazy version: use a git checkout
- speed: incremental import / do not re-encode images

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
