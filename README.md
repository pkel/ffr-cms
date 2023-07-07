# To-Do

- ux: remove email field from user. Use <handle@configurable-domain>
  instead.
- speed: In-memory Irmin store + background sync with on-disk git
- ux: stage changes in on-disk repo before pushing to remote git
- fix: Return proper Etags as soon as opium supports Lwt.t etags
  https://github.com/rgrinberg/opium/issues/265
- investigate whether newer irmin versions support bare git repositories

# Sync strategy

Irmin / ocaml-git seem to have trouble with pushing/pulling from remote
repositories. The straight-forward connections method (ssh + deploy key)
seems to be not supported.

For the PoC, we can just define the authorative repo to be on the server
running the web app. Netlify/Gitlab can pull from there. Or we do
optimistic pull/push from the opium server using git command line
interface.

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

# Setup (Ubuntu 20.04 LTS)

```shell
./setup/setup.sh
```

# Container

I'm working on deployment via OCI containers. The `Dockerfile` has all
instructions for building the containers.

```
# build the build image with all OCaml dependencies
podman build --target deps --tag ffr-cms/deps .
# build the OCaml project in a container
podman build --target build --tag ffr-cms/build .
# build and run the container serving the cms
podman build --target srv --tag ffr-cms/srv .
podman run -p 3000:3000/tcp -v "$(pwd)/_db.git":/website.git:Z --userns=keep-id:uid=1000,gid=1000 ffr-cms/srv
# inspect container
podman run -p 3000:3000/tcp -v "$(pwd)/_db.git":/website.git:Z --userns=keep-id:uid=1000,gid=1000 ffr-cms/srv bash
```
