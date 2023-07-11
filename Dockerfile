ARG DEBIAN_VERSION=11
ARG OCAML_VERSION=4.14

FROM docker.io/ocaml/opam:debian-${DEBIAN_VERSION}-ocaml-${OCAML_VERSION} as build-system
USER root
ADD container/install.sh /usr/bin/docker-install
RUN docker-install libargon2-dev libev-dev libffi-dev libgmp-dev pkg-config
USER opam
ADD ffr-cms.opam /home/opam/
RUN opam install . --deps-only && opam clean -y

FROM build-system as build-cms
ADD *.opam dune* lib src /home/opam/
RUN opam exec dune build && opam exec dune runtest && cp -rL _build/install/default/bin ./ && rm -rf _build

FROM docker.io/debian:${DEBIAN_VERSION} as base-system
ADD container/install.sh /usr/bin/docker-install
RUN docker-install entr git
RUN mkdir -p /home/ffr && useradd ffr -d /home/ffr && chown -R ffr:ffr /home/ffr

FROM base-system as checkout
ENV BRANCH main
ADD container/watch.sh container/checkout.sh /script
RUN mkdir /checkout && chown -R ffr:ffr /checkout
VOLUME /website.git
VOLUME /checkout
WORKDIR /home/ffr
USER ffr
CMD ["/script/watch.sh", "/script/checkout.sh"]

FROM base-system as hugo
ARG HUGO_VERSION=0.84.3
ADD https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_Linux-64bit.tar.gz /install/hugo.tar.gz
RUN cd /install && tar -xf hugo.tar.gz && cp hugo /usr/bin && cd / && rm -r /install

FROM hugo as preview
USER ffr
VOLUME /checkout
WORKDIR /checkout
CMD ["hugo", "server", "--buildDrafts", "--bind", "0.0.0.0", "--port", "3001"]
EXPOSE 3001/tcp

FROM hugo as build-www
ENV BRANCH main
ADD container/watch.sh container/build-www.sh /script
RUN mkdir /www && chown -R ffr:ffr /www
USER ffr
VOLUME /www
CMD ["/script/watch.sh", "/script/build-www.sh"]

FROM base-system as cms
ENV PREVIEW_URL /vorschau
RUN docker-install imagemagick git libargon2-dev libev-dev libffi-dev libgmp-dev
RUN git init --bare --initial-branch main /website.git && chown -R ffr:ffr /website.git
ADD container/ffr-cms.sh /usr/bin/ffr-cms-wrapper
COPY --from=build-cms /home/opam/bin/* /usr/bin/
ADD static /static
VOLUME /website.git
WORKDIR /home/ffr
USER ffr
CMD ["ffr-cms-wrapper"]
EXPOSE 3000/tcp
