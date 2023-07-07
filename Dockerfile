ARG DEBIAN_VERSION=11
ARG OCAML_VERSION=4.14

FROM docker.io/ocaml/opam:debian-${DEBIAN_VERSION}-ocaml-${OCAML_VERSION} as build-system
USER root
RUN apt-get install -y libargon2-dev libev-dev libffi-dev libgmp-dev pkg-config
USER opam
ADD ffr-cms.opam /home/opam/
RUN opam install . --deps-only

FROM build-system as build-cms
ADD *.opam dune* lib src /home/opam/
RUN opam exec dune build && \
  opam exec dune runtest && \
  cp -rL _build/install/default/bin ./ && \
  rm -rf _build

FROM docker.io/debian:${DEBIAN_VERSION} as cms
ADD container/cms /script
RUN cd /script && bash setup.sh && rm setup.sh
COPY --from=build-cms /home/opam/bin/* /usr/bin/
ADD static /static
VOLUME /website.git
WORKDIR /home/ffr
USER ffr
CMD ["bash", "/script/main.sh"]
EXPOSE 3000/tcp

FROM docker.io/debian:${DEBIAN_VERSION} as checkout
ENV BRANCH master
ADD container/checkout /script
RUN cd /script && bash setup.sh && rm setup.sh
VOLUME /website.git
VOLUME /checkout
WORKDIR /home/ffr
USER ffr
CMD ["bash", "/script/main.sh"]

FROM docker.io/debian:${DEBIAN_VERSION} as preview
ARG HUGO_VERSION=0.84.3
ADD container/preview /script
ADD https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_Linux-64bit.tar.gz /install/hugo.tar.gz
RUN cd /script && bash setup.sh && rm setup.sh
VOLUME /checkout
USER ffr
WORKDIR /home/ffr
CMD ["bash", "/script/main.sh"]
EXPOSE 3001/tcp

FROM docker.io/debian:${DEBIAN_VERSION} as build-www
ADD container/build-www /script
RUN cd /script && bash setup.sh && rm setup.sh
VOLUME /checkout
VOLUME /www
WORKDIR /home/ffr
USER ffr
CMD ["bash", "/script/main.sh"]
