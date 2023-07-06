ARG DEBIAN_VERSION=11
ARG OCAML_VERSION=4.14

FROM docker.io/ocaml/opam:debian-${DEBIAN_VERSION}-ocaml-${OCAML_VERSION} as deps
USER root
RUN apt-get install -y libargon2-dev libev-dev libffi-dev libgmp-dev pkg-config
USER opam
ADD ffr-opium.opam /home/opam/
RUN opam install . --deps-only

FROM deps as build
ADD *.opam dune* lib src /home/opam/
RUN opam exec dune build && opam exec dune runtest

FROM docker.io/debian:${DEBIAN_VERSION} as srv
RUN apt-get update && \
  apt-get -y upgrade && \
  apt-get install -y git libargon2-dev libev-dev libffi-dev libgmp-dev pkg-config && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*
COPY --from=build /home/opam/_build/install/default/bin/* /usr/bin/
ADD container/srv-main.sh /usr/bin/srv-main
ADD static /static
RUN mkdir -p /var/lib/ffr-opium && \
  useradd ffr -d /var/lib/ffr-opium && \
  git init --bare --initial-branch master /website.git && \
  chown -R ffr:ffr /website.git /var/lib/ffr-opium
VOLUME /website.git
WORKDIR /var/lib/ffr-opium
USER ffr
CMD ["srv-main"]
EXPOSE 3000/tcp
