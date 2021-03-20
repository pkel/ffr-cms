.PHONY: build
build:
	dune build

.PHONY: dependencies
dependencies: ffr-opium.opam devtools.opam
	opam install . --deps-only

.PHONY: test
test:
	dune runtest

.PHONY: import
import:
	migrate/kats-scraper.sh
	migrate/import.sh

ffr-opium.opam devtools.opam: dune-project
	dune build ffr-opium.opam devtools.opam
