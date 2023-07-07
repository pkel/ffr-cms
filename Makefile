.PHONY: build
build:
	dune build

.PHONY: dependencies
dependencies: ffr-cms.opam
	opam install . --deps-only

.PHONY: switch
switch: ffr-cms.opam
	opam switch create . --deps-only

.PHONY: dev-init
dev-init:
	@echo Add development user:
	@echo FZMV9Kgha69fN3sAbiK2 | dune exec ffr-set-user pkel 'patrik@pkel.dev' 'Patrik Keller'

.PHONY: test
test:
	dune runtest

.PHONY: import
import:
	migrate/kats-scraper.sh
	migrate/import.sh

.PHONY: push-florian
push-florian:
	ssh florian git -C ffr-cms switch --detach master
	git push florian
	ssh florian git -C ffr-cms switch master
	# now run "cd ffr-cms && bash setup/setup.sh" on florian

ffr-cms.opam: dune-project
	dune build ffr-cms.opam
