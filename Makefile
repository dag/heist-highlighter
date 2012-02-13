all: check-cabal hlint test

.PHONY: check-cabal
check-cabal:
	cabal check

.PHONY: hlint
hlint:
	hlint --color src

cabal-dev:
	cabal-dev install   -ftests --disable-optimization --only-dependencies
	cabal-dev configure -ftests --disable-optimization

.PHONY: build
build: cabal-dev
	cabal-dev build

.PHONY: test
test: build
	dist/build/test-heist-highlighter/test-heist-highlighter
