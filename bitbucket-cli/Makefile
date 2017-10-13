MFLAGS =
MAKEFLAGS = $(MFLAGS)
VER = gen/BuildInfo_bee.hs gen/version

.PHONY: version build test tags clean clobber

default: test

version:
	mkdir -p gen/
	../bin/git-version bitb

${VER}: version

build: ${VER}
	./mafia build

test: ${VER}
	./mafia test

clean:
	rm -rfv gen/
	./mafia clean

clobber: clean
