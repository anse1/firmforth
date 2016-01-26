
all: firmforth

gitrev.h: .git/*
	echo "#define GITREV \"$$(git describe --dirty --tags --always)\"" > $@

firmforth: firmforth.c firmforth.h gitrev.h
	$(CC) -std=gnu99 -Wall $< -o $@

clean:
	rm -f firmforth
