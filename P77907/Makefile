all: build
	./p77907

build: p77907.hs
	ghc p77907.hs

test: all
	./p77907 | diff - sample.cor

clean: p77907 p77907.hi p77907.o
	rm p77907 p77907.hi p77907.o
