all: build
	./p25054

build: p25054.hs
	ghc p25054.hs

test: all
	./p25054 | diff - sample.cor

clean: p25054 p25054.hi p25054.o
	rm p77907 p25054.hi p25054.o
