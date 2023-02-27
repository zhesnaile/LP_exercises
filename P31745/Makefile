PROGRAM_NAME=p31745

all: build
	./$(PROGRAM_NAME)

build: $(PROGRAM_NAME).hs
	ghc $(PROGRAM_NAME).hs

test: all
	./$(PROGRAM_NAME) | diff - sample.cor

clean: $(PROGRAM_NAME) $(PROGRAM_NAME).hi $(PROGRAM_NAME).o
	rm $(PROGRAM_NAME) $(PROGRAM_NAME).hi $(PROGRAM_NAME).o
