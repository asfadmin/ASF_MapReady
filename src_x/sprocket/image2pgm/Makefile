# Makefile for ASF Converter
include  ../mk.common

all: $(BIN)/image2pgm

$(BIN)/image2pgm: image2pgm.o
	$(LINK) $(BIN)/image2pgm image2pgm.o $(LINK_LIB)

image2pgm.o: image2pgm.c
	$(COMP) image2pgm.c
clean:
	rm -f *.o $(BIN)/image2pgm
