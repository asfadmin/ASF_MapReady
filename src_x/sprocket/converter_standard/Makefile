# Makefile for ASF Converter
include ../mk.common

all: $(BIN)/convert_standard 

$(BIN)/convert_standard: convert_standard.o
	$(LINK) $(BIN)/convert_standard convert_standard.o $(STEP_LINK_LIB)

convert_standard.o: convert_standard.c
	$(STEP_COMP) convert_standard.c
#	$(BEAT) convert_standard.c

clean:
	rm -f *.o $(BIN)/convert_standard
