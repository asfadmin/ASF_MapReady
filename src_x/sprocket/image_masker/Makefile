# Makefile for ASF Converter
include  ../mk.common

all: $(BIN)/image_mask

$(BIN)/image_mask: image_mask.o
	$(LINK) $(BIN)/image_mask image_mask.o $(LINK_LIB)

image_mask.o: image_mask.c
	$(COMP) image_mask.c
#	$(BEAT) image_mask.c

clean:
	rm -f *.o $(BIN)/image_mask
