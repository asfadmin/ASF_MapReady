# Makefile for deltilt_hgt

include ../Makerules.mk

FOBJECTS = detilt_hgt.o

all: detilt_hgt clean

detilt_hgt: $(FOBJECTS)
	$(FC) $(FFLAGS) -o detilt_hgt $(FOBJECTS)
	mv detilt_hgt$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
