# Makefile for deltilt_hgt

include ../../make_support/system_rules

FC     = g77
FFLAGS = $(OPTFLAG) $(LARGEFILE_FLAGS) -ffixed-line-length-132

FOBJECTS = detilt_hgt.o

all: detilt_hgt clean

detilt_hgt: $(FOBJECTS)
	$(FC) $(FFLAGS) -o detilt_hgt $(FOBJECTS)
	mv detilt_hgt$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
