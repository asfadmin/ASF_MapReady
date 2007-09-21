# Makefile for ampescheby

include ../../make_support/system_rules

FC     = g77
FFLAGS = $(OPTFLAG) $(LARGEFILE_FLAGS) -ffixed-line-length-132

FOBJECTS = ampscheby.o

all: ampscheby clean

ampscheby: $(FOBJECTS)
	$(FC) $(FFLAGS) -o ampscheby $(FOBJECTS)
	mv ampscheby$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
