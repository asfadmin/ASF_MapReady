# Makefile for ampescheby

include ../Makerules.mk

FOBJECTS = ampscheby.o

all: ampscheby clean

ampscheby: $(FOBJECTS)
	$(FC) $(FFLAGS) -o ampscheby $(FOBJECTS)
	mv ampscheby$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
