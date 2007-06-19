# Makefile for ampescheby

FC     = g77
FFLAGS = -O2 -ffixed-line-length-132

FOBJECTS = ampscheby.o

all: ampscheby clean

ampscheby: $(FOBJECTS)
	$(FC) $(FFLAGS) -o ampscheby $(FOBJECTS)

clean:
	rm -f *.o
