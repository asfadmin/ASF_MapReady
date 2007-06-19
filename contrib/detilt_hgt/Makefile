# Makefile for deltilt_hgt

FC     = g77
FFLAGS = -O2 -ffixed-line-length-132

FOBJECTS = detilt_hgt.o

all: detilt_hgt clean

detilt_hgt: $(FOBJECTS)
	$(FC) $(FFLAGS) -o detilt_hgt $(FOBJECTS)

clean:
	rm -f *.o
