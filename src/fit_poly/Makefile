# Makefile for          :  fit_quadratic 
# Module Author         :  Orion Lawlor

include ../../make_support/system_rules

OBJS  = matrix.o \
	poly.o \
	fit_poly.o

all: fit_poly
	-rm *.o

# -lm necessary for compilation on Solaris
fit_poly: $(OBJS) $(LIBDIR)/asf.a
	${CC} ${CFLAGS} -o fit_poly $(OBJS) $(LIBDIR)/asf.a -lm
	mv fit_poly$(BIN_POSTFIX) $(BINDIR)
	cp fit_poly.1 ../../man/cat1

clean:
	-rm -f core *.o





