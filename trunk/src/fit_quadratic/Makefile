# Makefile for          :  fit_quadratic 
# Module Author         :  Orion Lawlor

include ../../make_support/system_rules

OBJS  = matrix.o \
	quadratic.o \
	fit_quadratic.o

LIBS  = $(LIBDIR)/asf.a \
	-lm

all: fit_quadratic
	-rm *.o

# -lm necessary for compilation on Solaris
fit_quadratic: $(OBJS)
	$(CC) $(CFLAGS) -o fit_quadratic $(OBJS) $(LIBS) $(LDFLAGS)
	mv fit_quadratic$(BIN_POSTFIX) $(BINDIR)
	cp fit_quadratic.1 ../../man/cat1

clean:
	-rm -f core *.o





