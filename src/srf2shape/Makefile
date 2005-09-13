# Makefile for          :  srf2shape
# Module Author         :  Rudi Gens 

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a $(LIBDIR)/asf_meta.a -lm

OBJS = 	srf2shape.o
BOBJS = base2shape.o

all: srf2shape base2shape

srf2shape: $(OBJS)
	$(CC) -o srf2shape $(OBJS) $(LIBS)
	rm *.o
	cp srf2shape$(BIN_POSTFIX) $(BINDIR)
base2shape: $(BOBJS)
	$(CC) -o base2shape $(BOBJS) $(LIBS)
	rm *.o
	cp base2shape$(BIN_POSTFIX) $(BINDIR)

