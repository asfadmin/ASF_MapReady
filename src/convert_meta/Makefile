# Makefile for 		:  convert_meta
# Module Author 	:  Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS = $(LIBDIR)/libasf_meta.a \
	$(LIBDIR)/asf.a -lm

OBJS = convert_meta.o 

all: convert_meta
	rm *.o

convert_meta:  $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o convert_meta $(OBJS) $(LIBS)
	cp convert_meta$(BIN_POSTFIX) $(BINDIR)


