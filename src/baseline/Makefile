# Makefile for 		:  baseline
# Module Author 	:  Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a -lm
OBJS = baseline.o 

all: baseline
	rm *.o

baseline:  $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o baseline $(OBJS) $(LIBS)
	mv baseline$(BIN_POSTFIX) $(BINDIR)


