# Makefile for avg_in_dop
# Author: Patrick Denny
# Version 1.1


include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = avg_in_dop.o

all: avg_in_dop clean

avg_in_dop: $(OBJS)
	$(CC) $(CFLAGS) -o avg_in_dop $(OBJS) $(LIBS) $(LDFLAGS)
	mv avg_in_dop$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS)
