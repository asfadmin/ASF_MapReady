# Makefile for ceos2meta
# Author Patrick Denny

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm $(LIBDIR)/libgsl.a

OBJS  = ceos2meta.o

all: ceos2meta clean

ceos2meta: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ceos2meta $(OBJS) $(LIBS)
	mv ceos2meta$(BIN_POSTFIX) $(BINDIR)
#	cp ceos2meta.1 ../../man/man1/ceos2meta.1

clean:
	rm -f $(OBJS)
