# Makefile for ceos2meta
# Author Patrick Denny

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

OBJS  = ceos2meta.o

all: ceos2meta clean

ceos2meta: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ceos2meta $(OBJS) $(LIBS)
	mv ceos2meta$(BIN_POSTFIX) $(BINDIR)
#	cp ceos2meta.1 ../../man/man1/ceos2meta.1

clean:
	rm -f $(OBJS)
