# Makefile for meta_check

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GLIB_LIBS)

OBJS  = meta_check.o

all: meta_check clean

meta_check: $(OBJS)
	$(CC) $(CFLAGS) -o meta_check $(OBJS) $(LIBS) $(LDFLAGS)
	cp meta_check$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o *~
