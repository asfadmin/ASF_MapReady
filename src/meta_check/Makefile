# Makefile for meta_check

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	$(GLIB_LIBS)

OBJS  = meta_check.o

all: meta_check clean

meta_check: $(OBJS)
	$(CC) $(CFLAGS) -o meta_check $(OBJS) $(LIBS) $(LDFLAGS)
	cp meta_check$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o *~
