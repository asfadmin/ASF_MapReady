# Makefile for ASF stats program

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(XML_LIBS) \
	-lm

CFLAGS += \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

OBJS  = statIO.o \
	stats.o

all: stats clean


stats: $(OBJS)
	$(CC) $(CFLAGS) -o stats $(OBJS) $(LIBS) $(LDFLAGS)
	mv stats$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS)
