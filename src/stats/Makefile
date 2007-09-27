# Makefile for ASF stats program

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	-lm

CFLAGS += \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

OBJS  = statIO.o \
	stats1.o

all: stats1 clean


stats1: $(OBJS)
	$(CC) $(CFLAGS) -o stats1 $(OBJS) $(LIBS) $(LDFLAGS)
	mv stats1$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS)
