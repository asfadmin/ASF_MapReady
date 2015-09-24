# Makefile for meta2ddr/ddr2meta
# Author Patrick Denny

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	-lm -lz

CFLAGS += $(LIBTIFF_CLFAGS) $(GEOTIFF_CFLAGS)

OBJS = driver.o \
	make_s1a_mapping.o

all: make_s1a_mapping

make_s1a_mapping: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o make_s1a_mapping $(OBJS) $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv make_s1a_mapping$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS)
