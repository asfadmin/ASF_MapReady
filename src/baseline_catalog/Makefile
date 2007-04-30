# Makefile for libasf_baseline

include ../../make_support/system_rules

CFLAGS := $(JPEG_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS) $(CFLAGS)

LIBS =  \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LDFLAGS) \
	-lm

OBJLIB = 	\
	baseline2vector.o \
	filter_srf.o \
	find_pairs.o \
	determine_baseline.o \
	read_srf.o \
	generate_products.o \
	baseline_catalog.o

all: lib exe clean

lib: $(OBJLIB)
	rm -f libasf_baseline.a
	ar r libasf_baseline.a $(OBJLIB)
	$(RANLIB) libasf_baseline.a
	mv libasf_baseline.a $(LIBDIR)
	cp asf_baseline.h $(ASF_INCLUDE_DIR)

exe: baseline_catalog

baseline_catalog: baseline_catalog_main.c lib
	$(CC) $(CFLAGS) -o baseline_catalog $(LIBDIR)/libasf_baseline.a $(LIBS)
	rm *.o
	mv baseline_catalog$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*