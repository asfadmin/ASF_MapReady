#	reskew_dem
#	by Orion Lawlor, ASF, 1997
#	Reskew will remap a ground range DEM into a slant-range
#       DEM.

include ../../make_support/system_rules

LIBS :=	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

CFLAGS += $(GSL_CFLAGS) $(JPEG_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = flipv.o

all: flipv
	rm *.o

flipv:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) $(LDFLAGS) -o flipv 
	mv flipv$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
