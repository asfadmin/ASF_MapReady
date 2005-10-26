# Makefile for          :  asf_proj2slant_range 
# Module Author         :  Rudi Gens
# Combined the functionality of some established InSAR procedure into one tool

include ../../make_support/system_rules

OBJS  = asf_proj2slant_range.o \
	create_dem_grid.o \
	fit_poly.o \
	reskew_dem.o

LIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_reporting.a

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

all: asf_proj2slant_range
	-rm *.o

asf_proj2slant_range: $(OBJS) $(LIBS)
	${CC} ${CFLAGS} -o asf_proj2slant_range $(OBJS) $(LIBS) -lm
	mv asf_proj2slant_range$(BIN_POSTFIX) $(BINDIR)

clean:
	-rm -f core *.o





