# Makefile for		: create_dem_grid

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm
OBJS = create_dem_grid.o

CFLAGS += -Wall

all: create_dem_grid
	rm *.o

create_dem_grid: $(OBJS)
	$(CC) $(CFLAGS) -o create_dem_grid $(OBJS) $(LIBS) $(LDFLAGS)
	mv create_dem_grid$(BIN_POSTFIX) $(BINDIR)
	cp create_dem_grid.1 ../../man/cat1

clean:
	rm -f core ${OBJS} *.o
