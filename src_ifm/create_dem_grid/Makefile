# Makefile for		: create_dem_grid

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	-lm
OBJS = create_dem_grid.o

all: create_dem_grid
	rm *.o

create_dem_grid: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o create_dem_grid $(OBJS) $(LIBS)
	mv create_dem_grid$(BIN_POSTFIX) $(BINDIR)
	cp create_dem_grid.1 ../../man/cat1

clean:
	rm -f core ${OBJS} *.o
