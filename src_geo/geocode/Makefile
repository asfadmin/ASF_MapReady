# Makefile for          :  new_geo 
# Module Author         :  Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_lt.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = geoLatLon.o \
	geocode.o \
	proj.o \
	proj_grid.o \
	proj_prm.o \
	proj_win.o \
	projectGeo.o

all: geocode
	-rm *.o

geocode: $(OBJS)
	$(CC) $(CFLAGS) -o geocode $(OBJS) $(LIBS) $(LDFLAGS)
	mv geocode$(BIN_POSTFIX) $(BINDIR)
	cp geocode.1 ../../man/cat1

#projectGeo: $(POBJS)
#	$(CC) $(CFLAGS) -o projectGeo $(POBJS) $(LIBS) $(LDFLAGS)
#	mv projectGeo$(BIN_POSTFIX) $(BINDIR)
#	cp projectGeo.1 ../../man/cat1

clean:
	-rm -f core *.o





