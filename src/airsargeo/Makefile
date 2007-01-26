# Makefile for          :  airsargeo
# Module Author         :  Rudi Gens

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_lt.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
        -lm

OBJS = airsargeo.o
POBJS = proj_pix.o proj_win.o proj_prm.o proj.o proj_grid.o projectGeo.o

all: projectGeo airsargeo clean

projectGeo: $(POBJS)
	$(CC) $(CFLAGS) -o projectGeo $(POBJS) $(LIBS)
	mv projectGeo$(BIN_POSTFIX) $(BINDIR)
	cp projectGeo.1 ../../man/cat1

airsargeo: $(OBJS)
	$(CC) $(CFLAGS) -o airsargeo $(OBJS) $(LIBS)
	mv airsargeo$(BIN_POSTFIX) $(BINDIR)
	cp airsargeo.1 ../../man/cat1
	cp geocode_airsar.sh $(BINDIR)/geocode_airsar
	cp geocode_airsar.1 ../../man/cat1

clean:	
	rm -f core *.o
