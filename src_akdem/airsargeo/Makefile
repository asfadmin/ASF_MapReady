# Makefile for          :  airsargeo
# Module Author         :  Rudi Gens

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf_lt.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libifm.a
LIBS = $(OLIBS) -lm

OBJS = airsargeo.o
POBJS = proj_pix.o proj_win.o proj_prm.o proj.o proj_grid.o projectGeo.o

projectGeo: $(POBJS) $(OLIBS)
	$(CC) $(CFLAGS) -o projectGeo $(POBJS) $(LIBS)
	mv projectGeo$(BIN_POSTFIX) $(BINDIR)
	cp projectGeo.1 ../../man/cat1

airsargeo: $(OBJS)
	$(CC) $(CFLAGS) -o airsargeo $(OBJS) $(LIBS)
	rm *.o
	mv airsargeo $(BIN_POSTFIX) $(BINDIR)
	cp airsargeo.1 ../../man/cat1
	cp geocode_airsar ../../bin/irix
	cp geocode_airsar.1 ../../man/cat1

clean:	
	rm -f core *.o
