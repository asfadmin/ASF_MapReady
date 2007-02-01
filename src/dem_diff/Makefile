# Makefile for 		:  dem_diff
# Module Author 	:  Tom Logan
# Module Version	:  1.0

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS)

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

OBJS  = dem_diff.o

all: dem_diff clean

dem_diff: $(OBJS) 
	$(CC) $(CFLAGS) -o dem_diff $(OBJS) $(LIBS) -lm
	mv dem_diff$(BIN_POSTFIX) $(BINDIR)
	cp dem_diff.1 ../../man/cat1/

clean:
	-rm *.o
