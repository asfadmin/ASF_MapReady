# Makefile for 		:  dem_diff
# Module Author 	:  Tom Logan
# Module Version	:  1.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \

OBJS  = dem_diff.o

all: dem_diff clean

dem_diff: $(OBJS) 
	$(CC) $(CFLAGS) -o dem_diff $(OBJS) $(LIBS) -lm
	mv dem_diff$(BIN_POSTFIX) $(BINDIR)
	cp dem_diff.1 ../../man/cat1/

clean:
	-rm *.o
