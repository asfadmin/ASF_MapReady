#	reskew_dem
#	by Orion Lawlor, ASF, 1997
#	Reskew will remap a ground range DEM into a slant-range
#       DEM.

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	-lm

#	$(LIBDIR)/asf_las.a

OBJS=\
     dem_gr2sr.o \
     reskew_dem.o

all: reskew_dem
	rm *.o

reskew_dem:	$(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o reskew_dem $(OBJS) ${LIBS}
	mv reskew_dem$(BIN_POSTFIX) $(BINDIR)
	cp reskew_dem.man ../../man/cat1/reskew_dem.1

clean:
	rm -f core ${OBJS} *.o
