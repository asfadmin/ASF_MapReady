#	deskew_dem
#	by Orion Lawlor, ASF, 1997
#	Deskew and interpolate across areas which didn't 
#	phase unwrap, to correct an interferometric dem.

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = calc_ranges.o \
	set_ddr_corners.o \
	dem_sr2gr.o \
	dem_interp_col.o \
	geo_compensate.o \
	deskew_dem.o \
	radio_compensate.o

all: deskew_dem
	rm *.o

deskew_dem: $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o deskew_dem $(OBJS) ${LIBS}
	mv deskew_dem$(BIN_POSTFIX) $(BINDIR)
	cp deskew_dem.man ../../man/cat1/deskew_dem.1

clean:
	rm -f core ${OBJS} *.o
