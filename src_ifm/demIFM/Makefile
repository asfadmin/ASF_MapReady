# Makefile for		: dem2phase
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS_C = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
        $(LIBDIR)/asf_geolib.a \
        $(LIBDIR)/asf.a \
	-lm
LIBS_P = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	-lm
LIBS  = $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS_C = create_dem_grid.o
OBJS_P = dem2phase.o
OBJS_S= dem2seeds.o
OBJS_D= demIFM.o

all:	demIFM create_dem_grid dem2phase dem2seeds
	rm *.o

demIFM: $(OBJS_D)
	$(CC) $(CFLAGS) $(LFLAGS) -o demIFM $(OBJS_D) $(LIBS)
	mv demIFM$(BIN_POSTFIX) $(BINDIR)
	cp demIFM.1 ../../man/cat1

create_dem_grid: $(OBJS_C)
	$(CC) $(CFLAGS) $(LFLAGS) -o create_dem_grid $(OBJS_C) $(LIBS_C)
	mv create_dem_grid$(BIN_POSTFIX) $(BINDIR)
	cp create_dem_grid.1 ../../man/cat1

dem2phase: $(OBJS_P)
	$(CC) $(CFLAGS) -o dem2phase $(OBJS_P) $(LIBS_P)
	mv dem2phase$(BIN_POSTFIX) $(BINDIR)
	cp dem2phase.1 ../../man/man1

dem2seeds: $(OBJS_S)
	${CC} ${CFLAGS} -o dem2seeds $(OBJS_S) ${LIBS}
	mv dem2seeds$(BIN_POSTFIX) $(BINDIR)
	cp dem2seeds.1 ../../man/cat1

clean:
	rm -f core ${OBJS} *.o
