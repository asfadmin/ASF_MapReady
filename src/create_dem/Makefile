#	create_akdem
#	by Rudi Gens, ASF, 2001

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -DDECODE_CEOS
LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	-lm

OBJS  = config.o \
	functions.o \
	check_refinement.o

all:    create_dem clean

create_dem: create_dem.c $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o create_dem create_dem.c $(OBJS) ${LIBS}
	mv create_dem $(BINDIR)
#	cp create_dem.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
