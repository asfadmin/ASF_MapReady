#	create_akdem
#	by Rudi Gens, ASF, 2001

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a -lm

OBJLIB	= config.o \
	functions.o \
	swath_bound.o \
	check_refinement.o

all:    create_dem clean

create_dem: create_dem.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o create_dem create_dem.c $(OBJLIB) ${LIBS}
	mv create_dem $(BINDIR)
#	cp create_dem.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
