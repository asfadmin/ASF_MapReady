#	export_ceos
#	by Rudi Gens, ASF, 2004

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS -g
LIBS    = $(LIBDIR)/asf.a

OBJLIB	= config.o functions.o 

all:    export_ceos clean

export_ceos: export_ceos.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o export_ceos export_ceos.c $(OBJLIB) ${LIBS}
	mv export_ceos $(BINDIR)
#	cp export_ceos.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
