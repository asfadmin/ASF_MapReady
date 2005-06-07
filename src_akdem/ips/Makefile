#	ips
#	by Rudi Gens, ASF, 2001

DEBUG_BUILD = 2

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS -g
LIBS    = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a -lm

OBJLIB	= config.o \
	functions.o \
	check_refinement.o

all:    ips clean

ips: ips.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o ips ips.c $(OBJLIB) ${LIBS}
	mv ips $(BINDIR)
#	cp ips.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
