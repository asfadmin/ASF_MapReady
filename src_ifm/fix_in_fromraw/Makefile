#
#
#
#

include ../../make_support/system_rules

CFLAGS	= $(FLAGS) -DDECODE_CEOS
LIBS	= $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a \
	  -lm

OBJLIB	= fix_defs.o

all:	fix_in_fromraw clean

fix_in_fromraw: fix_in_fromraw.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o fix_in_fromraw fix_in_fromraw.c $(OBJLIB) ${LIBS}
	mv fix_in_fromraw $(BINDIR)

clean:
	rm -f core ${OBJS} *.o
	cp fix_in_fromraw.1 ../../man/cat1/
