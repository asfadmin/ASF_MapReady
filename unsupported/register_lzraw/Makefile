#	register_lzraw
#	by Dave Koster, ASF, 2000
#	   Patrick Denny, ASF, 2001
#	A program to determine the area of swath coincedence
#       for processing with aisp.

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a 


OBJLIB	= swath_bound.o \
	  register_lzraw_inc.o

all:    register_lzraw clean

register_lzraw: register_lzraw.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o register_lzraw register_lzraw.c $(OBJLIB) ${LIBS}
	mv register_lzraw $(BINDIR)
	cp register_lzraw.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
