#	swath_offset
#	by Dave Koster, ASF, 2000
#	A program to determine the area of swath coincedence
#       for processing with aisp.

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a \
	  -lm

OBJLIB	= lzFetch.o swath_offset_inc.o \
	  swath_bound.o

all:    swath_offset clean

swath_offset:	swath_offset.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o swath_offset swath_offset.c $(OBJLIB) ${LIBS}
	mv swath_offset $(BINDIR)
	cp swath_offset.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
