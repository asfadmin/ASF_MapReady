#	lz2raw
#	by Orion Lawlor, ASF, 1998
#	A program to convert ASF level-zero format data into
#       raw (unpacked, byte-oriented) data.

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a -lm

all:    fix_in clean

fix_in:	fix_in.c
	${CC} ${CFLAGS} ${LDFLAGS} -o fix_in fix_in.c ${LIBS}
	mv fix_in $(BINDIR)

clean:
	rm -f core ${OBJS} *.o
