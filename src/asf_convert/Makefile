#	asf_convert
#	by Rudi Gens, ASF, 2004

DEBUG_BUILD = 2

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS -g
LIBS    = $(LIBDIR)/asf.a

OBJLIB	= config.o functions.o 

all:    asf_convert clean

asf_convert: asf_convert.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o asf_convert asf_convert.c $(OBJLIB) ${LIBS}
	mv asf_convert $(BINDIR)
#	cp asf_convert.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
