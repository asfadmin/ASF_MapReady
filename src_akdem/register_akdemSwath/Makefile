#	register_akdemSwath
#	by Dave Koster, ASF, 2000
#	A program to determine the area of swath coincedence
#       for processing with aisp.

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf.a 

#	  $(LIBDIR)/asf_las.a -lm

OBJLIB	= swath_bound.o \
	  register_lzraw_inc.o \
	  aisp_params.o

all:    register_akdemSwath clean

register_akdemSwath: register_akdemSwath.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o register_akdemSwath register_akdemSwath.c $(OBJLIB) ${LIBS}
	mv register_akdemSwath $(BINDIR)
	cp register_akdemSwath.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
