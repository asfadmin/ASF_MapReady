#	ips
#	by Rudi Gens, ASF, 2001


include ../../make_support/system_rules

LIBS    = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libgslcblas.a \
	$(LIBDIR)/asf.a -lm

OBJLIB	= config.o \
	functions.o \
	check_refinement.o

all:    ips clean

ips: ips.c $(OBJLIB)
	${CC} ${CFLAGS} ${LDFLAGS} -o ips ips.c $(OBJLIB) ${LIBS}
	cp ips $(BINDIR)
#	cp ips.1 ../../man/cat1
clean:
	rm -f core ${OBJS} *.o
