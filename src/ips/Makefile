#	ips
#	by Rudi Gens, ASF, 2001


include ../../make_support/system_rules

LIBS    = $(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
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
