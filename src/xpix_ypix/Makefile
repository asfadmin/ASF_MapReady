# Makefile for		: dem2phase
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
        $(LIBDIR)/asf_geolib.a \
        $(LIBDIR)/asf.a \
	-lm

OBJS = main.o

all:	xpix_ypix
	rm *.o

xpix_ypix: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o $@ $(OBJS) $(LIBS)
	mv $@$(BIN_POSTFIX) $(BINDIR)
	#cp demIFM.1 ../../man/cat1

clean:
	rm -f core ${OBJS} *.o
