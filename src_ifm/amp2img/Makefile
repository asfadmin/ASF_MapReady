# Makefile for		: amp2img
# Module Author		: Rob Fatland, Mike Shindle, Tom Logan, Patrick Denny

include ../../make_support/system_rules

LIBS=	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a -lm

AIOBJS= amp2img.o \
	get_hist.o \
	get_mean.o \
	mldata.o 

all: amp2img 

amp2img: $(AIOBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o amp2img $(AIOBJS) $(LIBS)
	mv amp2img$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp amp2img.1 ../../man/man1

