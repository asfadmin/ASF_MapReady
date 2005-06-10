# Makefile for : convert2jpeg
# Module Author: Mike Shindle, mm/yyyy
#                Patrick Denny, 2/2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libjpeg.a \
	-lm

COBJS = convert2jpeg.o \
	colortable.o  \
	code_jpeg.o

DOBJS = jpeg2asf.o \
	decode_jpeg.o

all: convert2jpeg clean

convert2jpeg: $(COBJS)
	$(CC) $(CFLAGS) -o convert2jpeg $(COBJS) $(LIBS) $(LDFLAGS)
	mv convert2jpeg$(BIN_POSTFIX) $(BINDIR)
	cp convert2jpeg.1 ../../man/man1

jpeg2asf: $(DOBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o jpeg2asf $(DOBJS) $(LIBS)
	mv jpeg2asf$(BIN_POSTFIX) $(BINDIR)
	cp jpeg2asf.1 ../../man/man1

clean:
	rm -f core *.o
