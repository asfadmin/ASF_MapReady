# Makefile for		: ifm2ppm 
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_las.a  $(LIBDIR)/asf.a $(LIBDIR)/libjpeg.a -lm
COBJS = las2jpeg.o \
       colortable.o  \
       code_jpeg.o 

DOBJS =  jpeg2las.o decode_jpeg.o 

all: las2jpeg jpeg2las
	rm *.o

las2jpeg: $(COBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o las2jpeg $(COBJS) $(LIBS)
	mv las2jpeg$(BIN_POSTFIX) $(BINDIR)
	cp las2jpeg.1 ../../man/man1

jpeg2las: $(DOBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o jpeg2las $(DOBJS) $(LIBS)
	mv jpeg2las$(BIN_POSTFIX) $(BINDIR)
	cp jpeg2las.1 ../../man/man1


