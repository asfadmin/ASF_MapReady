# Makefile for		: las2ppm 
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = las2ppm.o \
	colortable.o 

las2ppm: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o las2ppm $(OBJS) $(LIBS)
	rm *.o
	mv las2ppm$(BIN_POSTFIX) $(BINDIR)
	cp las2ppm.1 ../../man/man1
