# Makefile for  : convert2ppm 
# Module Author : Mike Shindle, 11/1995
#                 Patrick Denny, 2/2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = convert2ppm.o \
	colortable.o 

convert2ppm: $(OBJS)
	$(CC) $(CFLAGS) -o convert2ppm $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv convert2ppm$(BIN_POSTFIX) $(BINDIR)
	cp convert2ppm.1 ../../man/man1
