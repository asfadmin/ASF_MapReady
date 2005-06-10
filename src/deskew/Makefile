# Makefile for deskew

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS  = deskew.o 

all: deskew clean

deskew: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o deskew $(OBJS) $(LIBS) $(LDFLAGS)
	mv deskew$(BIN_POSTFIX) $(BINDIR)
	cp deskew.1 ../../man/cat1/deskew.1

clean:
	rm *.o
