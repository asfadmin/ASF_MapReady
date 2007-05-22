# Makefile for deskew

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	-lm

CFLAGS += $(GLIB_CFLAGS)
OBJS  = deskew.o 

all: deskew clean

deskew: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o deskew $(OBJS) $(LIBS) $(LDFLAGS)
	mv deskew$(BIN_POSTFIX) $(BINDIR)
	cp deskew.1 ../../man/cat1/deskew.1

clean:
	rm -f *.o
