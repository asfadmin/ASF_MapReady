include ../../make_support/system_rules

LIBS :=	\
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(XML_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS)

OBJS  = flip.o

all: flip
	rm *.o

flip:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) $(LDFLAGS) -o flip
	mv flip$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
