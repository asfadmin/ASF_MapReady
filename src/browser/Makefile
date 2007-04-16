include ../../make_support/system_rules

SRCS = build_cache.c preview.c

CFLAGS += -Wall -Werror \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(GTK_CFLAGS) \
	$(shell /usr/bin/pkg-config --cflags libglade-2.0) \
	-D_XOPEN_SOURCE=600

LDLIBS = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(GTK_LIBS) \
	$(shell /usr/bin/pkg-config --libs libglade-2.0)

LDFLAGS = -Wl,--export-dynamic

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	$(CC) build_cache.c $(CFLAGS) $(LDLIBS) $(LDFLAGS) -o build_cache
	mv build_cache$(BIN_POSTFIX) $(BINDIR)
	$(CC) preview.c $(CFLAGS) $(LDLIBS) $(LDFLAGS) -o preview
	mv preview$(BIN_POSTFIX) $(BINDIR)
	cp preview.glade $(SHAREDIR)
	rm -f $(OBJS)

clean:
	rm -f core* $(OBJS)
