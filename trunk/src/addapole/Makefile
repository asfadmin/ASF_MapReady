include ../../make_support/system_rules

CFLAGS += $(GLIBS_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libshp.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = addapole.o

all: addapole
	-rm *.o

addapole: $(OBJS)
	$(CC) $(CFLAGS) -o addapole $(OBJS) $(LIBS) $(LDFLAGS)
	mv addapole$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o

