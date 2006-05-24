include ../../make_support/system_rules

CFLAGS += $(GLIBS_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = asf_terrcorr.o

all: tc
	-rm *.o

tc: $(OBJS)
	$(CC) $(CFLAGS) -o asf_terrcorr $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_terrcorr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o *tctmp*

