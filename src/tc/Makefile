include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = tc.o

all: tc
	-rm *.o

tc: $(OBJS)
	$(CC) $(CFLAGS) -o tc $(OBJS) $(LIBS) $(LDFLAGS)
	mv tc$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
