# Makefile for to_sr

include ../../make_support/system_rules

OBJS  = to_sr.o

REQUIRED_LIBS = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(JPEG_LIBS)

CEDAR_LIBS = $(LIBDIR)/asf_las.a
LIBS = \
	$(shell \
	if [ `hostname` = "cedar" ] ; then \
		echo $(REQUIRED_LIBS) $(CEDAR_LIBS) ; \
	else \
		echo $(REQUIRED_LIBS) ; \
	fi \
	)

all: to_sr clean

to_sr: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o to_sr $(OBJS) $(LIBS)
	mv to_sr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
