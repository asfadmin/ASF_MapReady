# Makefile for		: SARview tcl/tk
# Module Author		: Orion Lawlor
# Makefile Date		: 5/99
# Updated		: 6/01, P. Denny - include tcltk package

include ../../make_support/system_rules

LDFLAGS := -L$(XHOME) $(LDFLAGS)

 # ldl, lsocket, and lnsl are necessary for Solaris, but break sarview on irix
OTHER = -ldl -lsocket -lnsl
LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/libjpeg.a \
	$(LIBDIR)/libtcl8.1.a \
	$(LIBDIR)/libtk8.1.a \
	-lX11 -lm

OBJS = \
	cproc.o cproc_desc.o cproc_hist.o \
	hist.o sel.o \
	image.o image_desc.o image_out.o image_pix.o \
	jpeg_out.o \
	poly.o \
	tkAppInit.o tkCommand.o

all: sarview clean

# if system is solaris include extra libraries
sarview: $(OBJS)
	if [ "$(SYS)" = "solaris" ] ; then \
		$(CC) $(CFLAGS) $(LDFLAGS) -o sarview_bin $(OBJS) $(LIBS) $(OTHER) ; \
	else \
		$(CC) $(CFLAGS) $(LDFLAGS) -o sarview_bin $(OBJS) $(LIBS) ; \
	fi
	mv sarview_bin$(BIN_POSTFIX) $(BINDIR)
	cp sarview.sh $(BINDIR)/sarview
	chmod 775 $(BINDIR)/sarview
	-rm -r $(BINDIR)/support
	-cp -r ./support $(BINDIR)

clean:
	rm -f $(OBJS)
