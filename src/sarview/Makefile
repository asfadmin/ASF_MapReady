# Makefile for		: SARview tcl/tk
# Module Author		: Orion Lawlor
# Makefile Date		: 5/99
# Updated		: 6/01, P. Denny - include tcltk package

include ../../make_support/system_rules

LDFLAGS := -L$(XHOME) $(LDFLAGS)

 # ldl, lsocket, and lnsl are necessary for Solaris, but break sarview on irix
SOLARIS_LIBS = -ldl -lsocket -lnsl
REQUIRED_LIBS = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	$(TCL_TK_LIBS) \
	-lX11 -lm
LIBS = \
	$(shell \
	if [ "$(SYS)" = "solaris" ] ; then \
		echo $(REQUIRED_LIBS) $(SOLARIS_LIBS) ; \
	else \
		echo $(REQUIRED_LIBS) ; \
	fi \
	)

OBJS = \
	cproc.o cproc_desc.o cproc_hist.o \
	hist.o sel.o \
	image.o image_desc.o image_out.o image_pix.o \
	jpeg_out.o \
	poly.o \
	tkAppInit.o tkCommand.o

CFLAGS += $(GSL_CFLAGS) $(JPEG_CFLAGS) $(TCL_TK_CFLAGS) $(GLIB_CFLAGS)

all: sarview clean

# if system is solaris include extra libraries
sarview: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o sarview_bin $(OBJS) $(LIBS)
	mv sarview_bin$(BIN_POSTFIX) $(BINDIR)
	cp sarview.sh $(BINDIR)/sarview
	chmod 775 $(BINDIR)/sarview
	-rm -rf $(BINDIR)/support
	-cp -r ./support $(BINDIR)

clean:
	rm -f $(OBJS)
