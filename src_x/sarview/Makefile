# Makefile for		: SARview tcl/tk
# Module Author		: Orion Lawlor
# Makefile Date		: 5/99
# Updated		: 6/01, P. Denny - include tcltk package

include ../../make_support/system_rules

LIBDIR = ../../lib/$(SYS)
CFLAGS = -I$(LIBDIR)/tcltk81/include $(FLAGS) -g
LFLAGS = -L$(LIBDIR) -L$(LIBDIR)/tcltk81/lib

 # ldl, lsocket, and lnsl are necessary for Solaris, but break sarview on irix
OTHER = -ldl -lsocket -lnsl
LIBS =	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/libjpeg.a \
	-ltcl8.1 -ltk8.1 -lX11 -lm

OBJS = \
	cproc.o cproc_desc.o cproc_hist.o \
	hist.o sel.o \
	image.o image_desc.o image_out.o image_pix.o \
	jpeg_out.o \
	poly.o \
	tkAppInit.o tkCommand.o

all: tcltk sarview
	rm *.o

tcltk:
	./make_tcltk.sh

# if system is solaris include extra libraries; test looks wrong, but it's right
sarview: $(OBJS)
	test $(SYS) != solaris || $(CC) $(CFLAGS) $(LFLAGS) -o sarview_bin $(OBJS) $(LIBS) $(OTHER)
	test $(SYS) = solaris || $(CC) $(CFLAGS) $(LFLAGS) -o sarview_bin $(OBJS) $(LIBS)
	mv sarview_bin ../../bin/$(SYS)/sarview_bin
	cp sarview.sh ../../bin/$(SYS)/sarview
	-rm -r ../../bin/$(SYS)/support
	-cp -r ./support ../../bin/$(SYS)/
