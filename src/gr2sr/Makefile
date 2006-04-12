# Makefile for gr2sr

include ../../make_support/system_rules

OBJS  = gr2sr_vec.o \
	gr2ml_vec.o \
	gr2sr.o

REQUIRED_LIBS = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	-lm
CEDAR_LIBS = $(LIBDIR)/asf_las.a
LIBS = \
	$(shell \
	if [ `hostname` = "cedar" ] ; then \
		echo $(REQUIRED_LIBS) $(CEDAR_LIBS) ; \
	else \
		echo $(REQUIRED_LIBS) ; \
	fi \
	)

all: gr2sr clean

gr2sr: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o gr2sr $(OBJS) $(LIBS)
	mv gr2sr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
