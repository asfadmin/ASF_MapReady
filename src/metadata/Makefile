# Makefile for:   metadata
# Module Author:  Tom Logan & Mike Shindle
#   modified by:  Patrick Denny
# Module Version: 1.2

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-lm

OBJLIB  = prn_facdr.o \
	prn_raddr.o \
	prn_ifiledr.o \
	prn_mpdr.o \
	prn_dssr.o \
	prn_dhr.o \
	prn_ppdr.o \
	prn_atdr.o \
	prn_rsr.o \
	prn_dqsr.o \
	prn_fdr.o \
	prn_ampr.o \
	prn_esa_facdr.o \
	prn_ppr.o \
	prn_rcdr.o \
	prn_shr.o \
	prn_ardr.o \
	metadata.o

all: lib exe clean

lib: ${OBJLIB}
	ar r libasf_metadata.a $(OBJLIB)
	$(RANLIB) libasf_metadata.a
	mv libasf_metadata.a $(LIBDIR)
	cp metadisplay.h $(ASF_INCLUDE_DIR)

exe: metadata

metadata: metadata_main.c lib
	$(CC) $(CFLAGS) -o metadata metadata_main.c $(LIBDIR)/libasf_metadata.a $(LIBS)
	mv  metadata$(BIN_POSTFIX) $(BINDIR)
	cp metadata.man ../../man/cat1/metadata.1 

clean:
	rm -f *.o core
