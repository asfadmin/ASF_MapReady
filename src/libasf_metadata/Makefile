CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for:   metadata
# Module Author:  Tom Logan & Mike Shindle
#   modified by:  Patrick Denny
# Module Version: 1.2

include ../../make_support/system_rules

CFLAGS += $(W_ERROR)

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
	prn_jaxa_facdr.o \
	prn_ppr.o \
	prn_rcdr.o \
	prn_shr.o \
	prn_ardr.o \
	prn_rsi_raddr.o \
	prn_tfdr.o \
	metadata.o

all: lib clean

lib: ${OBJLIB}
	ar r libasf_metadata.a $(OBJLIB)
	$(RANLIB) libasf_metadata.a
	mv libasf_metadata.a $(LIBDIR)
	cp metadisplay.h $(ASF_INCLUDE_DIR)

clean:
	rm -f *.o core
