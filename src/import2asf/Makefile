# import2asf
# by Rudi Gens, ASF, 2004
#    Patrick Denny, 2004
# A program to ingest the complete variety of CEOS data, STF data
#    as well as the external ESRI and ENVI data into ASF internal format

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -DDECODE_CEOS

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = cal_params.o ceos2raw.o \
	aisp_params.o aisp_write.o missing.o \
	bin_state.o unpack.o frame.o \
	decoder_utils.o ceosUtil.o \
	decoder_RSAT.o aux_RSAT.o \
	decoder_ERS.o aux_ERS.o \
	decoder_JRS.o aux_JRS.o aux_JRS_PCM.o \
	createSubset.o fetch_prc_stvec.o stf2raw.o \
	matrix.o least_squares.o \
	sprocket_layers.o \
	import2asf.o

all: import2asf
	-rm *.o

import2asf: $(OBJS) import2asf.c
	$(CC) $(CFLAGS) -o import2asf $(OBJS) $(LIBS) $(LDFLAGS)
	mv import2asf$(BIN_POSTFIX) $(BINDIR)
#	cp import2asf.1 ../../man/cat1/

clean:
	rm -f core $(OBJS) *.o
