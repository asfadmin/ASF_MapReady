# import2asf
# by Rudi Gens, ASF, 2004
#    Patrick Denny, 2004
# A program to ingest the complete variety of CEOS data, STF data
#    as well as the external ESRI and ENVI data into ASF internal format

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -DDECODE_CEOS

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = aisp_params.o \
	aisp_write.o \
	asf_import.o \
	aux_ERS.o \
	aux_JRS.o \
	aux_JRS_PCM.o \
	aux_RSAT.o \
	bin_state.o \
	cal_params.o \
	ceos2raw.o \
	ceosUtil.o \
	createSubset.o \
	decoder_ERS.o \
	decoder_JRS.o \
	decoder_RSAT.o \
	decoder_utils.o \
	fetch_prc_stvec.o \
	frame.o \
	least_squares.o \
	matrix.o \
	missing.o \
	sprocket_layers.o \
	stf2raw.o \
	unpack.o

all: asf_import
	-rm *.o

asf_import: $(OBJS) asf_import.c
	$(CC) $(CFLAGS) -o asf_import $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_import$(BIN_POSTFIX) $(BINDIR)
#	cp asf_import.1 ../../man/cat1/

clean:
	rm -f core $(OBJS) *.o
