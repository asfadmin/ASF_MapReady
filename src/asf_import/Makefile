# asf_import
# by Rudi Gens, ASF, 2004
#    Patrick Denny, June, 2004
# A program to ingest the complete variety of CEOS data, STF data
#    as well as the external ESRI and ENVI data into ASF internal format

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
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
	createSubset.o \
	decoder_ERS.o \
	decoder_JRS.o \
	decoder_RSAT.o \
	decoder_utils.o \
	fetch_prc_stvec.o \
	frame.o \
	import_ceos.o \
	import_envi.o \
	import_esri.o \
	import_stf.o \
	least_squares.o \
	matrix.o \
	missing.o \
	sprocket_layers.o \
	unpack.o \
	utilities.o \
	utilities_ceos.o \
	utilities_stf.o

all: asf_import
	-rm *.o

asf_import: $(OBJS)
	$(CC) $(CFLAGS) -o asf_import $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_import$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
