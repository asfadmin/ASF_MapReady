# asf_import
# by Rudi Gens, ASF, 2004
#    Patrick Denny, June, 2004
# A program to ingest the complete variety of CEOS data, STF data
#    as well as the external ESRI and ENVI data into ASF internal format

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lz

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

# These defines enable various debugging behavior and output.  Leaving
# debugging code in and available is all the rage these days, and is
# probably even a good idea.  The following stuff constitute a sort of
# catalog of the available debugging features for this program, which
# acronymish things you might choose to enable from the command line
# that invokes make (e.g. make DISSJO=nonemptystring) so you don't
# forget to remove them before release.
#
DISSJO =
ifdef DISSJO
  DEBUG_FEATURES += -DDEBUG_IMPORT_SRTM_SEAMLESS_JPEG_OUTPUT
endif
DIAUG =
ifdef DIAUG
  DEBUG_FEATURES += -DDEBUG_IMPORT_ASF_UTM_GEOTIFF_JPEG_OUTPUT
endif
CFLAGS += $(DEBUG_FEATURES)

OBJS  = ardop_params.o \
	ardop_write.o \
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
	find_geotiff_name.o \
	frame.o \
	geotiff_flavors.o \
	import_asf_utm_geotiff.o \
	import_ceos.o \
	import_envi.o \
	import_esri.o \
	import_srtm_seamless.o \
	import_stf.o \
	least_squares.o \
	matrix.o \
	missing.o \
	projected_image_import.o \
	sprocket_layers.o \
        tiff_to_float_image.o \
	unpack.o \
	utilities.o \
	utilities_ceos.o \
	utilities_stf.o \
	write_meta_and_img.o

all: asf_import
	-rm *.o

asf_import: $(OBJS)
	$(CC) $(CFLAGS) -o asf_import $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_import$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
