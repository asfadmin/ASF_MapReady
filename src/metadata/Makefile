CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
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
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(W_ERROR)

all: exe clean

exe: metadata dumpCeosRecords dumpLineHeader

metadata: metadata_main.c help.c
	$(CC) $(CFLAGS) -o metadata metadata_main.c help.c $(LIBDIR)/libasf_metadata.a $(LIBS)
	mv  metadata$(BIN_POSTFIX) $(BINDIR)
	cp metadata.man ../../man/cat1/metadata.1 

dumpCeosRecords: dumpCeosRecords.c
	$(CC) $(CFLAGS) -o dumpCeosRecords dumpCeosRecords.c $(LIBDIR)/libasf_metadata.a $(LIBS)
	mv dumpCeosRecords$(BIN_POSTFIX) $(BINDIR)

dumpLineHeader: dumpLineHeader.c
	$(CC) $(CFLAGS) -o dumpLineHeader dumpLineHeader.c $(LIBDIR)/libasf_metadata.a $(LIBS)
	mv dumpLineHeader$(BIN_POSTFIX) $(BINDIR)
	mkdir -p $(SHAREDIR)/metadata
	cp alos_processed_data.rec alos_signal_data.rec \
	asf_processed_data.rec asf_signal_data.rec \
	rsat_processed_data.rec rsat_signal_data.rec $(SHAREDIR)/metadata

clean:
	rm -f *.o core
