CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for:   data_qc

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	-lm

all: data_qc

data_qc: data_qc.c
	$(CC) $(CFLAGS) -o data_qc data_qc.c $(LIBS)
	mv  data_qc$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
