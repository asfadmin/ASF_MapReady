#	ceos2asf
#	by Rudi Gens, ASF, 2003
#	A program to ingest the complete variety of CEOS data
#       into ASF internal format

include ../../make_support/system_rules

CFLAGS  := $(CFLAGS) -DDECODE_CEOS

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = cal_params.o ceos2raw.o \
	aisp_params.o aisp_write.o missing.o \
	bin_state.o unpack.o frame.o \
	decoder_utils.o ceosUtil.o \
	decoder_RSAT.o aux_RSAT.o \
	decoder_ERS.o aux_ERS.o \
	decoder_JRS.o aux_JRS.o aux_JRS_PCM.o\
	ceos2asf.o

all: ceos2asf
#	-rm $(OBJS)

ceos2asf: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o ceos2asf $(OBJS) $(LIBS)
	mv ceos2asf$(BIN_POSTFIX) $(BINDIR)
#	cp ceos2asf.1 ../../man/cat1/

clean:
	rm -f core $(OBJS) *.o
