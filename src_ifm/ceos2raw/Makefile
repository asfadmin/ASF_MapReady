#	ceos2raw
#	by Orion Lawlor, ASF, 1998
#	A program to convert CEOS level-zero format data into
#       raw (unpacked, byte-oriented) data.

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -DDECODE_CEOS

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = aisp_params.o \
	aisp_write.o \
	missing.o \
	bin_state.o \
	unpack.o \
	frame.o \
	decoder_utils.o \
	ceosUtil.o \
	decoder_RSAT.o \
	aux_RSAT.o \
	decoder_ERS.o \
	aux_ERS.o \
	decoder_JRS.o \
	aux_JRS.o \
	aux_JRS_PCM.o \
	ceos2raw.o

all: ceos2raw clean

ceos2raw: $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o ceos2raw $(OBJS) ${LIBS}
	mv ceos2raw$(BIN_POSTFIX) $(BINDIR)
	cp ceos2raw.1 ../../man/cat1/

clean:
	rm -f core ${OBJS} *.o
