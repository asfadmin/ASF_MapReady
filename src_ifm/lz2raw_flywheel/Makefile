#	lz2raw_flywheel
#	by Orion Lawlor, ASF, 1998
#	A program to convert ASF level-zero format data into
#       raw (unpacked, byte-oriented) data.

include ../../make_support/system_rules

CFLAGS  := $(CFLAGS) -DDECODE_CEOS

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = aisp_write.o \
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
	fetch_prc_stvec.o \
	createSubset.o

all:	lz2raw_flywheel
	-rm *.o

lz2raw_flywheel: $(OBJS) lz2raw_flywheel.c
	${CC} ${CFLAGS} ${LDFLAGS} -o lz2raw_flywheel lz2raw_flywheel.c $(OBJS) ${LIBS}
	mv lz2raw_flywheel$(BIN_POSTFIX) $(BINDIR)
	cp lz2raw_flywheel.1 ../../man/cat1/

clean:
	rm -f core ${OBJS} *.o
