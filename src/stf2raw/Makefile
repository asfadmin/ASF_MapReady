#	stf2raw
#	by Orion Lawlor, ASF, 1998
#          Patrick Denny, ASF, 2004
#	A program to convert ASF level-zero format data into
#       raw (unpacked, byte-oriented) data.

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
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

all:	stf2raw
	-rm *.o

stf2raw: $(OBJS) stf2raw.c
	${CC} ${CFLAGS} ${LDFLAGS} -o stf2raw stf2raw.c $(OBJS) ${LIBS}
	mv stf2raw$(BIN_POSTFIX) $(BINDIR)
	cp stf2raw.1 ../../man/cat1/

clean:
	rm -f core *.o
