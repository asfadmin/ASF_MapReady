#	lz_scan
#	by Orion Lawlor, ASF, 1998
#	A program to convert ASF level-zero format data into
#       raw (unpacked, byte-oriented) data.

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a -lm
OBJS= lzFetch.o aisp_params.o aisp_write.o missing.o \
	propagate.o bin_state.o unpack.o frame.o \
	decoder_utils.o ceosUtil.o \
	decoder_RSAT.o aux_RSAT.o \
	decoder_ERS.o aux_ERS.o \
	decoder_JRS.o aux_JRS.o aux_JRS_PCM.o

#all: lz_scan ceos2raw
#	-rm *.o

lz_scan:	$(OBJS) lz_scan.c
	${CC} ${CFLAGS} ${LDFLAGS} -o lz_scan lz_scan.c $(OBJS) ${LIBS}
	mv lz_scan$(BIN_POSTFIX) $(BINDIR)
	-rm *.o
#	cp lz_scan.1 ../../man/cat1/

ceos2raw:	$(OBJS) ceos2raw.c
	${CC} ${CFLAGS} ${LDFLAGS} -o ceos2raw ceos2raw.c $(OBJS) ${LIBS}
	mv ceos2raw$(BIN_POSTFIX) $(BINDIR)
	cp ceos2raw.1 ../../man/cat1/

clean:
	rm -f core ${OBJS} *.o
