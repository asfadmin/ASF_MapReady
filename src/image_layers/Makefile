#	image_layers
#	by Rudi Gens, ASF, 2003
#	A program to ingest the complete variety of CEOS data
#       into ASF internal format

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS -Wall -O3
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a $(LIBDIR)/asf_las.a -lm
OBJS	= lzFetch.o least_squares.o matrix.o
#OBJS= lzFetch.o aisp_params.o aisp_write.o missing.o \
#	propagate.o bin_state.o unpack.o frame.o \
#	decoder_utils.o ceosUtil.o \
#	decoder_RSAT.o aux_RSAT.o \
#	decoder_ERS.o aux_ERS.o \
#	decoder_JRS.o aux_JRS.o aux_JRS_PCM.o

all: image_layers image_layers_org
	-rm *.o

image_layers:	$(OBJS) image_layers.c
	${CC} ${CFLAGS} ${LDFLAGS} -o image_layers image_layers.c $(OBJS) ${LIBS}
	mv image_layers$(BIN_POSTFIX) $(BINDIR)
#	cp image_layers.1 ../../man/cat1/

image_layers_org:	$(OBJS) image_layers_org.c
	${CC} ${CFLAGS} ${LDFLAGS} -o image_layers_org image_layers_org.c $(OBJS) ${LIBS}
	mv image_layers_org$(BIN_POSTFIX) $(BINDIR)
#	cp image_layers.1 ../../man/cat1/

clean:
	rm -f core ${OBJS} *.o
