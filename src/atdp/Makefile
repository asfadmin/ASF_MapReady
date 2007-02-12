######################################################################
# Makefile for 		:  atdp.c 
# Module Author 	:  Rudi Gens
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm 

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS)

OBJS = \
	atdp_setup.o \
        read_signal.o \
        parse_cla.o \
        atdp.o

# Filter out the ISOC99 FLAGS which causes some problems.
#CFLAGS := $(patsubst -D_ISOC99_SOURCE, , $(CFLAGS))

all: atdp clean

atdp:	$(OBJS)
	$(CC) $(CFLAGS) -o atdp $(OBJS) $(LIBS) $(LDFLAGS)
	mv atdp$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
