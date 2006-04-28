# Makefile for:   metadata
# Module Author:  Tom Logan & Mike Shindle
#   modified by:  Patrick Denny
# Module Version: 1.2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_reporting.a \
	$(GSL_LIBS) \
	-lm

OBJS  = prn_facdr.o \
	prn_raddr.o \
	prn_ifiledr.o \
	prn_mpdr.o \
	prn_dssr.o \
	prn_dhr.o \
	prn_ppdr.o \
	prn_atdr.o \
	prn_rsr.o \
	prn_dqsr.o \
	prn_fdr.o \
	metadata.o

all: metadata
	rm *.o
 
metadata: $(OBJS)
	$(CC) $(CFLAGS) -o metadata $(OBJS) $(LIBS) $(LDFLAGS)
	mv  metadata$(BIN_POSTFIX) $(BINDIR)
	cp metadata.man ../../man/cat1/metadata.1 

clean:
	rm -f *.o core
