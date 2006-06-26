# Makefile for:   leader2pdf
# Module Author:  Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
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
	leader2pdf.o

all: metadata
	rm *.o
 
metadata: $(OBJS)
	$(CC) $(CFLAGS) -o leader2pdf $(OBJS) $(LIBS) $(LDFLAGS)
	mv  leader2pdf$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
