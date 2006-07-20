# Makefile for          :  sr2gr
# Module Author         :  Tom Logan 
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	-lm

OBJS  = sr2gr.o

all: sr2gr clean

sr2gr : $(OBJS)
	$(CC) $(CFLAGS) -o sr2gr $(OBJS) $(LIBS) $(LDFLAGS)
	mv sr2gr$(BIN_POSTFIX) $(BINDIR)
	cp sr2gr.1 ../../man/cat1

clean:
	rm *.o
