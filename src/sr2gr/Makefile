# Makefile for          :  sr2gr
# Module Author         :  Tom Logan 
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = sr2gr.o \
	sr2gr_vec.o 

all: sr2gr deskew ui2byte
	rm *.o

sr2gr : $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o sr2gr $(OBJS) $(LIBS)
	mv sr2gr$(BIN_POSTFIX) $(BINDIR)
	cp sr2gr.man ../../man/cat1/sr2gr.1
	@ echo "XXXXXXXXXXXXX sr2gr Compiled Successfully! XXXXXXXXXXXXXX"

deskew: deskew.o
	$(CC) $(CFLAGS) $(LFLAGS) deskew.o -o deskew $(LIBS)
	mv deskew$(BIN_POSTFIX) $(BINDIR)
	cp deskew.man ../../man/cat1/deskew.1
	@ echo "XXXXXXXXXXXXX deskew Compiled Sucessfully! XXXXXXXXXXXXX"

ui2byte: ui2byte.o
	$(CC) $(CFLAGS) $(LFLAGS) ui2byte.o -o ui2byte $(LIBS)
	mv ui2byte$(BIN_POSTFIX) $(BINDIR)
	cp ui2byte.man ../../man/cat1/ui2byte.1
	@ echo "XXXXXXXXXXXXX ui2byte Compiled Sucessfully! XXXXXXXXXXXXX"

