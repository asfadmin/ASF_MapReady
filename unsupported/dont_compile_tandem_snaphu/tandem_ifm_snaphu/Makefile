# Makefile for 		:  tandem_ifm.c 
# Module Author 	:  Tom Logan
# Module Version	:  2.0

include ../../make_support/system_rules

LIBS =$(LIBDIR)/asf.a $(LIBDIR)/asf_las.a -lm

OBJLIB =	 check_refinement.o \
	 tandem_ifm_snaphu.o

all: tandem_ifm_snaphu zeroify
	rm *.o
	cp *.1 ../../man/cat1

tandem_ifm_snaphu: $(OBJLIB)
	$(CC) $(CFLAGS) -o tandem_ifm_snaphu $(OBJLIB) $(LIBS)
	mv tandem_ifm_snaphu$(BIN_POSTFIX) $(BINDIR)
	cp make_demargfile $(BINDIR)

zeroify: zeroify.o
	$(CC) $(CFLAGS) -o zeroify zeroify.o $(LIBS)
