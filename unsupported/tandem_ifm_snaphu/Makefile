# Makefile for 		:  tandem_ifm.c 
# Module Author 	:  Tom Logan
# Module Version	:  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a

OBJLIB = check_refinement.o \
	 tandem_ifm_snaphu.o
OBJLIB2 = check_refinement.o \
	 tandem_ifm_snaphu_v2.o

# all: tandem_ifm_snaphu zeroify

tandem_ifm_snaphu: $(OBJLIB)
	$(CC) $(CFLAGS) -o tandem_ifm_snaphu $(OBJLIB) $(LIBS)
	mv tandem_ifm_snaphu$(BIN_POSTFIX) $(BINDIR)
	rm *.o

tandem_ifm_snaphu_v2: $(OBJLIB2)
	$(CC) $(CFLAGS) -o tandem_ifm_snaphu_v2 $(OBJLIB2) $(LIBS)
	mv tandem_ifm_snaphu_v2$(BIN_POSTFIX) $(BINDIR)
	rm *.o
#	cp *.1 ../../man/cat1

# zeroify: zeroify.o
#	$(CC) $(CFLAGS) -o zeroify zeroify.o $(LIBS)
