# Makefile for 		:  tandem_ifm.c 
# Module Author 	:  Tom Logan
# Module Version	:  2.0

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a $(LIBDIR)/asf_las.a -lm

OBJLIB =	 check_refinement.o \
	 tandem_ifm.o

all: tandem_ifm zeroify
	rm *.o
	cp *.1 ../../man/cat1

tandem_ifm: $(OBJLIB)
	$(CC) $(CFLAGS) -o tandem_ifm $(OBJLIB) $(LIBS)
	mv tandem_ifm$(BIN_POSTFIX) $(BINDIR)

zeroify: zeroify.o
	$(CC) $(CFLAGS) -o zeroify zeroify.o $(LIBS)
	mv zeroify$(BIN_POSTFIX) $(BINDIR)
