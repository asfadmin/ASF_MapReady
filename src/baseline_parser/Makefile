# Makefile for          :  baseline_parser
# Module Author         :  Rudi Gens 

DEBUG_BUILD = 0

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a

OBJS = 	baseline_parser.o
SOBJS = shape_parser.o

all: baseline_parser shape_parser

baseline_parser: $(OBJS)
	$(CC) -o baseline_parser $(OBJS) $(LIBS)
	rm *.o
	cp baseline_parser$(BIN_POSTFIX) $(BINDIR)

shape_parser: $(SOBJS)
	$(CC) -o shape_parser $(SOBJS) $(LIBS)
	rm *.o
	cp shape_parser$(BIN_POSTFIX) $(BINDIR)


