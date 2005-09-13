# Makefile for 		:  baseline2html
# Module Author 	:  Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a -lm

all: baseline2html month2html mode2html area2html
	rm *.o

baseline2html:  baseline2html.o
	$(CC) $(CFLAGS) $(LFLAGS) -o baseline2html baseline2html.o $(LIBS)
	mv baseline2html$(BIN_POSTFIX) $(BINDIR)

month2html:  month2html.o
	$(CC) $(CFLAGS) $(LFLAGS) -o month2html month2html.o $(LIBS)
	mv month2html$(BIN_POSTFIX) $(BINDIR)

mode2html:  mode2html.o
	$(CC) $(CFLAGS) $(LFLAGS) -o mode2html mode2html.o $(LIBS)
	mv mode2html$(BIN_POSTFIX) $(BINDIR)

area2html: area2html.o
	$(CC) $(CFLAGS) $(LFLAGS) -o area2html area2html.o $(LIBS)
	mv area2html$(BIN_POSTFIX) $(BINDIR)

