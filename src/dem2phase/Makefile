# Makefile for		: dem2phase
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS = dem2phase.o

all: dem2phase
	rm *.o

dem2phase: $(OBJS)
	$(CC) $(CFLAGS) -o dem2phase $(OBJS) $(LIBS) $(LDFLAGS)
	mv dem2phase$(BIN_POSTFIX) $(BINDIR)
	cp dem2phase.1 ../../man/man1

clean:
	rm -f core ${OBJS} *.o
