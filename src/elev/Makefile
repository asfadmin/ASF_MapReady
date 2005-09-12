# Makefile for		: elev 
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm
OBJS  = elev.o

all: elev clean

elev: $(OBJS)
	$(CC) $(CFLAGS) -o elev $(OBJS) $(LIBS) $(LDFLAGS)
	mv elev$(BIN_POSTFIX) $(BINDIR)
	cp elev.1 ../../man/man1

clean:
	rm *.o
