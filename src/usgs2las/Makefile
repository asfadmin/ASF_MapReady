# Makefile for: usgs2las
# Makefile author: Orion Lawlor
# Makefile date: 5/24/99

include ../../make_support/system_rules
LIBS=$(LIBDIR)/asf.a $(LIBDIR)/asf_las.a 

OBJS = \
        usgs2las.o

all: usgs2las usgs_ingest clean

usgs_ingest:
	cp usgs_ingest.sh $(BINDIR)/usgs_ingest
	-chmod 775 $(BINDIR)/usgs_ingest
	cp usgs_ingest.1 ../../man/cat1

usgs2las: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o usgs2las $(LIBS) -lm
	mv usgs2las$(BIN_POSTFIX) $(BINDIR)
	cp usgs2las.1 ../../man/cat1

clean:
	-rm -f *.o
