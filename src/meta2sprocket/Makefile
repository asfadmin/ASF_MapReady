# Makefile by Patrick Denny, March 2005

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = meta2sprocket.o

all: meta2sprocket
	-rm *.o

meta2sprocket: $(OBJS)
	$(CC) $(CFLAGS) -o meta2sprocket $(OBJS) $(LIBS) $(LDFLAGS)
	mv meta2sprocket$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
