#	asf_convert
#	by Rudi Gens, ASF, 2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_reporting.a

OBJS  = asf_convert.o \
	config.o \
	functions.o

all:    asf_convert clean

asf_convert: $(OBJS)
	$(CC) $(CFLAGS) -o asf_convert $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_convert$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
