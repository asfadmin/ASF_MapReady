#	asf_convert
#	by Rudi Gens, ASF, 2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a

OBJS  = asf_convert.o \
	config.o \
	functions.o

all:    asf_convert clean

asf_convert: $(OBJS)
	$(CC) $(CFLAGS) -o asf_convert $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_convert$(BIN_POSTFIX) $(BINDIR)
	mkdir -p $(SHAREDIR)/asf_convert
	cp batch.config $(SHAREDIR)/asf_convert
	cp default_values $(SHAREDIR)/asf_convert
	cp asf_convert_cmd.pdf $(SHAREDIR)/asf_convert

clean:
	rm -f core $(OBJS) *.o
