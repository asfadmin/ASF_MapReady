#	asf_convert
#	by Rudi Gens, ASF, 2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a

OBJS  = config.o \
	functions.o \
	asf_convert.o

all:    asf_convert clean

asf_convert: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o asf_convert $(OBJS) $(LIBS)
	mv asf_convert $(BINDIR)
#	cp asf_convert.1 ../../man/cat1

clean:
	rm -f core $(OBJS) *.o
