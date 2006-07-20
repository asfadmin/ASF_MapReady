include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS)

LIBC = $(LIBS) -lm 

OBJS = ardop.o parse_cla.o

all: ardop calc_deltas

calc_deltas: calc_deltas.o
	$(CC) $(CFLAGS) -o calc_deltas calc_deltas.o $(LIBC) $(LDFLAGS)
	mv calc_deltas$(BIN_POSTFIX) $(BINDIR)
	cp calc_deltas.1 ../../man/cat1/ardop.1

ardop:	$(OBJS)
	$(CC) $(CFLAGS) -o ardop $(OBJS) $(LIBC) $(LDFLAGS)
	mv ardop$(BIN_POSTFIX) $(BINDIR)
	cp ardop.1 ../../man/cat1/ardop.1 

clean:
	rm -f core *.o
