PROG = write_kml

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-lz

OBJS  = $(PROG).o

all: $(PROG)
	-rm *.o

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(PROG)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
