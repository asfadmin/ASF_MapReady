
include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-lz

all: meta2shape meta2shape_list

meta2shape: meta2shape.o
	$(CC) $(CFLAGS) -o meta2shape meta2shape.o $(LIBS) $(LDFLAGS)
	mv meta2shape$(BIN_POSTFIX) $(BINDIR)

meta2shape_list: meta2shape_list.o
	$(CC) $(CFLAGS) -o meta2shape_list meta2shape_list.o $(LIBS) $(LDFLAGS)
	mv meta2shape_list$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
