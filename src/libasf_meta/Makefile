# Makefile for libasf_meta

include ../../make_support/system_rules

OBJS = 	asf_meta1x_to_meta09.o \
	asf_meta_init.o \
	asf_meta_read.o \
	asf_meta_read09.o \
	asf_meta_read1x.o \
	asf_meta_write.o \
	asf_meta_write09.o \
	asf_meta_write1x.o \
	las.o \
	earth_radius2datum.o \
	extract_meta_parameters.o \
	meta_fetch.o


all: libasf_meta test

libasf_meta: build_only
	mv libasf_meta.a $(LIBDIR)

build_only: $(OBJS)
	rm -rf libasf_meta.a
	ar r libasf_meta.a $(OBJS)
	$(RANLIB) libasf_meta.a

# Rebuild everything if Makefile or any ASF header has changed
$(OBJS): Makefile $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) test_meta

distclean:
	rm -f core *~ TAGS gdb_init.com

# Build tester program
test: build_only
	$(CC) $(CFLAGS) test_meta.c -o test_meta $(LIBDIR)/libasf_meta.a \
		$(LIBDIR)/asf.a -lm
