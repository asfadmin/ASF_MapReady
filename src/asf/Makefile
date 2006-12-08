# Makefile for: asf.a, our standard included library.
# Contains:
# caplib, our error-protected standard library routines.
# fileUtil, a convenient set of routines for manipulating files.
# stopwatch, an easy-to-use set of timing routines
# cla, command line argument parsing
# log, routines to handle log files
# error, proper handling of error messages

include ../../make_support/system_rules

OBJS  = caplib.o \
	cla.o \
	endian.o \
	error.o \
	fileUtil.o \
	get_ceos_names.o \
	get_stf_names.o \
	ioLine.o \
	log.o \
	stopwatch.o \
	share.o \
	strUtil.o \
	system.o \
	tmpdir.o \
	license.o \
	splash_screen.o \
	progress_meters.o \
	print_alerts.o \
	diagnostics.o

CFLAGS += -D_XOPEN_SOURCE=600

all: build_only
	mv asf.a $(LIBDIR)
	cp asf.h asf_glib.h asf_complex.h $(ASF_INCLUDE_DIR)
	rm *.o

build_only: $(OBJS)
	rm -f asf.a
	ar r asf.a $(OBJS)
	$(RANLIB) asf.a

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h) \
	 $(addprefix $(LIBDIR)/, $(LIBS))

check: unit_tests.c build_only
	$(CC) $(CFLAGS) $< asf.a -lcheck -lm $(LDFLAGS) -o asf_tester
	./asf_tester

clean:
	rm -rf $(OBJS) asf.a test_data/*.img asf_tester library_cooking_pot

distclean:
	rm -f core *~ TAGS gdb_init.com
