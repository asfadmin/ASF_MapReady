CFLAGS += $(GEOTIFF_CFLAGS)
# Makefile for: asf.a, our standard included library.
# Contains:
# caplib, our error-protected standard library routines.
# fileUtil, a convenient set of routines for manipulating files.
# stopwatch, an easy-to-use set of timing routines
# cla, command line argument parsing
# log, routines to handle log files
# error, proper handling of error messages

include ../../make_support/system_rules

OBJS  = asf_tiff.o \
	caplib.o \
	check.o \
	cla.o \
	endian.o \
	error.o \
	fileUtil.o \
	log.o \
	stopwatch.o \
	share.o \
	strUtil.o \
	system.o \
	tmpdir.o \
	license.o \
	splash_screen.o \
	print_alerts.o \
	diagnostics.o \
	matrix.o \
	vector.o \
	complex.o \
	solve1d.o \
	socket.o \
	httpUtil.o

CFLAGS += $(GEOTIFF_CFLAGS)

CFLAGS += $(W_ERROR) $(shell \
	if [ "$(SYS)x" != "solarisx" ]; \
	then echo "-D_XOPEN_SOURCE=600"; \
	else echo "-D_XOPEN_SOURCE=600 -std=c99"; \
	fi)

all: build_only test
	mv asf.a $(LIBDIR)
	cp asf.h asf_glib.h asf_complex.h caplib.h vector.h matrix.h \
		asf_tiff.h asf_jpeg.h $(ASF_INCLUDE_DIR)
	cp pixmaps/* $(SHAREDIR)
	rm *.o

build_only: $(OBJS)
	rm -f asf.a
	ar r asf.a $(OBJS)
	$(RANLIB) asf.a

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

check: unit_tests.c build_only
	$(CC) $(CFLAGS) $< asf.a -lcheck -lm $(LDFLAGS) -o asf_tester
	./asf_tester

clean:
	rm -rf $(OBJS) asf.a test_data/*.img asf_tester library_cooking_pot test *.t.o

distclean:
	rm -f core *~ TAGS gdb_init.com

test: *.t.c
	$(CC) $(CFLAGS) *.t.c asf.a -lm -o test $(CUNIT_LIBS)
	./test
