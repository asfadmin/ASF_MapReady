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
	fileUtil.o \
	stopwatch.o \
	cla.o \
	log.o \
	error.o

all: $(OBJS)
	ar rv asf.a $(OBJS)
	$(RANLIB) asf.a
	mv asf.a $(LIBDIR)
	# Make a link obeying the usual naming convention.
	ln -s -f  $(LIBDIR)/asf.a $(LIBDIR)/libasf.a
	rm *.o
