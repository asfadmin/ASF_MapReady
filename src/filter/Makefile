# Makefile for 		:  filter
# Module Author 	:  Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a

LIBS  = $(OLIBS) \
	-lm

OBJS  = kernel.o \
	filter.o

filter: $(OBJS) $(OLIBS)
	$(CC) $(CFLAGS) -o filter $(OBJS) $(LIBS)
	rm *.o
	mv filter$(BIN_POSTFIX) $(BINDIR)
#	cp filter.1 ../../man/man1/filter.1

#kernel.o : kernel.c
#	$(CC) $(CFLAGS) -c kernel.c
clean:
	rm -f core $(OBJS) *.o
