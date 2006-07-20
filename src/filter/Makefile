# Makefile for 		:  filter
# Module Author 	:  Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

OLIBS = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS)

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
