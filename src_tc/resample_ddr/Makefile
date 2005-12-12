# Makefile for 		:  resample_ddr
# Module Author 	:  Tom Logan & Mike Shindle

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
        $(GSL_LIBS)

LIBS = $(OLIBS) -lm
OBJS = 	filter.o 

resample_ddr:  resample_ddr.c $(OBJS)  $(OLIBS)
	$(CC) $(CFLAGS) -o resample_ddr resample_ddr.c $(OBJS) $(LIBS)
	rm *.o
	mv resample_ddr$(BIN_POSTFIX) $(BINDIR)
	cp resample_ddr.1 ../../man/man1/resample_ddr.1

filter.o : filter.c
	$(CC) $(CFLAGS) -c filter.c
