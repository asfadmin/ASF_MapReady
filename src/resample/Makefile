# Makefile for 		:  resample
# Module Author 	:  Tom Logan & Mike Shindle

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a
LIBS = $(OLIBS) -lm
OBJS = 	filter.o 

resample:  resample.c $(OBJS)  $(OLIBS)
	$(CC) $(CFLAGS) -o resample resample.c $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv resample$(BIN_POSTFIX) $(BINDIR)
	cp resample.1 ../../man/man1/resample.1

filter.o : filter.c
	$(CC) $(CFLAGS) -c filter.c
