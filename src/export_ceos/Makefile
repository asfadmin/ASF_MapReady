#	export_ceos
#	by Rudi Gens, ASF, 2004

include ../../make_support/system_rules

CFLAGS  = $(FLAGS) -DDECODE_CEOS
LIBS    = $(LIBDIR)/asf.a

OBJS	= config.o functions.o 

all:    export_ceos clean

export_ceos: export_ceos.c $(OBJS)
	$(CC) $(CFLAGS) -o export_ceos export_ceos.c $(OBJS) $(LIBS) $(LDFLAGS)
	mv export_ceos$(BIN_POSTFIX) $(BINDIR)
#	cp export_ceos.1 ../../man/cat1
clean:
	rm -f core $(OBJS) *.o
