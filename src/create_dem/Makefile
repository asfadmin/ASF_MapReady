#	create_akdem
#	by Rudi Gens, ASF, 2001

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) $(GSL_CFLAGS) $(PROJ_CFLAGS)

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS)

OBJS  = config.o \
	check_refinement.o \
	create_dem.o \
	functions.o

all: create_dem clean

create_dem: $(OBJS)
	$(CC) $(CFLAGS) -o create_dem $(OBJS) $(LIBS) $(LDFLAGS)
	mv create_dem$(BIN_POSTFIX) $(BINDIR)
	#cp create_dem.1 ../../man/cat1

clean:
	rm -f core $(OBJS) *.o
