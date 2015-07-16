CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for create_man_page
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS = create_man_page.o

all: create_man_page clean

create_man_page: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o create_man_page $(OBJS) $(LIBS)
	mv create_man_page$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS)

