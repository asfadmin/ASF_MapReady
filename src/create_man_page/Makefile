# Makefile for create_man_page
# Author Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS = create_man_page.o

all: create_man_page clean

create_man_page: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o create_man_page $(OBJS) $(LIBS)
	cp create_man_page$(BIN_POSTFIX) $(BINDIR)
#	cp create_man_page.1 ../../man/man1/create_man_page.1

clean:
	rm -f $(OBJS)


