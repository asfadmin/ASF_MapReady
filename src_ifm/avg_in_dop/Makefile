# Makefile for avg_in_dop
# Version 1.0
#
#

include ../../make_support/system_rules

CFLAGS = $(FLAGS) -DDECODE_CEOS
LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm
OBJS  = avg_in_dop.o

all: avg_in_dop clean

avg_in_dop: $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o avg_in_dop $(OBJS) ${LIBS}
	mv avg_in_dop $(BINDIR)

clean:
	rm -f core ${OBJS} *.o
