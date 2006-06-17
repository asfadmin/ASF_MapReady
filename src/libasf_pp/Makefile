# Makefile for		: libasf_pp
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

OBJS = pp_get_earth_radius.o

all:	libasf_pp

libasf_pp: $(OBJS)
	ar r libasf_pp.a $(OBJS)
	$(RANLIB) libasf_pp.a
	mv libasf_pp.a $(LIBDIR)
	cp asf_pp.h $(ASF_INCLUDE_DIR)

clean:
	rm -f core ${OBJS} *.o
