# Makefile for          : pp_kcuf
# Module Author         : Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_pp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_reporting.a \
        $(LIBDIR)/asf_geolib.a \
        $(LIBDIR)/asf.a \
        $(GSL_LIBS) \
        -lm

OBJS = main.c

all:    pp_kcuf

pp_kcuf: $(OBJS)
	gcc $(CFLAGS) $(LFLAGS) -o $@ $(OBJS) $(LIBS)
	mv $@$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core ${OBJS} *.o
