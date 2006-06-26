# Makefile for ASF stats program

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS  = statIO.o \
	stats.o

all: stats clean

stats: $(OBJS)
	$(CC) $(CFLAGS) -o stats $(OBJS) $(LIBS) $(LDFLAGS)
	mv stats$(BIN_POSTFIX) $(BINDIR)
	cp stats.1 ../../man/man1/.

check: stats_tester.c statIO.o
	$(CC) $(CFLAGS) -o stats_tester $< statIO.o -lm -lcheck $(LDFLAGS) $(LIBS)
	./stats_tester

clean:
	rm -f stats_tester $(OBJS)
