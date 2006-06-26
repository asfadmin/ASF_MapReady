# Makefile for meta2envi
# Author Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS_M2E = meta2envi.o
OBJS_E2M = envi2meta.o

all: meta2envi envi2meta clean

meta2envi: $(OBJS_M2E)
	$(CC) $(CFLAGS) $(LFLAGS) -o meta2envi $(OBJS_M2E) $(LIBS)
	mv meta2envi$(BIN_POSTFIX) $(BINDIR)
#	cp meta2envi.1 ../../man/man1/meta2envi.1

envi2meta: $(OBJS_E2M)
	$(CC) $(CFLAGS) $(LFLAGS) -o envi2meta $(OBJS_E2M) $(LIBS)
	mv envi2meta$(BIN_POSTFIX) $(BINDIR)
#	cp envi2meta.1 ../../man/man1/envi2meta.1

# check: meta2ddr_tester.c meta2ddr ddr2meta
#	$(CC) $(CFLAGS) -D_TEST_DATA_DIR=\"./test_data/\" -o meta2ddr_tester $< -lm -lcheck 
#	./meta2ddr_tester

clean:
	rm -f $(OBJS_M2E) $(OBJS_E2M)


