# Makefile for meta2esri
# Author Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS)

OBJS_M2E = meta2esri.o
OBJS_E2M = esri2meta.o

all: meta2esri esri2meta clean

meta2esri: $(OBJS_M2E)
	$(CC) $(CFLAGS) $(LFLAGS) -o meta2esri $(OBJS_M2E) $(LIBS)
	mv meta2esri$(BIN_POSTFIX) $(BINDIR)
#	cp meta2esri.1 ../../man/man1/meta2esri.1

esri2meta: $(OBJS_E2M)
	$(CC) $(CFLAGS) $(LFLAGS) -o esri2meta $(OBJS_E2M) $(LIBS)
	mv esri2meta$(BIN_POSTFIX) $(BINDIR)
#	cp esri2meta.1 ../../man/man1/esri2meta.1

# check: meta2ddr_tester.c meta2ddr ddr2meta
#	$(CC) $(CFLAGS) -D_TEST_DATA_DIR=\"./test_data/\" -o meta2ddr_tester $< -lm -lcheck 
#	./meta2ddr_tester

clean:
	rm -f $(OBJS_M2E) $(OBJS_E2M)


