# Makefile for meta2ddr/ddr2meta
# Author Patrick Denny

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS_M2D = meta2ddr.o

OBJS_D2M = ddr2meta.o

all: meta2ddr ddr2meta clean

meta2ddr: $(OBJS_M2D)
	$(CC) $(CFLAGS) $(LFLAGS) -o meta2ddr $(OBJS_M2D) $(LIBS)
	mv meta2ddr$(BIN_POSTFIX) $(BINDIR)
	cp meta2ddr.1 ../../man/man1/meta2ddr.1

ddr2meta: $(OBJS_D2M)
	$(CC) $(CFLAGS) $(LFLAGS) -o ddr2meta $(OBJS_D2M) $(LIBS)
	mv ddr2meta$(BIN_POSTFIX) $(BINDIR)
	cp ddr2meta.1 ../../man/man1/ddr2meta.1

check: meta2ddr_tester.c meta2ddr ddr2meta
	$(CC) $(CFLAGS) -D_TEST_DATA_DIR=\"./test_data/\" -o meta2ddr_tester $< -lm -lcheck 
	./meta2ddr_tester

clean:
	rm -f $(OBJS_M2D) $(OBJS_D2M) ./meta2ddr_tester
