# Makefile for convert2byte

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = converters.o \
	convert2byte.o

all: build_only
	mv convert2byte$(BIN_POSTFIX) $(BINDIR)
	cp convert2byte.1 ../../man/man1
	rm *.o

build_only: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o convert2byte $(OBJS) $(LIBS) $(LDFLAGS)

check: convert2byte_tester.c build_only
	$(CC) -DTEST_DATA_DIR='"$(TEST_DATA_DIR)"' $(CFLAGS) $< $(LFLAGS) \
              -lcheck $(LDFLAGS) -o convert2byte_tester
	mkdir test_data
	./convert2byte_tester

clean:
	rm -f *.o convert2byte_tester
