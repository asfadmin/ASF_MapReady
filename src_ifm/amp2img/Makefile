# Makefile for		: amp2img
# Module Author		: Rob Fatland, Mike Shindle, Tom Logan, Patrick Denny

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS= amp2img.o \
      get_hist.o \
      get_mean.o \
      mldata.o 

all: build_only
	mv amp2img$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp amp2img.1 ../../man/man1

build_only: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o amp2img $(OBJS) $(LIBS)

check: amp2img_tester.c build_only
	$(CC) -DTEST_DATA_DIR='"$(TEST_DATA_DIR)"' $(CFLAGS) $< $(LFLAGS) \
              -lcheck $(LDFLAGS) -o amp2img_tester
	mkdir test_data
	./amp2img_tester

clean:
	rm -rf *.o amp2img_tester test_data

distclean: clean
	rm TAGS *~
