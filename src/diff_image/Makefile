# Makefile for diff_image
# by Orion Lawlor, ASF, 1997
#    Patrick Denny, 2/2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = diff_image.o

all: diff_image clean

diff_image: $(OBJS)
	$(CC) $(CFLAGS) -o diff_image $(OBJS) $(LIBS) $(LDFLAGS)
	mv diff_image$(BIN_POSTFIX) $(BINDIR)
	cp diff_image.1 ../../man/cat1

clean:
	rm -f core $(OBJS)
