# Makefile for converter_standard

include ../Make_rules

OBJS  = convert_standard.o

LIBS  = $(LIBDIR)/libUtil.a \
	$(LIBDIR)/asf_meta.a \
	-lm

all: convert_standard clean

convert_standard: $(OBJS)
	$(CC) $(CFLAGS) -o convert_standard $(OBJS) $(LIBS)
	mv convert_standard$(BIN_POSTFIX) $(BINDIR)
#	cp convert_standard.1 ../../man/cat1

clean:
	rm -f *.o
