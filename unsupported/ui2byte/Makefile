# Makefile for ui2byte

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = ui2byte.o 

all: ui2byte clean

ui2byte: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ui2byte $(OBJS) $(LIBS)
	mv ui2byte$(BIN_POSTFIX) $(BINDIR)
	cp ui2byte.man ../../man/cat1/ui2byte.1

clean:
	rm *.o
