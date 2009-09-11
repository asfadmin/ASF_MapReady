# Makefile

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a

OBJS  = byteswap.o

all: byteswap clean

byteswap: $(OBJS)
	$(CC) $(CFLAGS) -o byteswap $(OBJS) $(LIBS) $(LDFLAGS)
	mv byteswap$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS)
