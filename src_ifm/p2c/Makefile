# Makefile for		: p2c 

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a -lm
OBJS = p2c.o 

p2c: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o p2c $(OBJS) $(LIBS)
	rm *.o
	mv p2c$(BIN_POSTFIX) $(BINDIR)
	cp p2c.1 ../../man/cat1
