# Makefile for		: c2p 
# Module Author		: Rob Fatland & Mike Shindle

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a -lm
OBJS = c2p.o 

c2p: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o c2p $(OBJS) $(LIBS)
	rm *.o
	mv c2p$(BIN_POSTFIX) $(BINDIR)
	cp c2p.1 ../../man/man1
