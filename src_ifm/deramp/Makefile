# Makefile for		: deramp
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm 
OBJS = deramp.o 
        

deramp: $(OBJS)
	$(CC) $(CFLAGS) -o deramp $(OBJS) $(LIBS) $(LDFLAGS)
	mv deramp$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp deramp.1 ../../man/man1
