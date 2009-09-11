# Makefile for		: igram
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm
OBJS = igram.o

igram: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o igram $(OBJS) $(LIBS) $(LDFLAGS)
	mv igram$(BIN_POSTFIX) $(BINDIR)
	cp igram.1 ../../man/man1
	rm *.o

