# Makefile for		: eleverr 
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm
OBJS  = eleverr.o 

eleverr: $(OBJS)
	$(CC) $(CFLAGS) -o eleverr $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv eleverr$(BIN_POSTFIX) $(BINDIR)
	cp eleverr.1 ../../man/cat1
