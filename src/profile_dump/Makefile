# Makefile for		: profile_dump
# Module Author		: Joe Lovick

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	-lm
OBJS = profile_dump.o 

c2p: $(OBJS)
	$(CC) -g $(CFLAGS) -o profile_dump $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv profile_dump$(BIN_POSTFIX) $(BINDIR)

