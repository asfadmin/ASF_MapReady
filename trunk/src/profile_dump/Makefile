# Makefile for		: profile_dump
# Module Author		: Joe Lovick

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

OBJS = profile_dump.o 

c2p: $(OBJS)
	$(CC) -g $(CFLAGS) -o profile_dump $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv profile_dump$(BIN_POSTFIX) $(BINDIR)

