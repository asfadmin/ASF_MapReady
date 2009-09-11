# Makefile for		: mfd ( mask_from_dem )
# Module Author		: Joe Lovick

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm
OBJS = mfd.o 

mfd: $(OBJS)
	$(CC) -g $(CFLAGS) -o mfd $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv mfd$(BIN_POSTFIX) $(BINDIR)

