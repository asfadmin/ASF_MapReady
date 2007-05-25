# Makefile for		: c2p 
# Module Author		: Rob Fatland & Mike Shindle

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS)

OBJS = c2p.o 

c2p: $(OBJS)
	$(CC) $(CFLAGS) -o c2p $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv c2p$(BIN_POSTFIX) $(BINDIR)
	cp c2p.1 ../../man/man1
