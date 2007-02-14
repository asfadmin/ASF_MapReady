# Makefile for meta2envi
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS)

OBJS_M2E = meta2envi.o
OBJS_E2M = envi2meta.o

all: meta2envi clean

meta2envi: $(OBJS_M2E)
	$(CC) $(CFLAGS) $(LFLAGS) -o meta2envi $(OBJS_M2E) $(LIBS)
	mv meta2envi$(BIN_POSTFIX) $(BINDIR)

envi2meta: $(OBJS_E2M)
	$(CC) $(CFLAGS) $(LFLAGS) -o envi2meta $(OBJS_E2M) $(LIBS)
	mv envi2meta$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2E) $(OBJS_E2M)


