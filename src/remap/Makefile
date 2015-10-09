# Makefile for		: remap
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_remap.a \
	$(LIBDIR)/asf_meta.a \
        $(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	-lm -lz

OBJS = \
	remap_main.o

CFLAGS += -Wall $(W_ERROR)

all: exe clean

exe: $(OBJS)
	$(CC) $(CFLAGS) -o remap $(OBJS) $(LIBS) $(LDFLAGS)
	mv remap$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
