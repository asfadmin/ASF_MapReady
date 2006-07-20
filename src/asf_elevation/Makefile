# Makefile for asf_elevation

include ../../make_support/system_rules

DEBUG_BUILD = 2

LIBS  = $(LIBDIR)/libasf_elevation.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	-lm
OBJS  = asf_elevation.o

all: asf_elevation clean

asf_elevation: $(OBJS)
	$(CC) $(CFLAGS) -o asf_elevation $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_elevation$(BIN_POSTFIX) $(BINDIR)

clean:
	rm *.o
