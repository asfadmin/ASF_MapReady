# Makefile for asf_elevation

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/libasf_insar.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

OBJS  = asf_elevation.o

all: asf_elevation clean

asf_elevation: $(OBJS)
	$(CC) $(CFLAGS) -o asf_elevation $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_elevation$(BIN_POSTFIX) $(BINDIR)

clean:
	rm *.o
