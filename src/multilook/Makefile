CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for program multilook
# Module Author Mike Shindle,  6/1997
#               Patrick Denny, 2/2004

include ../../make_support/system_rules

LIBS = 	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(XML_LIBS) \
	-lm

OBJS  = c2i.o \
	colortable.o \
	multilook.o

all: multilook clean

multilook: $(OBJS)
	$(CC) $(CFLAGS) -o multilook $(OBJS) $(LIBS) $(LDFLAGS)
	mv multilook$(BIN_POSTFIX) $(BINDIR)
	cp multilook.1 ../../man/man1

clean:
	rm -f core *.o
