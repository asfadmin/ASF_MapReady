CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = faraday_prediction

CFLAGS += $(GLIBS_CFLAGS)

LIBS  = \
	$(LIBDIR)/asf.a \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = $(TARGET).o

all: prog
	-rm *.o

prog: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)
	cp *.txt $(SHAREDIR)

clean:
	rm -f core $(OBJS) *.o
