CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
include ../../make_support/system_rules

TARGET = analyze_yaw

CFLAGS += $(GLIBS_CFLAGS) $(HDF5_CFLAGS)

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = $(TARGET).o

all: prog
	-rm *.o

prog: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
