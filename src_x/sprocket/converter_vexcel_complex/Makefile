# Makefile for vexcel_complex_metadata and vexcel_complex_data_planes

include ../Make_rules

LIBS  = $(LIBDIR)/libUtil.a \
	-lm

META_OBJS = ceos_rd.o \
	vexcel_complex_metadata.o
DATA_OBJS  = ceos_rd.o \
	vexcel_complex_data_planes.o

all: vexcel_complex_metadata vexcel_complex_data_planes clean

vexcel_complex_metadata: $(META_OBJS)
	$(CC) $(CFLAGS) -o vexcel_complex_metadata $(META_OBJS) $(LIBS)
	mv vexcel_complex_metadata$(BIN_POSTFIX) $(BINDIR)

vexcel_complex_data_planes: $(DATA_OBJS)
	$(CC) $(CFLAGS) -o vexcel_complex_data_planes $(DATA_OBJS) $(LIBS)
	mv vexcel_complex_data_planes$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
