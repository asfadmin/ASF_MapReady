# Makefile for		: remap
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
        $(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	-lm

OBJS = CLA.o \
	Matrix2D.o \
	mapping.o \
	fetcher.o \
	poly.o \
	quadratic.o \
	mapSamp.o \
	warpMap.o \
	writer.o \
	remap.o

EXE_OBJS = \
	remap_main.o

CFLAGS += -Wall -Werror

all: lib exe clean

lib: $(OBJS)
	ar r libasf_remap.a $(OBJS)
	$(RANLIB) libasf_remap.a
	mv libasf_remap.a $(LIBDIR)
	cp asf_remap.h $(ASF_INCLUDE_DIR)

exe: $(EXE_OBJS)
	$(CC) $(CFLAGS) -o remap $(EXE_OBJS) $(LIBS) $(LIBDIR)/libasf_remap.a $(LDFLAGS)
	mv remap$(BIN_POSTFIX) $(BINDIR)
	cp remap.1 ../../man/cat1

clean:
	rm -f *.o
