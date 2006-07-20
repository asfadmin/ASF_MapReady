# Makefile for convert2vector

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	-lm
OBJS  = convert2vector.o

all: convert2vector clean

convert2vector: $(OBJS)
	$(CC) $(CFLAGS) -o convert2vector $(OBJS) $(LIBS) $(LDFLAGS)
	mv convert2vector$(BIN_POSTFIX) $(BINDIR)

clean:
	rm *.o
