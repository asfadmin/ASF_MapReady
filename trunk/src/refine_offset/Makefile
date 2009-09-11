# Makefile for          :  refine_offset
# Module Author         :  Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm
OBJS  = main.o \
	refine.o \
	minimize.o

all: refine

refine_offset: refine 

refine:	$(OBJS)
	$(CC) $(CFLAGS) -o refine_offset $(OBJS) $(LIBS)  $(LDFLAGS)
	mv refine_offset$(BIN_POSTFIX) $(BINDIR)
	cp refine_offset.1 ../../man/cat1/
	rm *.o

