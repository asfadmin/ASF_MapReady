# Makefile for          :  refine_offset
# Module Author         :  Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
OBJS  = main.o \
	refine.o \
	minimize.o

all: refine

refine_offset: refine 

refine:	$(OBJS)
	$(CC) $(CFLAGS) -o refine_offset $(OBJS) $(LIBS) 
	mv refine_offset$(BIN_POSTFIX) $(BINDIR)
	cp refine_offset.1 ../../man/cat1/
	rm *.o

