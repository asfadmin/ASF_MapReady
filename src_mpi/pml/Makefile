# Makefile for		: pml
# Module Author		: Mark Ayers

include ../../make_support/system_rules

LIBS = 	$(LIBDIR)/asf_meta.a $(LIBDIR)/asf_las.a \
	$(LIBDIR)/libifm.a $(LIBDIR)/asf.a -lm -lmpi 
OBJS = pml.o \
       c2i.o \
       colortable.o

pml: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pml $(OBJS) $(LIBS)
	rm *.o
	mv pml$(BIN_POSTFIX) $(BINDIR)
	cp pml.1 ../../man/man1
