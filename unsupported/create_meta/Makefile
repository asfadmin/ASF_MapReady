# Makefile for          :  create_meta
# Module Author         :  Orion Lawlor
# Makefile Date         :  12/98

SYS = solaris
INCDIR = ../asf_inc
CFLAGS = -I$(INCDIR)  $(FLAGS) 
LIBDIR = ../../lib/$(SYS)
LIBS = $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_ceos.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a -lm
OBJS = create_meta.o

all: create_meta
	rm *.o

create_meta: $(OBJS) 
	$(CC) $(CFLAGS) -o create_meta $(OBJS) $(LIBS)
	mv create_meta ../../bin/$(SYS)
	cp create_meta.1 ../../man/cat1/

