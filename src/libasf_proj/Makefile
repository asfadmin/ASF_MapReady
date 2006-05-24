# Makefile for libasf_proj library

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

LIBS =	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS =  read_proj_file.o \
	project.o

#CC := /home/bkerin/local/my_gcc/bin/gcc
#CC := gcc

all: build_only
	cp libasf_proj.a $(LIBDIR)
	cp libasf_proj.h $(ASF_INCLUDE_DIR)
	rm *.o

build_only: $(OBJS)
	rm -f libasf_proj.a
	ar r libasf_proj.a $(OBJS)
	$(RANLIB) libasf_proj.a

clean:
	rm -rf $(OBJS) libasf_proj.a project.t.o project.t

test: project.t.c all
	$(CC) $(CFLAGS) project.t.c $(LIBDIR)/libasf_proj.a $(LIBS) -o project.t
