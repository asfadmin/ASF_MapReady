# Makefile for libasf_proj library

include ../../make_support/system_rules

LIBS =	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS =  project.o

all: build_only
	mv libasf_proj.a $(LIBDIR)
	rm *.o

build_only: $(OBJS)
	rm -f libasf_proj.a
	ar r libasf_proj.a $(OBJS)
	$(RANLIB) libasf_proj.a

clean:
	rm -rf $(OBJS) libasf_proj.a project.t.o project.t

test: project.t.c all
	$(CC) $(CFLAGS) project.t.c $(LIBDIR)/libasf_proj.a $(LIBS) -o project.t
