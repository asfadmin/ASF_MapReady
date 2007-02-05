# Makefile for libasf_proj library

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

# libs for the test program
LIBS =	$(PROJ_LIBS) \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	-lm

OBJS =  read_proj_file.o \
	is_map_projected.o \	
	project.o

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

test: project.t.c  nad27.t.c all
	$(CC) $(CFLAGS) nad27.t.c $(LIBDIR)/libasf_proj.a $(LIBS) -o nad27.t

