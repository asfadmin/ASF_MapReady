include ../../make_support/system_rules

LIBS =	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS =  geocode_options.o

all: build_only
	cp geocode_options.a $(LIBDIR)
	cp geocode_options.h $(ASF_INCLUDE_DIR)
	rm *.o

build_only: $(OBJS)
	rm -f geocode_options.a
	ar r geocode_options.a $(OBJS)
	$(RANLIB) geocode_options.a

clean:
	rm -rf $(OBJS) geocode_options.a geocode_options.t.o geocode_options.t

test:  geocode_options.t.c all
	$(CC) $(CFLAGS)  geocode_options.t.c $(LIBDIR)/geocode_options.a $(LIBS) -o  geocode_options.t
