# Makefile for libasf_reporting library

include ../../make_support/system_rules

OBJS = progress_meters.o

all: build_only
	mv libasf_reporting.a $(LIBDIR)
	rm *.o

build_only: $(OBJS)
	rm -f libasf_reporting.a
	ar r libasf_reporting.a $(OBJS)
	$(RANLIB) libasf_reporting.a

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h) \
	 $(addprefix $(LIBDIR)/, $(LIBS))

clean:
	rm -rf $(OBJS) libasf_reporting.a
