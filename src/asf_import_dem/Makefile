# Makefile

include ../../make_support/system_rules

PROGRAM := asf_import_dem

OBJS := \
	check_parameters.o \
	check_splines.o \
	create_grid.o \
	data_to_fit.o \
	find_extents.o \
	geocode_options.o \
	gridFloat.o \
	parse_options.o \
	reverse_map.o \
	seamless_meta.o \
	write_asf_dem.o \
	$(PROGRAM).o

LIBS := $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libasf_raster.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(JPEG_LIBS) \
	-lpcre

CFLAGS += -I/usr/include/pcre \
          $(PROJ_CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS) $(JPEG_CFLAGS)

all: $(PROGRAM) #clean

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(PROGRAM)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS)
