# asf_meta: a library to fetch metadata from disparate sources.
# Includes facilities to geolocate, extract critical angles from,
# and do interferometric processing on SAR images.
#   Now incorporates asf_ceos library, as well.
#
# Convert .c files to .o list:
#    >  ls *.c | sed -e "s/\.c/.o/" | awk '{print "\t"$1"\\"}'

include ../../make_support/system_rules

# Things used by makes implicit rules for lex and yacc.
# Uncommend these to debug the parser.
#YDEBUG_FLAGS = -t -v
#LDEBUG_FLAGS = -d
YFLAGS = $(YDEBUG_FLAGS) -d
LFLAGS = $(LDEBUG_FLAGS)

LEX_YACC_OBJS =  $(patsubst %.y, %.o, $(YACC_SOURCES)) \
                 $(patsubst %.l, %.o, $(LEX_SOURCES))

OBJS =  \
	coniFetch.o \
	getLatLong.o \
	getLoc.o \
	fixed2gei.o \
	gei2fixed.o \
	interp_stVec.o \
	jpl_proj.o \
	latLon2timeSlant.o \
	meta_coni.o \
	meta_get.o\
	meta_get_geo.o \
	meta_get_geo2.o \
	meta_get_ifm.o \
	meta_get_util.o \
	meta_init.o \
	read_baseline.o \
	slantRange2groundPixel.o \
	util3d.o \
	yaw2doppler.o \
        aisp_params.o \
        code_ceos.o \
        ceos2ddr.o \
        ceos_io.o \
        dateUtil.o \
        endian.o \
        get_ceos.o \
        heading.o \
        meta_create.o \
        meta_init_aisp.o \
        meta_init_asf.o \
        meta_init_ceos.o \
        meta_init_stVec.o \
        set_era.o \
        utc2gha.o \
        $(LEX_YACC_OBJS)

all: asf_meta clean

asf_meta: $(OBJS)
	 ar rv asf_meta.a $(OBJS)
	 $(RANLIB) asf_meta.a
	 mv asf_meta.a $(LIBDIR)
	 # Make a link obeying the usual naming convention.
	 ln -s -f $(LIBDIR)/asf_meta.a $(LIBDIR)/libasf_meta.a

clean:
	- rm $(OBJS)
