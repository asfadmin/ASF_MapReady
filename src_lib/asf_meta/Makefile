# asf_meta: a library to fetch metadata from disparate sources.
# Includes facilities to geolocate, extract critical angles from,
# and do interferometric processing on SAR images.
#   Now incorporates asf_ceos library, as well.

include ../../make_support/system_rules

# Things used/needed by make's implicit rules for lex and yacc.
# Need the GNU flex and bison, others are broken in variety of ways.
LEX = flex
YACC = bison -y			# -y means use yacc file name conventions.
#YDEBUG_FLAGS = -t -v   # Uncomment this line to debug parser
#LDEBUG_FLAGS = -d      # Uncomment this line to debug lexer
YFLAGS = $(YDEBUG_FLAGS) -d
LFLAGS = $(LDEBUG_FLAGS)
LDLIBS = -lfl -lregex_wrapper

YACC_SOURCES = metadata_parser.y
LEX_SOURCES = metadata_lexer.l

LEX_YACC_OBJS =  $(patsubst %.y, %.o, $(YACC_SOURCES)) \
                 $(patsubst %.l, %.o, $(LEX_SOURCES))

OBJS =  coniFetch.o \
	getLatLong.o \
	getLoc.o \
	fixed2gei.o \
	gei2fixed.o \
	interp_stVec.o \
	jpl_proj.o \
	latLon2timeSlant.o \
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
	meta_read.o \
	meta_write.o \
        set_era.o \
        utc2gha.o \
	meta_read.o \
        $(LEX_YACC_OBJS)

all: asf_meta clean

asf_meta: $(OBJS)
	 ar rv asf_meta.a $(OBJS) 
	 $(RANLIB) asf_meta.a
	 mv asf_meta.a $(LIBDIR)
	 # Make a link obeying the usual naming convention.
	 ln -s -f $(LIBDIR)/asf_meta.a $(LIBDIR)/libasf_meta.a

# Small independent testing program for the parser itself.
parser_tester: $(OBJS) parser_tester.o

# Rebuild everything if makefile or any ASF header has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -f $(OBJS) $(patsubst %.y, %.tab.c, $(YACC_SOURCES)) \
              $(patsubst %.y, %.tab.h, $(YACC_SOURCES)) y.tab.h

