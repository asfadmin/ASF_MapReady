# asf_meta: a library to fetch metadata from disparate sources.
# Includes facilities to geolocate, extract critical angles from,
# and do interferometric processing on SAR images.
#   Now incorporates asf_ceos library, as well.

include ../../make_support/system_rules

# Static libraries used by this library.  These get pulled out of the
# library repository and expanded, then recombined together with the
# objects from this library to save user having to link against all
# libraries used by this library and give some semblance of modularity
# to this benighted C language.
LIBS = asf.a \
       asf_las.a \
       regex_wrapper.a

# Things used/needed by make's implicit rules for lex and yacc.
# Need the GNU flex and bison, others are broken in variety of ways.
LEX = flex
YACC = bison -y			# -y means use yacc file name conventions.

# Uncomment next line and yydebug lines in .y file to debug parser.
#YDEBUG_FLAGS = -t -v   
# Uncomment next line to debug lexer.
#LDEBUG_FLAGS = -d     

# All normal lexer parser symbols are preceded by 'meta_' namespace hack.
YFLAGS = $(YDEBUG_FLAGS) -d -p meta_yy
LFLAGS = $(LDEBUG_FLAGS) -Pmeta_yy

YACC_SOURCES = metadata_parser.y
LEX_SOURCES = metadata_lexer.l

LEX_YACC_OBJS =  $(patsubst %.y, %.o, $(YACC_SOURCES)) \
                 $(patsubst %.l, %.o, $(LEX_SOURCES))

OBJS =  $(LEX_YACC_OBJS) \
	getLatLong.o \
	getLoc.o \
	err_die.o \
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
	coniFetch.o \
	meta_create.o \
        meta_init_aisp.o \
        meta_init_asf.o \
        meta_init_ceos.o \
        meta_init_stVec.o \
	meta_read.o \
	meta_write.o \
        set_era.o \
        utc2gha.o

all: asf_meta

asf_meta: build_only
	 mv asf_meta.a $(LIBDIR)
	 # Make a link obeying the usual naming convention.
	 ln -s -f $(LIBDIR)/asf_meta.a $(LIBDIR)/libasf_meta.a

build_only: $(OBJS) cook_libs
	rm -rf asf_meta.a	# Get rid of all stale symbols.
	ar rv asf_meta.a $(OBJS) $(wildcard ./library_cooking_pot/*.o) 
	$(RANLIB) asf_meta.a

cook_libs: $(addprefix $(LIBDIR)/, $(LIBS)) library_cooking_pot
	cd library_cooking_pot; \
	for lib in $(LIBS); do \
	  ar xv ../$(LIBDIR)/$$lib; \
	done

library_cooking_pot:
	mkdir library_cooking_pot

# Rebuild everything if makefile or any local or ASF header or any lex
# or yacc file has changed.  The line commented out uses a target
# local variable to keep track of the local headers which are not
# automaticly generated, useful to prevent make from rebuilding
# everything if the lex and yacc objects aren't first in the object
# list.
#$(OBJS): LOCAL_HEADERS = $(subst y.tab.h, , $(wildcard *.h))
$(OBJS): Makefile $(LOCAL_HEADERS) $(wildcard ../../include/*.h) \
	 $(YACC_SOURCES) $(LEX_SOURCES)

check: unit_tests.c build_only
	$(CC) $(CFLAGS) $< asf_meta.a -lm -lcheck $(LDFLAGS) \
              -o asf_meta_tester 
	./asf_meta_tester

clean:
	rm -rf $(OBJS) $(patsubst %.y, %.tab.c, $(YACC_SOURCES)) \
               $(patsubst %.y, %.tab.h, $(YACC_SOURCES)) y.tab.h y.output \
	       library_cooking_pot asf_meta_tester

distclean:
	rm -f core *~ TAGS gdb_init.com
