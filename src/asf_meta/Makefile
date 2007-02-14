# Makefile for asf_meta library

include ../../make_support/system_rules

# Things used/needed by make's implicit rules for lex and yacc.
# Need the GNU flex and bison, others are broken in variety of ways.
LEX = flex
YACC = bison -y			# -y means use yacc file name conventions.

# Build with this defined to enable parser diagnostics.  Note that
# this doesn't automaticly clean or otherwise force a rebuild.
# DEBUG_METADATA_PARSER = defined
ifdef DEBUG_METADATA_PARSER
  YDEBUG_FLAGS = -t -v
  LDEBUG_FLAGS = -d
  CC_PARSER_DEBUG_FLAGS = -DDEBUG_METADATA_PARSER
  CFLAGS += $(CC_PARSER_DEBUG_FLAGS)
endif

# All global lexer and parser symbols are preceded by 'meta_' namespace hack.
YFLAGS = $(YDEBUG_FLAGS) -d -p meta_yy
LFLAGS = $(LDEBUG_FLAGS) -Pmeta_yy -s

YACC_SOURCES = metadata_parser.y
LEX_SOURCES = metadata_lexer.l

LEX_YACC_OBJS =  $(patsubst %.y, %.o, $(YACC_SOURCES)) \
		 $(patsubst %.l, %.o, $(LEX_SOURCES))

LAS_OBJS = allocbuf.o \
	c_calcor.o \
	c_dkcre.o \
	c_dkdel.o \
	c_dkfree.o \
	c_dkresv.o \
	c_dkrspac.o \
	c_errmsg.o \
	c_getbdr.o \
	c_getddr.o \
	c_getsys.o \
	c_gettyp.o \
	c_intbdr.o \
	c_intddr.o \
	c_low2up.o \
	c_lsclos.o \
	c_lsmknm.o \
	c_lsopen.o \
	c_lsread.o \
	c_lsrepl.o \
	c_lsstat.o \
	c_lswrit.o \
	c_ptio.o \
	c_ptopen.o \
	c_putbdr.o \
	c_putddr.o \
	c_pxconv.o \
	c_pxcopy.o \
	c_pxfill.o \
	c_pxswap.o \
	c_pxsys.o \
	c_rotcor.o \
	c_rtcoef.o \
	c_sysset.o \
	c_up2low.o \
	convInt.o \
	doopen.o \
	lasIO.o  \
	packed_deg.o \
	spheroid_axes_lengths.o \
	squeeze.o

OBJS =  $(LEX_YACC_OBJS) \
	ardop_params.o \
	c_degdms.o \
	c_prostr.o \
	ceos_io.o \
	code_ceos.o \
	coniFetch.o \
	dateUtil.o \
	datum_spheroid.o \
	earth_radius2datum.o \
	err_die.o \
	fixed2gei.o \
	frame_calc.o \
	gei2fixed.o \
	getLatLong.o \
	getLoc.o \
	get_ceos.o \
	heading.o \
	interp_stVec.o \
	latLon2timeSlant.o \
	lzFetch.o \
	meta_complex2polar.o \
	meta_copy.o \
	meta_create.o \
	meta_get.o\
	meta_get_geo.o \
	meta_get_ifm.o \
	meta_get_util.o \
	meta_init.o \
	meta_init_ardop.o \
	meta_init_ceos.o \
	meta_init_stVec.o \
	meta_is_valid.o \
	meta2ddr.o \
	meta2envi.o \
	meta2esri.o \
	meta_read.o \
	meta_write.o \
	orbital_state_vector.o \
	meta_write_sprocket.o \
	proj2meta.o \
	propagate.o \
	readSubset.o \
	set_era.o \
	slantRange2groundPixel.o \
	unpacked_deg.o \
	utc2gha.o \
	util3d.o \
	vector.o \
	yaw2doppler.o \
	xpix_ypix.o \
	pp_corrected_vals.o \
	$(LAS_OBJS)

all: asf_meta

asf_meta: build_only
	mv asf_meta.a $(LIBDIR)

build_only: $(OBJS)
	rm -f asf_meta.a
	ar r asf_meta.a $(OBJS)
	$(RANLIB) asf_meta.a

# Rebuild everything if makefile or any local or ASF header or library
# or any lex or yacc file has changed.  The line commented out uses a
# target local variable to keep track of the local headers which are
# not automaticly generated, useful to prevent make from rebuilding
# everything if the lex and yacc objects aren't first in the object
# list.
#$(OBJS): LOCAL_HEADERS = $(subst y.tab.h, , $(wildcard *.h))
$(OBJS): Makefile $(LOCAL_HEADERS) $(wildcard ../../include/*.h) \
	$(YACC_SOURCES) $(LEX_SOURCES)

clean:
	rm -rf $(OBJS) $(patsubst %.y, %.tab.c, $(YACC_SOURCES)) \
	$(patsubst %.y, %.tab.h, $(YACC_SOURCES)) y.tab.h y.output \
	asf_meta_tester meta_update asf_meta.a

check: asf_meta_tester.c build_only
	$(CC) $(CFLAGS) $< asf_meta.a \
		$(LIBDIR)/libasf_proj.a \
		$(LIBDIR)/asf.a $(GSL_LIBS) $(PROJ_LIBS) -lcheck $(LDFLAGS) \
		-o asf_meta_tester
	./asf_meta_tester

meta_update: meta_update.c build_only
	$(CC) $(CFLAGS) $< asf_meta.a -lm $(LDFLAGS) -o meta_update

distclean:
	rm -f core *~ TAGS gdb_init.com
