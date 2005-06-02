PROGRAM := asf_terrain_correct

include ../../make_support/system_rules

###############################################################################
#                                                                             
# List of Sources                                                             
#                                                                             
# Here are the variables that list all the sources that need building
# and public headers that need installing.  When new source files are
# added, something in here will need to change.
#                                                                             
###############################################################################

PROGRAM_C_SOURCE = $(addsuffix .c, $(PROGRAM))

# Classes.  Each entry here has a corresponding .h and .c file.
CLASSES := date_time \
	   dem \
	   map_projected_dem \
           orbital_state_vector \
	   platform_path \
           progress_meter \
           slant_range_image \
	   slant_range_image_cspline \
	   vector \
           ITRS_platform_path \
           ITRS_point \
	   lsm \
	   dem_geom_info

# Non-main, non-class source files that need building.
OTHER_C_SOURCES := utilities.c

# We have a special little program which we run to mash a bunch of
# International Earth Rotation Service Bulletin B issues together.
IERS_BULLETIN_B_MASHER := ./scan_IERS_bulletin_Bs.pl

# Sources from which static data files needed by the library code are
# derived live in this directory.
IERS_BULLETIN_B_DIR := IERS_Bulletin_Bs

###############################################################################
#
# Libraries and Tools
#
# Here are variables which describe the libraries and tools needed by
# this module, and the flags required to compile code which uses them.
# If a new library or tool dependency is added, something in here will
# need to change.
#
###############################################################################

# The ScratchPlot library allows us to draw quick plots of data,
# images, etc.  Useful for development work.
ifeq ($(BK_DEBUG), 1)
  SCRATCHPLOT_INCLUDE_DIR := /tmp/test_scratchplot/include

  INCLUDE_FLAGS := -I$(ASF_INCLUDE_DIR) -I$(SCRATCHPLOT_INCLUDE_DIR) \
                   $(shell pkg-config --cflags-only-I glib-2.0)
else 
  INCLUDE_FLAGS := -I$(ASF_INCLUDE_DIR) $(GLIB_CFLAGS)
endif

CPPFLAGS := $(INCLUDE_FLAGS)

ifndef $(DEBUG_BUILD)
  CPPFLAGS += -DFINAL_PRODUCTION_BUILD
endif

# I'd like to use -Wconversion as advised by the GSL, but it seems to
# make it impossible to pass arguments of type float to functions that
# take type float (still get a complaint about argument being passed
# as float due to prototype rather than double.
#          -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align
# For the moment we don't have -Wshadow in either, since fftw 3.0.1 at
# least ends up redefining some things when compiled with
# -D_ISOC99_SOURCE.
#CFLAGS += -Werror -Wall -W -g -O2 -Wmissing-prototypes -Wstrict-prototypes \
#          -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings \
#          -Wnested-externs -fno-common
CFLAGS += -Werror -Wall -W -g -O2 -Wmissing-prototypes -Wstrict-prototypes \
          -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings \
          -Wnested-externs -fno-common

ifeq ($(DEBUG_BUILD), 2)
  CFLAGS += -O0
endif

ifeq ($(BK_DEBUG), 1)
  # Note that debugging builds turn optimization off, which
  # unfortunately causes gcc not to catch certain warnings (because the
  # optimizer doesn't run).
  CFLAGS += $(shell pkg-config --cflags-only-other glib-2.0) -O0

  LDFLAGS := $(shell pkg-config --libs-only-other glib-2.0)

  # The ScratchPlot library allows us to draw quick plots of data,
  # images, etc.  Useful for development work.
  SCRATCHPLOT_LIB_DIR := /tmp/test_scratchplot/lib
endif 

# Libraries needed by this program.
ifeq ($(BK_DEBUG), 1)
  LDLIBS := $(LIBDIR)/asf.a \
	    $(LIBDIR)/asf_meta.a \
	    $(LIBDIR)/libasf_proj.a \
	    $(LIBDIR)/libproj.a \
            $(LIBDIR)/libasf_reporting.a \
	    $(LIBDIR)/libasf_raster.a \
	    $(LIBDIR)/asf.a \
	    $(LIBDIR)/libgsl.a \
	    $(LIBDIR)/libgslcblas.a \
	    $(LIBDIR)/libjpeg.a \
	    $(LIBDIR)/libfftw3.a \
	    $(shell pkg-config --libs-only-L glib-2.0) \
            $(shell pkg-config --libs-only-l glib-2.0) \
	    -Wl,--rpath,$(SCRATCHPLOT_LIB_DIR) \
	    -L$(SCRATCHPLOT_LIB_DIR) -lscratchplot \
	    -lm
else
  LDLIBS := $(LIBDIR)/asf.a \
	    $(LIBDIR)/asf_meta.a \
	    $(LIBDIR)/libasf_proj.a \
	    $(LIBDIR)/libproj.a \
            $(LIBDIR)/libasf_reporting.a \
	    $(LIBDIR)/libasf_raster.a \
	    $(LIBDIR)/asf.a \
	    $(GSL_LIBS) \
	    $(LIBDIR)/libjpeg.a \
	    $(LIBDIR)/libfftw3.a \
	    $(GLIB_LIBS)

endif

# Defines needed by this module.  For defining the location of the
# shared data archive, etc.
DEFINE_FLAGS := -DASF_SHAREDIR=\"$(SHAREDIR)/$(MN)\"
ifeq ($(BK_DEBUG), 1)
  DEFINE_FLAGS += -DBK_DEBUG
endif


# Add the define flags to the other cpp flags.
CPPFLAGS += $(DEFINE_FLAGS)

###############################################################################
#
# Install
#
# Install built and tested program and supporting files into standard
# staging area.
#
###############################################################################

# Note that build_stamp actually installs in this case.
install: build_stamp
	cp time_scale_offsets $(SHAREDIR)

###############################################################################
#
# Tests
#
# Rules which say how to test the features of the library.  As new
# classes are added it is good to add tests.
#
###############################################################################

# To test, we must have everything built, and any tools we intend to
# call installed.
TEST_PREREQUISITES := build_stamp

test_stamp: test_orbital_state_vector_stamp \
            test_platform_path_stamp \
            test_date_time_stamp
	touch $@

# Build and run the date_time class test.
test_date_time_stamp: test_date_time.c $(TEST_PREREQUISITES)
	$(CC) $(CPPFLAGS) $(CFLAGS) $< $(NONMAIN_OBJS) $(LDLIBS) \
              -o test_date_time
	./test_date_time
	touch $@

# Build and run the orbital_state_vector class test.
test_orbital_state_vector_stamp: test_orbital_state_vector.c \
                                 $(TEST_PREREQUISITES)
	$(CC) $(CPPFLAGS) $(CFLAGS) $< $(NONMAIN_OBJS) $(LDLIBS) \
              -o test_orbital_state_vector
	./test_orbital_state_vector;
	touch $@

test_platform_path_stamp: test_platform_path.c \
                          test_orbital_state_vector_stamp \
                          $(TEST_PREREQUISITES)
	$(CC) $(CPPFLAGS) $(CFLAGS) $< $(NONMAIN_OBJS) $(LDLIBS) \
              -o test_platform_path
	./test_platform_path
	touch $@


# Build (and don't run... yet) the test_dem class test.
test_dem_stamp: test_dem.c $(TEST_PREREQUISITES)
	$(CC) $(CPPFLAGS) $(CFLAGS) $< $(NONMAIN_OBJS) $(LDLIBS) dem.o \
	      -o test_dem
	touch $@

###############################################################################
#
# Stuff to Actually Build Stuff
#
# This portion of this makefile consists of variables derived from the
# above variables and rules that actually build stuff.
#
###############################################################################

# Sources.
SRCS := $(PROGRAM_C_SOURCE) $(OTHER_C_SOURCES) $(addsuffix .c, $(CLASSES))

# Module object files.
OBJS := $(patsubst %.c, %.o, $(SRCS))

# Objects other than the one containing the main program.
NONMAIN_OBJS := $(patsubst %.c, %.o, $(OTHER_C_SOURCES) \
                $(addsuffix .c, $(CLASSES)))

ifeq ($(BK_DEBUG), 1)
  # Some options to link in the scratchplot library, used to take a look
  # at data.
  DRAW_OPTS := -Wl,-rpath,/tmp/test/lib -L/tmp/test/lib -lscratchplot
endif

# Building depends on having all the individual files compiled, and on
# having depended on libraries installed.  Actually, since the
# libraries are all static, we can build fine without them, but its ok
# to insist on what will logically be required anyway.  We also need
# to have the static data files we need built.
build_stamp: $(OBJS) $(LIBS) static_data_file_build_stamp
	$(CC) $(LDFLAGS) $(OBJS) $(LDLIBS) -o $(PROGRAM)
	mv $(PROGRAM)$(BIN_POSTFIX) $(BINDIR)
	touch $@

ifeq ($(BK_DEBUG), 1)
# This is a target for a somewhat isolated test of the orbit model.
  test_model: $(OBJS) $(LIBS) static_data_file_build_stamp test_model.o
	$(CC) $(LDFLAGS) $(patsubst asf_terrain_correct.o, , $(OBJS)) \
              test_model.o $(LDLIBS) -o $@

# This is a target for a somewhat isolated test of fft coregistration.
  test_coreg: $(OBJS) $(LIBS) test_coreg.o
	$(CC) $(LDFLAGS) $(patsubst asf_terrain_correct.o, , $(OBJS)) \
              test_coreg.o $(LDLIBS) -o $@
endif

static_data_file_build_stamp: time_scale_offsets
	touch $@

# Script which concatenates International Earth Rotation Service
# Bulletin B issues into a data file.
time_scale_offsets: $(IERS_BULLETIN_B_DIR) \
                    $(IERS_BULLETIN_B_MASHER)
	$(IERS_BULLETIN_B_MASHER) $< $@

# Include the C include dependencies.  This mechanism depends on a
# useful GNU make feature: if, after a make file has been read, any of
# its included files are found to be out-of-date or nonexistent
# targets of rules, then those include files are rebuilt and then make
# starts again.
DEPEND_FILES := $(patsubst %.c, %.d, $(SRCS))
# Here is a static pattern rule saying how to build .d files from .c
# files, for any .c file listed in $(DEPEND_FILES).
$(DEPEND_FILES): %.d: %.c
	printf "$@ " >$@
	gcc -MM $(INCLUDE_FLAGS) $(DEFINE_FLAGS) $< >>$@
# The leading hyphen keeps make from giving a warning if it can't find
# one or more of $(DEPEND_FILES).  Since they don't exist after a
# clean, these warning will always be given on a first build.
# Silencing them looks cleaner, but of course, you won't get a warning
# message if it misses them for any other reason either...
-include $(DEPEND_FILES)

# The .o files are built from the .c files, but they have other
# dependencies as well.  The other dependencies are automaticly
# tracked in the above rule via automatic header dependency tracking.
# We again use a static pattern rule to specify which files the rule
# should (always) apply to.
$(OBJS): %.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

# Delete files which produced by commands that return non-zero.
.DELETE_ON_ERROR:

# Remove the dependency files, object files, dumped cores, the library
# archive itself, the test program or programs, and all stamps.
.PHONY: clean
clean:
	rm -rf $(OBJS) \
	       $(DEPEND_FILES) \
	       *_stamp \
               bk_debug_*_freeze \
               core.* \
	       core \
	       test_model.o \
	       test_model \
               time_scale_offsets \
	       test_orbital_state_vector \
               test_platform_path \
	       test_date_time
