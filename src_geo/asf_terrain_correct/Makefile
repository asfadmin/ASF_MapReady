PROGRAM := asf_terrain_correct

include ../../make_support/system_rules

# Where to put text data if FINAL_PRODUCTION_BUILD is defined.
SHARE_DIR := '.'

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
           orbital_state_vector \
	   platform_path \
	   vector
# Disabled for now
#	   dem

# Non-main, non-class source files that need building.
OTHER_C_SOURCES := utilities.c

# We have a special little program which we run to mash a bunch of
# International Earth Rotation Service Bulletin B issues together.
IERS_BULLETIN_B_MASHER := ./scan_IERS_bulletin_Bs.pl

# Sources from which static data files needed by the library code are
# derived.
STATIC_DATA_FILE_SOURCES = IERS_Bulletin_Bs

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

#CC := /home/bkerin/local/gcc-3.4.3/bin/gcc

INCLUDE_FLAGS := -I$(ASF_INCLUDE_DIR)

CPPFLAGS := $(INCLUDE_FLAGS)

# I'd like to use -Wconversion as advised by the GSL, but it seems to
# make it impossible to pass arguments of type float to functions that
# take type float (still get a complaint about argument being passed
# as float due to prototype rather than double.
#          -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align
CFLAGS += -Werror -Wall -W -Wmissing-prototypes -Wstrict-prototypes \
          -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align \
          -Wwrite-strings -Wnested-externs -fno-common

# Libraries needed by this program.
LDLIBS := $(LIBDIR)/libasf_reporting.a \
	  $(LIBDIR)/asf.a \
	  $(LIBDIR)/asf_meta.a \
	  $(LIBDIR)/libasf_raster.a \
	  $(LIBDIR)/asf.a \
	  $(LIBDIR)/libgsl.a \
	  $(LIBDIR)/libgslcblas.a \
	  $(LIBDIR)/libjpeg.a \
	  $(LIBDIR)/libglib-2.0.a \
	  $(LIBDIR)/libiconv.a \
	  -lm

# Defines needed by this module.  For defining the location of the
# shared data archive, etc.
DEFINE_FLAGS := -DASF_SHAREDIR=\"$(SHAREDIR)/$(MN)\"

# Add the define flags to the other cpp flags.
CPPFLAGS += $(DEFINE_FLAGS)

###############################################################################
#
# Tests
#
# Rules which say how to test the features of the library.  As new
# classes are added its good to add tests.
#
###############################################################################


# To test, we must have everything built, and any tools we intend to
# call installed.
TEST_PREREQUISITES := build_stamp

test_stamp: test_orbital_state_vector_stamp \
            test_platform_path_stamp \
            test_date_time_stamp

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

# Building depends on having all the individual files compiled, and on
# having depended on libraries installed.  Actually, since the
# libraries are all static, we can build fine without them, but its ok
# to insist on what will logically be required anyway.  We also need
# to have the static data files we need built.
build_stamp: $(OBJS) $(LIBS) static_data_file_build_stamp
	$(CC) $(OBJS) $(LDFLAGS) $(LDLIBS) -o $(PROGRAM)
	mv $(PROGRAM)$(BIN_POSTFIX) $(BINDIR)

static_data_file_build_stamp: time_scale_offsets

# Script which concatenates International Earth Rotation Service
# Bulletin B issues into a data file.
time_scale_offsets: IERS_Bulletin_Bs \
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

# Remove the dependency files, object files, dumped cores, the library
# archive itself, the test program or programs, and all stamps.
.PHONY: clean
clean:
	rm -rf $(OBJS) \
	       $(DEPEND_FILES) \
	       *_stamp \
               core.* \
	       core \
               time_scale_offsets \
	       test_orbital_state_vector \
               test_platform_path \
	       test_date_time
