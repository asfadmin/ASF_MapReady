AC_PREREQ(2.59)
AC_INIT
# The most recent version of aclocal doesn't seem to work if this
# macro isn't called.
#AM_INIT_AUTOMAKE
AC_LANG(C)

PACKAGE=convert
VERSION=`grep PACKAGE_VERSION_STRING include/asf_version.h | grep -i $PACKAGE | sed -e 's/^.*"\(.*\)"$/\1/'`

AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
AC_DEFINE_UNQUOTED(VERSION, "$VERSION")

echo "Configuring ASF $PACKAGE $VERSION"

AC_SUBST(PACKAGE)
AC_SUBST(VERSION)

build_gui="yes"
have_gsl="no"
have_pkg_config="yes"
have_glib="no"
have_proj="no"
have_jpeg="no"
have_tiff="no"
have_geotiff="no"
have_tcl_tk="no"
try_gtk="yes"
have_gtk="no"

AC_PROG_CC
AC_PROG_LEX
AC_PROG_YACC

cc="cc"
osType=`uname`
versMajor=`uname -r | awk -F. '{print $1}'`
versMinor=`uname -r | awk -F. '{print $2}'`

ranlib="@echo do not need to run ranlib on this system on file: "
optFlag="-O"
case "${osType}:${versMajor}:${versMinor}" in
	SunOS:*:*)
		sys="solaris" ;;
	Linux:*:*)
		sys="linux" ;;
	CYGWIN*:*:*)
		sys="win32"
		makeExtra="BIN_POSTFIX = .exe" ;;
	IRIX*:*:*)
		sys="irix"
		if [ "$CC" == "cc" ]; then
			optFlag="-O -n32"
		fi ;;
	*:*:*)
		echo "ASF tools are only supported on Solaris, Linux, and Windows (Cygwin)"
		echo "Your system appears to be '" $osType "'."
		echo "Exiting..."
		exit 1 ;;
esac

AC_SUBST(sys)

# Make the install directories for the libraries and binaries.
mkdir -p lib
mkdir -p bin
mkdir -p share

asfDir=`pwd`

# basic libraries check
AC_CHECK_LIB(c,fopen)
AC_CHECK_LIB(m,main)
AC_CHECK_HEADERS(unistd.h)

ext_lib_src_dir=${asfDir}/external/work_dir

#### GSL check ####
AC_CHECK_LIB(gslcblas,main)
AC_CHECK_LIB(gsl,main,have_gsl="yes",have_gsl="no")
if test "$have_gsl" = "no" ; then
   ASF_LIB_GSL="${ext_lib_src_dir}/libgsl"
   GSL_LIBS="\$(LIBDIR)/libgsl.a \$(LIBDIR)/libgslcblas.a"
else
   GSL_LIBS="-lgsl -lgslcblas"
fi
AC_SUBST(ASF_LIB_GSL)

#### libproj check ####
AC_CHECK_LIB(proj,pj_transform,have_proj="yes",have_proj="no")
if test "$have_proj" = "no" ; then
   ASF_LIB_PROJ="${ext_lib_src_dir}/libproj"
   PROJ_LIBS="\$(LIBDIR)/libproj.a"
else
   PROJ_LIBS="-lproj"
fi
AC_SUBST(ASF_LIB_PROJ)

#### pkg-config check ####
#if test -z "$PKG_CONFIG"; then
#   AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
#fi
#if test "$PKG_CONFIG" = "no" ; then
#   echo "*** pkg-config not found, can't build with gtk 2.0"
#   try_gtk="no"
#   have_pkg_config="no"
#fi
#if test "$have_pkg_config" = "no" ; then
#   ASF_PKG_CONFIG="${ext_lib_src_dir}/pkgconfig"
#fi
#AC_SUBST(ASF_PKG_CONFIG)

#### glib check ####
if test "$have_pkg_config" = "yes" ; then
   PKG_CHECK_MODULES([GLIB], [glib-2.0 >= 2.4.0],
      have_glib="yes", have_glib="no")
fi

if test "$have_glib" = "no" ; then
   ASF_GLIB="${ext_lib_src_dir}/libglib"
   GLIB_LIBS="\$(LIBDIR)/libglib-2.0.a \$(LIBDIR)/libiconv.a"
   GLIB_CFLAGS="-I../../include/glib-2.0 -I../../lib/glib-2.0/include"
else
   GLIB_LIBS="-lglib-2.0"
fi
AC_SUBST(ASF_GLIB)

#### libjpeg check ####
AC_CHECK_LIB(jpeg,jpeg_read_header,have_jpeg="yes",have_jpeg="no")
if test "$have_jpeg" = "no" ; then
   ASF_LIB_JPEG="${ext_lib_src_dir}/libjpeg"
   JPEG_LIBS="\$(LIBDIR)/libjpeg.a"
else
   JPEG_LIBS="-ljpeg"
fi
AC_SUBST(ASF_LIB_JPEG)

#### libtiff check ####
AC_CHECK_LIB(tiff,TIFFGetTagListCount,have_tiff="yes",have_tiff="no")
if test "$have_tiff" = "no" ; then
   ASF_LIB_TIFF="${ext_lib_src_dir}/libtiff"
   TIFF_LIBS="\$(LIBDIR)/libtiff.a"
   LIBTIFF_LOCATION="-with-libtiff=../../.."
else
   TIFF_LIBS="-ltiff"
   LIBTIFF_LOCATION=""
fi
AC_SUBST(ASF_LIB_TIFF)
AC_SUBST(LIBTIFF_LOCATION)

#### libgeotiff check ####
AC_CHECK_LIB(geotiff,main,have_geotiff="yes",have_geotiff="no")
if test "$have_geotiff" = "no" ; then
   ASF_LIB_GEOTIFF="${ext_lib_src_dir}/libgeotiff"
   GEOTIFF_LIBS="\$(LIBDIR)/libgeotiff.a"
else
   GEOTIFF_LIBS="-lgeotiff"
fi
AC_SUBST(ASF_LIB_GEOTIFF)

#### TCL/TK check ####
AC_CHECK_LIB(tcl8.1,main)
AC_CHECK_LIB(tk8.1,main,have_tcl_tk="yes",have_tcl_tk="no")
if test "$have_tcl_tk" = "no" ; then
   ASF_LIB_TCL_TK="${ext_lib_src_dir}/tcl_tk"
   TCL_TK_LIBS="\$(LIBDIR)/libtcl8.1.a \$(LIBDIR)/libtk8.1.a"
else
   TCL_TK_LIBS="-ltcl8.1 -ltk8.1"
fi
AC_SUBST(ASF_LIB_TCL_TK)

#### gtk 2.4 check ####
if test "$try_gtk" = "yes" ; then
    PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.4.0,
      have_gtk="yes"
      AC_DEFINE(USE_GTK),
      have_gtk="no"
    )
fi

#### libglade check ####
if test "$have_gtk" = "yes" ; then
   PKG_CHECK_MODULES(LIBGLADE, libglade-2.0 > 2.0.0,
       have_libglade="yes", have_libglade="no")
fi

#### whether or not to build the ASF Convert GUI ####
if test "$have_libglade" = "yes" ; then
    ASF_GUI="src/asf_convert_gui"
fi
# we always build the GUI on windows, as we provide our own gtk libraries.
if test "$sys" = "win32" ; then
    ASF_GUI="src/asf_convert_gui"
fi
AC_SUBST(ASF_GUI)

echo "########## Determining machine characteristics #############"
cd make_support
$CC -o endian endian.c
./endian

#echo "########### Building the make_support/system_rules file ###############"

# write make_support/system_rules file header
cat <<EOF >system_rules
############### System_rules: ###############
# Contains information about the current machine.
# This file was automatically created by 'asf_tools/config'
# And is included by each program's Makefile.
#

EOF

# write make_support/system_rules ENDIAN_FLAGS
./endian -r >> system_rules
rm -f ./endian

# EOF (without quotes) makes cat print out strings with expanded variables
cat <<EOF >>system_rules
SYS     = $sys
CC      = $CC
RANLIB  = $ranlib
OPTFLAG = $optFlag

CFLAGS = $CFLAGS
LDFLAGS = $LDFLAGS

EOF

# 'EOF' (with quotes) makes cat print out literal strings (no expanded variables [$])
cat <<'EOF' >>system_rules
ASF_INCLUDE_DIR = ../../include
INCLUDE = -I. -I$(ASF_INCLUDE_DIR)
# Flags to support large (> 2GB) files.  These must precede all include options.
LARGEFILE_FLAGS = -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64
FLAGS   = -D_ISOC99_SOURCE $(LARGEFILE_FLAGS) $(INCLUDE) $(OPTFLAG) -D$(SYS) $(ENDIAN_FLAGS)
# If compiler is gcc we're going to add some debugging flags/libraries
#   if <make 'DEBUG_BUILD=1'>  apply debugging tags and show all compiler warnings
#   if <make 'DEBUG_BUILD=2'>  do the same and add the electric fence library
ifeq ($(CC),gcc)
  ifeq ($(DEBUG_BUILD),1)
    OPTFLAG    =
    DEBUGFLAGS = -Wall -g3 -O0
  endif
  ifeq ($(DEBUG_BUILD),2)
    OPTFLAG    =
    DEBUGFLAGS = -Wall -g3 -O0
    DEBUGLIBS  = -lefence
  endif
endif

LIBDIR  = ../../lib
BINDIR  = ../../bin
SHAREDIR = ../../share
DOCDIR = $(SHAREDIR)/doc

EOF

# EOF (without quotes) makes cat print out strings with expanded variables
cat <<EOF >>system_rules

GTK_LIBS = $GTK_LIBS
GTK_CFLAGS = $GTK_CFLAGS

GSL_LIBS = $GSL_LIBS
GSL_CFLAGS = $GSL_CFLAGS

GLIB_LIBS = $GLIB_LIBS
GLIB_CFLAGS = $GLIB_CFLAGS

PROJ_LIBS = $PROJ_LIBS
PROJ_CFLAGS = $PROJ_CFLAGS

JPEG_LIBS = $JPEG_LIBS
JPEG_CFLAGS = $JPEG_CFLAGS

TIFF_LIBS = $TIFF_LIBS
TIFF_CFLAGS = $TIFF_CFLAGS

GEOTIFF_LIBS = $GEOTIFF_LIBS
GEOTIFF_CFLAGS = $GEOTIFF_CFLAGS

TCL_TK_LIBS = $TCL_TK_LIBS
TCL_TK_CFLAGS = $TCL_TK_CFLAGS

EOF

# 'EOF' (with quotes) makes cat print out literal strings (no expanded variables [$])
cat <<'EOF' >>system_rules

CFLAGS  := $(CFLAGS) $(FLAGS) $(DEBUGFLAGS)
LDFLAGS := $(LDFLAGS) $(DEBUGLIBS) -lm

EOF

echo "$makeExtra" >>system_rules

# Look for the X-Windows directory
for xdir in `echo /usr /usr/openwin /usr/X /usr/X11 /usr/X11R6`
do
	if [[ -r $xdir/lib/libX11.so -o -r $xdir/lib/libX11.dll.a ]]
	then
		# linux needs to know about the /lib directory
		if [[ $sys = "linux" -o $sys = "win32" ]]
		then
			echo "XHOME   = ${xdir}/lib" >> system_rules
		else
			echo "XHOME   = ${xdir}" >> system_rules
		fi
		break
	fi
done


#echo "########### Building the Makefile ###############"
#make CC=$cc
#./makemake $sys
cd ..

# determine installation prefix
if test "${prefix}" = "NONE" ; then
   prefix="/usr/local"
fi
AC_DEFINE_UNQUOTED(ASF_INSTALL_PREFIX, "${prefix}")

SHAREDIR="$prefix/share"
AC_SUBST(SHAREDIR)

BINDIR="$prefix/bin"
AC_SUBST(BINDIR)

AC_OUTPUT([
Makefile
include/config.h
external/Makefile
external/work_dir/libgeotiff/Makefile
])

echo "########### Automatic configuration complete ###########"
directory=${asfDir}/bin/
echo ""
echo "The ASF tools Makefile is now fully prepared."
echo ""
echo "To build the tools, type 'make'."
echo "To install the tools, type 'make install'."
echo ""
echo "The tools will be installed in this location:"
echo "  "$prefix" "
echo ""
echo "########### Build system is now configured  ############"
