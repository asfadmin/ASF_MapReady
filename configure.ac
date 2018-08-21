AC_PREREQ(2.59)
AC_INIT
config_flags="$*";

# The most recent version of aclocal doesn't seem to work if this
# macro isn't called.
#AM_INIT_AUTOMAKE
AC_LANG(C)

echo "Configuring ASF Tools"

build_gui="yes"
have_gsl="yes"
have_pkg_config="yes"
have_glib="yes"
have_proj="yes"
have_jpeg="yes"
have_tiff="yes"
have_geotiff="yes"
have_shp="yes"
try_gtk="yes"
have_gtk="yes"
have_fftw="yes"
have_png="yes"
have_exif="yes"
have_xml2="yes"
have_gdal="yes"
have_hdfeos5="yes"
have_hdf5="yes"
have_netcdf="yes"
have_cunit="yes"
w_error=""

if test "${config_flags#*no-werror}" != "${config_flags}" ; then
   w_error=""
fi

AC_PROG_CC
AC_PROG_LEX
AC_PROG_YACC

CC="gcc"
osType=`uname`
nodeName=`uname -n`
versMajor=`uname -r | awk -F. '{print $1}'`
versMinor=`uname -r | awk -F. '{print $2}'`
c99="-D_ISOC99_SOURCE"

ranlib="@echo do not need to run ranlib on this system on file: "
optFlag="-O4"
case "${osType}:${versMajor}:${versMinor}" in
	SunOS:*:*)
		sys="solaris" ;;
	Darwin:*:*)
		sys="darwin" ;;
	Linux:*:*)
		sys="linux" ;;
	CYGWIN*:*:*)
		sys="win32"
		winsys="cygwin"
		makeExtra="BIN_POSTFIX = .exe" ;;
	MINGW*:*:*)
		sys="win32"
		winsys="mingw"
		makeExtra="BIN_POSTFIX = .exe" ;;
	IRIX*:*:*)
		sys="irix"
		if [ "$CC" == "cc" ]; then
			optFlag="$optFlag -n32"
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

# Make the install directories for man pages (necessary for older tools)
mkdir -p man/cat1
mkdir -p man/man1

asfDir=`pwd`

# basic libraries check
AC_CHECK_LIB(c,fopen)
AC_CHECK_LIB(m,main)
AC_CHECK_HEADERS(unistd.h)

PKG_PROG_PKG_CONFIG

AC_DEFUN([ASF_FIND_INCLUDE],
	 AC_MSG_CHECKING(for $2 in /usr/include)
	 [header=$(find /usr/include -name $2|head -1)]
	 AS_IF([test -n $header],
	       AC_MSG_RESULT([$header])
	       AC_CHECK_HEADER($header,
			       $1_CFLAGS=-I$(dirname $header),
			       AC_MSG_ERROR($1 headers were not found)),
	       AC_MSG_RESULT(no)
	       AC_MSG_ERROR($1 headers were not found)))

#### GSL check ####
AC_CHECK_LIB([m],[cos])
AC_CHECK_LIB([gslcblas],[cblas_dgemm])
AC_CHECK_LIB([gsl],[gsl_blas_dgemm])
PKG_CHECK_MODULES(GSL, gsl,,
		  AC_CHECK_LIB(gsl, gsl_blas_dgemm,
		   	       [GSL_LIBS="-lgsl -lm -lgslcblas"],
			       AC_MSG_ERROR(library gsl was not found)))

#### libproj check ####
#AC_CHECK_LIB(proj,pj_transform,have_proj="yes",have_proj="no")
PKG_CHECK_MODULES(PROJ, proj,,
		  AC_CHECK_LIB(proj, pj_transform,
		   	       [PROJ_LIBS=-lproj],
			       AC_MSG_ERROR(library proj was not found)))
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
PKG_CHECK_MODULES(GLIB, glib-2.0,,
		 AC_CHECK_LIB(glib-2.0, main,
		  	      [GLIB_LIBS=-lglib-2.0],
			      AC_MSG_ERROR(library glib-2.0 was not found)))

#### libjpeg check ####
#AC_CHECK_LIB(jpeg,jpeg_read_header,have_jpeg="yes",have_jpeg="no")
PKG_CHECK_MODULES(JPEG, jpeg,,
		 AC_CHECK_LIB(jpeg, jpeg_read_header,
		  	      [JPEG_LIBS=-ljpeg],
			      AC_MSG_ERROR(library jpeg was not found)))

#### libtiff check ####
#AC_CHECK_HEADER(tiff.h,
#  AC_CHECK_LIB(tiff,TIFFGetTagListCount,have_tiff="yes",have_tiff="no"),
#  have_tiff="no")
PKG_CHECK_MODULES(TIFF, libtiff-4,,
		 AC_CHECK_LIB(tiff, TIFFGetTagListCount,
		  	      [TIFF_LIBS=-ltiff],
			      AC_MSG_ERROR(library tiff was not found)))

#### libpng check ####
PKG_CHECK_MODULES(PNG, libpng,,
		 AC_CHECK_LIB(png, main,
		  	      [PNG_LIBS=-lpng],
			      AC_MSG_ERROR(library png was not found)))

#### libgeotiff check ####
#AC_CHECK_HEADER(geotiff.h,
#  AC_CHECK_LIB(geotiff,main,have_geotiff="yes",have_geotiff="no"),
#  have_geotiff="no")
PKG_CHECK_MODULES(GEOTIFF, geotiff,,
		 AC_CHECK_LIB(geotiff, main,
		  	      GEOTIFF_LIBS=-lgeotiff,
			      AC_MSG_ERROR(library geotiff was not found))
		 ASF_FIND_INCLUDE(GEOTIFF, xtiffio.h))

#### libshp check ####
#AC_CHECK_LIB(shp,main,have_shp="yes",have_shp="no")
PKG_CHECK_MODULES(SHAPELIB, shapelib,,
		  AC_CHECK_LIB(shp, main,
			       [SHAPELIB_LIBS=-lshp],
			       AC_MSG_ERROR(library shapelib was not found))
		  ASF_FIND_INCLUDE(SHAPELIB, shapefil.h))

#### libfftw check ####
#AC_CHECK_LIB(fftw,main,have_fftw="yes",have_fftw="no")
PKG_CHECK_MODULES(FFTW, fftw3,,
		  AC_CHECK_LIB(fftw, main,
			       [FFTW_LIBS=-lfftw],
			       AC_MSG_ERROR(library fftw was not found)))

#### gtk 2.4 check ####
PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.4.0, [have_gtk=yes]
		  			  AC_DEFINE(USE_GTK),
		  [have_gtk=no])

#### libxml2 check ####
PKG_CHECK_MODULES(XML2, libxml-2.0,,
		  AC_CHECK_LIB(xml2, xmlParseFile,
			       [XML2_LIBS=-lxml2],
			       AC_MSG_ERROR(library xml2 was not found))
		  ASF_FIND_INCLUDE(XML2, xmlIO.h))

#### libgdal check ####
PKG_CHECK_MODULES(GDAL, gdal,,
		  AC_CHECK_LIB(gdal, main,
			       [GDAL_LIBS=-lgdal],
			       AC_MSG_ERROR(library gdal was not found))
		  ASF_FIND_INCLUDE(GDAL, gdal.h))

#### libcurl check ####
PKG_CHECK_MODULES(CURL, libcurl,,
		  AC_CHECK_LIB(curl, main,
			       [CURL_LIBS=-lcurl],
			       AC_MSG_ERROR(library curl was not found)))

#### libhdf5 check ####
PKG_CHECK_MODULES(HDF5, hdf5,,
		  AC_CHECK_LIB(hdf5, main,
			       [HDF5_LIBS=-lhdf5],
			       AC_MSG_ERROR(library hdf5 was not found)))

#### libnetcdf check ####
PKG_CHECK_MODULES(NETCDF, netcdf,,
		  AC_CHECK_LIB(netcdf, main,
			       [NETCDF_LIBS=-lnetcdf],
			       AC_MSG_ERROR(library netcdf was not found)))

#### libcunit check ####
PKG_CHECK_MODULES(CUNIT, cunit,,
		  AC_CHECK_LIB(cunit, main,
			       [CUNIT_LIBS=-lcunit],
			       AC_MSG_ERROR(library cunit was not found))
		  ASF_FIND_INCLUDE(CUNIT, CUnit.h))

#### libglade check ####
if test "$have_gtk" = "yes" ; then
   PKG_CHECK_MODULES(LIBGLADE, libglade-2.0 > 2.0.0,
       have_libglade="yes", have_libglade="no")
else
   echo
   echo
   echo " ********************************************************************************"
   echo "**********************************************************************************"
   echo "****                                                                          ****"
   echo "****  WARNING: GTK v2.4.0 or higher does not appear to exist.                 ****"
   echo "****    Applications with graphical user interfaces that require GTK          ****"
   echo "****    will not be built:                                                    ****"
   echo "****                                                                          ****"
   echo "****      mapready      (The main MapReady application)                       ****"
   echo "****      mdv           (Metadata viewer)                                     ****"
   echo "****      asf_view      (Data and graphics file viewer)                       ****"
   echo "****      proj2proj     (Lat/Long and projection coordinates conversion tool) ****"
   echo "****                                                                          ****"
   echo "****    Command line tools will be created.  The following command line       ****"
   echo "****    versions of the tools listed above ARE available:                     ****"
   echo "****                                                                          ****"
   echo "****      asf_mapready                                                        ****"
   echo "****      metadata                                                            ****"
   echo "****      (no command line replacement for asf_view)                          ****"
   echo "****      (no command line replacement for proj2proj)                         ****"
   echo "****                                                                          ****"
   echo "**********************************************************************************"
   echo " ********************************************************************************"
   echo
   echo
   echo
   echo
fi

#### whether or not to build the ASF Convert GUI ####
if test "$have_libglade" = "yes" ; then
    ASF_GUI="src/mapready src/metadata_gui src/asf_view src/proj2proj"
fi
# we always build the GUI on windows, as we provide our own gtk libraries.
if test "$sys" = "win32" ; then
    ASF_GUI="src/mapready src/metadata_gui src/asf_view src/proj2proj"
fi
AC_SUBST(ASF_GUI)

if test "$winsys" = "mingw" ; then
  JPEG_LIBS="-ljpeg"
  ZLIB_LIBS="-lz"
else
  ZLIB_LIBS=-lz
fi

echo "########## Determining machine characteristics #############"
cd make_support
$CC -o endian endian.c
./endian

#echo "########### Building the make_support/system_rules file ###############"

# write make_support/system_rules file header
cat <<EOF >system_rules
############### System_rules: ###############
# Contains information about the current machine.
# This file was automatically created by the ASF tools 'configure' script
# It is included by each program's Makefile.
#

EOF

# write make_support/system_rules ENDIAN_FLAGS
./endian -r >> system_rules
rm -f ./endian

# EOF (without quotes) makes cat print out strings with expanded variables
cat <<EOF >>system_rules
SYS       =   $sys
NODENAME  =   $nodeName
WINSYS    =   $winsys
CC        =   $CC
RANLIB    =   $ranlib
OPTFLAG   =   $optFlag
C99_FLAGS =   $c99
W_ERROR   =   $w_error
VER       =   -DMAKE_VER
EOF

# 'EOF' (with quotes) makes cat print out literal strings (no expanded variables [$])
cat <<'EOF' >>system_rules

# Get the name of the package to build from build_pkg.mk which is
# generated by the top level Makefile
# build_pkg.mk is generated by the top level Makefile
-include ../../../make_support/build_pkg.mk
-include ../../make_support/build_pkg.mk

ASF_INCLUDE_DIR = ../../include

# LARGEFILE_FLAGS must precede all include options.
LARGEFILE_FLAGS = -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64
INCLUDE_FLAGS   = -I. -I$(ASF_INCLUDE_DIR)
SYS_FLAGS       = -D$(SYS)

WIN32_FLAGS =
ifeq ($(SYS),win32)
  WIN32_FLAGS = -D$(WINSYS)
endif

# If compiler is gcc we're going to add some debugging flags/libraries
#  if DEBUG_BUILD=1 apply debugging tags & show all compiler warnings
#  if DEBUG_BUILD=2 do the same & add the electric fence library
#  if DEBUG_BUILD=3 debugging tags, compiler warnings, & use our default optflag
ifeq ($(CC),gcc)
  ifeq ($(DEBUG_BUILD),1)
    OPTFLAG    = -O0
    DEBUGFLAGS = -Wall -g3
    DEBUGLIBS  =
  endif
  ifeq ($(DEBUG_BUILD),2)
    OPTFLAG    = -O0
    DEBUGFLAGS = -Wall -g3
    DEBUGLIBS  = -lefence
  endif
  ifeq ($(DEBUG_BUILD),3)
    DEBUGFLAGS = -Wall -g3
  endif
endif

LIBDIR  = ../../lib
BINDIR  = ../../bin
DOCDIR = ../../share/asf_tools/doc

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

PNG_LIBS = $PNG_LIBS
PNG_CFLAGS = $PNG_CFLAGS

FFT_LIBS = $FFTW_LIBS
FFT_CFLAGS = $FFTW_CFLAGS

GEOTIFF_LIBS = $GEOTIFF_LIBS
GEOTIFF_CFLAGS = $GEOTIFF_CFLAGS

SHAPELIB_LIBS = $SHAPELIB_LIBS
SHAPELIB_CFLAGS = $SHAPELIB_CFLAGS

EXIF_LIBS = $EXIF_LIBS
EXIF_CFLAGS = $EXIF_CFLAGS

XML_LIBS = $XML2_LIBS
XML_CFLAGS = $XML2_CFLAGS

CURL_LIBS = $CURL_LIBS
CURL_CFLAGS = $CURL_CFLAGS

HDF5_LIBS = $HDF5_LIBS
HDF5_CFLAGS = $HDF5_CFLAGS

HDFEOS5_LIBS = $HDFEOS5_LIBS
HDFEOS5_CFLAGS = $HDFEOS5_CFLAGS

NETCDF_LIBS = $NETCDF_LIBS
NETCDF_CFLAGS = $NETCDF_CFLAGS

CUNIT_LIBS = $CUNIT_LIBS
CUNIT_CFLAGS = $CUNIT_CFLAGS

GDAL_LIBS = $GDAL_LIBS
GDAL_CFLAGS = $GDAL_CFLAGS

ZLIB_LIBS = $ZLIB_LIBS

EOF

# 'EOF' (with quotes) makes cat print out literal strings (no expanded variables [$])
cat <<'EOF' >>system_rules

CFLAGS := \
	$(OPTFLAG) \
	$(DEBUGFLAGS) \
	$(LARGEFILE_FLAGS) \
	$(C99_FLAGS) \
	$(SYS_FLAGS) \
	$(WIN32_FLAGS) \
	$(BUILD_PKG) \
	$(ENDIAN_FLAGS) \
	$(INCLUDE_FLAGS) \
	$(VER) \
	$(CFLAGS)

LDFLAGS := $(LDFLAGS) $(DEBUGLIBS) -lm

EOF

echo "$makeExtra" >>system_rules

#echo "########### Building the Makefile ###############"
cd ..

# determine installation prefix
if test "${prefix}" = "NONE" ; then
   prefix="/usr/local"
fi
AC_DEFINE_UNQUOTED(ASF_INSTALL_PREFIX, "${prefix}")

SHAREDIR="$prefix/share/asf_tools"
AC_SUBST(SHAREDIR)

BINDIR="$prefix/bin"
AC_SUBST(BINDIR)

DOCDIR="$prefix/doc"
AC_SUBST(DOCDIR)

TMPDIR="/tmp"
AC_SUBST(TMPDIR)

AC_OUTPUT([
Makefile
include/config.h
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
