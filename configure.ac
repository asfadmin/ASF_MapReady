AC_PREREQ(2.59)
AC_INIT
# The most recent version of aclocal doesn't seem to work if this
# macro isn't called.
#AM_INIT_AUTOMAKE
AC_LANG(C)

echo "Configuring ASF Tools"

build_gui="yes"
have_gsl="no"
have_pkg_config="yes"
have_glib="no"
have_proj="no"
have_jpeg="no"
have_tiff="no"
have_geotiff="no"
have_shp="no"
try_gtk="yes"
have_gtk="no"
have_fftw="no"
have_png="no"
have_exif="no"
have_xml2="no"
have_gdal="no"
have_hdfeos5="no"
have_hdf5="no"
have_netcdf="no"
have_cunit="no"

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

ext_lib_src_dir=${asfDir}/external/work_dir

#### GSL check ####
AC_CHECK_LIB(gslcblas,main)
AC_CHECK_HEADER(gsl/gsl_errno.h,
  AC_CHECK_LIB(gsl,main,have_gsl="yes",have_gsl="no"), have_gsl="no")
if test "$have_gsl" = "no" ; then
   ASF_LIB_GSL="${ext_lib_src_dir}/libgsl"
   GSL_LIBS="\$(LIBDIR)/libgsl.a \$(LIBDIR)/libgslcblas.a"
else
   GSL_LIBS="-lgsl -lgslcblas"
fi
AC_SUBST(ASF_LIB_GSL)

#### libproj check ####
##### --> always use our own libproj because we have the NAD27
#####     grid shift files, a user's install probably does not
#AC_CHECK_LIB(proj,pj_transform,have_proj="yes",have_proj="no")
have_proj="no"
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
  if test "$GLIB_LIBS" = "" ; then
    GLIB_LIBS="-lglib-2.0"
  fi
fi
AC_SUBST(ASF_GLIB)

#### libjpeg check ####
#AC_CHECK_LIB(jpeg,jpeg_read_header,have_jpeg="yes",have_jpeg="no")
if test "$sys" = "win32" ; then
  if test "$winsys" = "mingw" ; then
    have_jpeg="yes"
  else
    have_jpeg="no"
  fi
fi
if test "$have_jpeg" = "no" ; then
  ASF_LIB_JPEG="${ext_lib_src_dir}/libjpeg"
  JPEG_LIBS="\$(LIBDIR)/libjpeg.a"
else
  JPEG_LIBS="-ljpeg"
fi
AC_SUBST(ASF_LIB_JPEG)

#### libtiff check ####
#AC_CHECK_HEADER(tiff.h,
#  AC_CHECK_LIB(tiff,TIFFGetTagListCount,have_tiff="yes",have_tiff="no"),
#  have_tiff="no")
if test "$sys" = "win32" ; then
  have_tiff="no"
fi
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

#### libpng check ####
AC_CHECK_LIB(png,png_write_image,have_png="yes",have_png="no")
if test "$sys" = "win32" ; then
   have_png="no"
fi
if test "$have_png" = "no" ; then
   ASF_LIB_PNG="${ext_lib_src_dir}/libpng"
   PNG_LIBS="\$(LIBDIR)/libpng.a"
else
   PNG_LIBS="-lpng"
fi
AC_SUBST(ASF_LIB_PNG)

#### libgeotiff check ####
#AC_CHECK_HEADER(geotiff.h,
#  AC_CHECK_LIB(geotiff,main,have_geotiff="yes",have_geotiff="no"),
#  have_geotiff="no")
if test "$sys" = "win32" ; then
  have_geotiff="no"
fi
if test "$have_geotiff" = "no" ; then
   ASF_LIB_GEOTIFF="${ext_lib_src_dir}/libgeotiff"
   GEOTIFF_LIBS="\$(LIBDIR)/libgeotiff.a"
else
   GEOTIFF_LIBS="-lgeotiff"
fi
AC_SUBST(ASF_LIB_GEOTIFF)

#### libshp check ####
##### --> always use our own shapelib because different versions
#####     the user may have installed are not necessarily compatible
#AC_CHECK_LIB(shp,main,have_shp="yes",have_shp="no")
if test "$have_shp" = "no" ; then
   ASF_LIB_SHAPELIB="${ext_lib_src_dir}/shapelib"
   SHAPELIB_LIBS="\$(LIBDIR)/libshp.a"
else
   SHAPELIB_LIBS="-lshp"
fi
AC_SUBST(ASF_LIB_SHAPELIB)

#### libfftw check ####
AC_CHECK_LIB(fftw,main,have_fftw="yes",have_fftw="no")
if test "$have_fftw" = "no" ; then
   ASF_LIB_FFTW="${ext_lib_src_dir}/libfftw"
   FFTW_LIBS="\$(LIBDIR)/libfftw3f.a"
else
   FFTW_LIBS="-lfftw"
fi
AC_SUBST(ASF_LIB_FFTW)

#### gtk 2.4 check ####
if test "$try_gtk" = "yes" ; then
    PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.4.0,
      have_gtk="yes"
      AC_DEFINE(USE_GTK),
      have_gtk="no"
    )
fi

#### libexif check ####
AC_CHECK_LIB(exif,main,have_exif="yes",have_exif="no")
if test "$sys" = "win32" ; then
  have_exif="no"
fi
if test "$have_exif" = "no" ; then
   ASF_LIB_EXIF="${ext_lib_src_dir}/libexif"
   EXIF_LIBS="\$(LIBDIR)/libexif.a"
else
   EXIF_LIBS="-lexif"
fi
AC_SUBST(ASF_LIB_EXIF)

#### libxml2 check ####
AC_CHECK_LIB([xml2],[xmlParseFile],have_xml2="yes",have_xml2="no")
if test "$sys" = "win32" ; then
  have_xml2="yes"
  XML2_LIBS="-L$(GTKWIN32_DIR)/lib -lxml2"
else
  if test "$have_xml2" = "no" ; then
   ASF_LIB_XML2="${ext_lib_src_dir}/libxml2"
   XML2_LIBS="\$(LIBDIR)/libxml2.a"
   XML2_CFLAGS="-I../../include/libxml2"
  else
   XML2_LIBS="-lxml2"
   XML2_CFLAGS="-I/usr/include/libxml2"
  fi
fi
AC_SUBST(ASF_LIB_XML2)

#### libgdal check ####
##### --> always use our own libgdal version
#####     not point in trying to sort out external dependencies
have_gdal="no"
if test "$have_gdal" = "no" ; then
   ASF_LIB_GDAL="${ext_lib_src_dir}/libgdal"
   GDAL_LIBS="\$(LIBDIR)/libgdal.a"
else
   GDAL_LIBS="-lgdal"
fi
AC_SUBST(ASF_LIB_GDAL)

#### libcurl check ####
##### --> always use our own libcurl version
#####     no point in trying to sort out external dependencies
have_curl="no"
if test "$have_curl" = "no" ; then
   ASF_LIB_CURL="${ext_lib_src_dir}/libcurl"
   CURL_LIBS="\$(LIBDIR)/libcurl.a"
else
   CURL_LIBS="-lcurl"
fi
AC_SUBST(ASF_LIB_CURL)

#### libhdf5 check ####
##### --> always use our own libhdf5 version
#####     no point in trying to sort out external dependencies
have_hdf5="no"
if test "$have_hdf5" = "no" ; then
   ASF_LIB_HDF5="${ext_lib_src_dir}/libhdf5"
   HDF5_LIBS="\$(LIBDIR)/libhdf5_hl.a \$(LIBDIR)/libhdf5.a"
else
   HDF5_LIBS="-lhdf5_hl -lhdf5"
fi
AC_SUBST(ASF_LIB_HDF5)

#### libhdfeos check ####
##### --> always use our own libhdfeos version
#####     no point in trying to sort out external dependencies
have_hdfeos5="no"
if test "$have_hdfeos5" = "no" ; then
   ASF_LIB_HDFEOS5="${ext_lib_src_dir}/libhdfeos5"
   HDFEOS5_LIBS="\$(LIBDIR)/libhe5_hdfeos.a \$(LIBDIR)/libGctp.a"
else
   HDFEOS5_LIBS="-lhe5_hdfeos -lGctp"
fi
AC_SUBST(ASF_LIB_HDFEOS5)

#### libnetcdf check ####
##### --> always use our own libnetcdf version
#####     no point in trying to sort out external dependencies
have_netcdf="no"
if test "$have_netcdf" = "no" ; then
   ASF_LIB_NETCDF="${ext_lib_src_dir}/libnetcdf"
   NETCDF_LIBS="\$(LIBDIR)/libnetcdf.a"
else
   NETCDF_LIBS="-lnetcdf"
fi
AC_SUBST(ASF_LIB_NETCDF)

#### libcunit check ####
##### --> always use our own libcunit version
#####     no point in trying to sort out external dependencies
have_cunit="no"
if test "$have_cunit" = "no" ; then
   ASF_LIB_CUNIT="${ext_lib_src_dir}/libcunit"
   CUNIT_LIBS="\$(LIBDIR)/libcunit.a"
else
   CUNIT_LIBS="-lcunit"
fi
AC_SUBST(ASF_LIB_CUNIT)

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
    ASF_GUI="src/asf_convert_gui src/metadata_gui src/asf_view src/proj2proj"
fi
# we always build the GUI on windows, as we provide our own gtk libraries.
if test "$sys" = "win32" ; then
    ASF_GUI="src/asf_convert_gui src/metadata_gui src/asf_view src/proj2proj"
fi
AC_SUBST(ASF_GUI)

if test "$winsys" = "mingw" ; then
  JPEG_LIBS="-L../../external/gtkwin32/lib -ljpeg"
  ZLIB_LIBS="../../lib/libz.a"
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
C99_FLAGS = $c99
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

GTKWIN32_DIR = ../../external/gtkwin32
GTKWIN32_ZIP = ../../external/gtkwin32zip
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

AC_OUTPUT([
Makefile
include/config.h
])

if test -d external ; then
	AC_OUTPUT([
		external/Makefile
		external/work_dir/libgeotiff/Makefile
	])
else
	echo "WARNING: asf_tools/external directory was not found!"
	echo "         Be aware that the build may not complete without"
	echo "         this resource."
fi

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
