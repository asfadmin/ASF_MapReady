AC_INIT
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
try_gtk="yes"
have_gtk="yes"

AC_PROG_CC

cc="cc"
osType=`uname`
versMajor=`uname -r | awk -F. '{print $1}'`
versMinor=`uname -r | awk -F. '{print $2}'`

ranlib="@echo do not need to run ranlib on this system on file: "
optFlag="-O"
case "${osType}:${versMajor}:${versMinor}" in
	IRIX*:*:*)
		sys="irix"
		optFlag="-O -n32";;
	SunOS:*:*)
		sys="solaris" ;;
	Linux:*:*)
		sys="linux" ;;
	CYGWIN*:*:*)
		sys="win32"
		makeExtra="BIN_POSTFIX = .exe" ;;
	*:*:*)
		echo "ASF tools are only supported on IRIX, Solaris, Linux, and Windows (Cygwin)"
		echo "Your system appears to be '" $osType "'."
		echo "Exiting..."
		exit 1 ;;
esac

AC_SUBST(sys)

# Make the install directories for the libraries and binaries.
mkdir -p lib/$sys
mkdir -p bin/$sys
mkdir -p java
mkdir -p share

######## Ask about GCC ############
#if [[ ! $cc = "gcc" -a ! $batch_mode = "true" ]]; then
#    printf "Would you like to use the GNU C Compiler 'gcc' (y/n)? "
#    read resp
#    if [[ $resp = "y" -o $resp = "Y" -o $resp = "yes" ]]; then
#        cc="gcc"
#	optFlag="-O"
#    fi
#else
#    cc="gcc"
#    optFlag="-O4"
#fi

asfDir=`pwd`

# basic libraries check
AC_CHECK_LIB(c,fopen)
AC_CHECK_LIB(m,main)
AC_CHECK_HEADERS(unistd.h)

# GSL
AC_CHECK_LIB(gslcblas,main)
AC_CHECK_LIB(gsl,main,have_gsl="yes",have_gsl="no")

if test "$have_gsl" = "no" ; then
   ASF_LIB_GSL="src_lib/libgsl"
   GSL_LIBS="\$(LIBDIR)/libgsl.a"
else
   GSL_LIBS="-lgsl -lgslcblas"
fi

AC_SUBST(ASF_LIB_GSL)

# Proj
AC_CHECK_LIB(proj,pj_transform,have_proj="yes",have_proj="no")

if test "$have_proj" = "no" ; then
   ASF_LIB_PROJ="src_lib/libproj"
   PROJ_LIBS="\$(LIBDIR)/libproj.a"
else
   PROJ_LIBS="-lproj"
fi

AC_SUBST(ASF_LIB_PROJ)

# check for pkg-config
if test -z "$PKG_CONFIG"; then
   AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
fi
if test "$PKG_CONFIG" = "no" ; then
   echo "*** pkg-config not found, can't build with gtk 2.0" 
   $try_gtk="no"
   $have_pkg_config="no"
fi

if test "$have_pkg_config" = "no" ; then
   ASF_PKG_CONFIG="src/pkgconfig"
fi

AC_SUBST(ASF_PKG_CONFIG)

# glib check
if test "$have_pkg_config" = "yes" ; then
   PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.4.0,
      have_glib="yes", have_glib="no")
fi

if test "$have_glib" = "no" ; then
   ASF_GLIB="src_lib/libglib-2.0"
   GLIB_LIBS="\$(LIBDIR)/libglib-2.0.a \$(LIBDIR)/libiconv.a"
else
   GLIB_LIBS="-lglib-2.0"
fi

AC_SUBST(ASF_GLIB)

# gtk 2.4 check
orig_CFLAGS=$CFLAGS
if test "$try_gtk" = "yes" ; then
    PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.4.0,
      CFLAGS="$CFLAGS $GTK_CFLAGS"
      LIBS="$LIBS $GTK_LIBS"
      have_gtk="yes"
      AC_DEFINE(USE_GTK),
      have_gtk="no"      
    )
fi

# libglade check
if test "$have_gtk" = "yes" ; then
   PKG_CHECK_MODULES(LIBGLADE, libglade-2.0 > 2.0.0,
       have_libglade="yes", have_libglade="no")
fi

if test "$have_libglade" = "yes" ; then
    ASF_GUI="src/asf_convert_gui"
fi

AC_SUBST(ASF_GUI)

echo "########## Determining machine characteristics #############"
cd make_support
$CC -o endian endian.c
./endian

echo "########### Building the make_support/system_rules file ###############"

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

CFLAGS = $CLFAGS
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
    DEBUGFLAGS = -Wall -g3
  endif
  ifeq ($(DEBUG_BUILD),2)
    OPTFLAG    =
    DEBUGFLAGS = -Wall -g3
    DEBUGLIBS  = -lefence
  endif
endif

LIBDIR  = ../../lib/$(SYS)
BINDIR  = ../../bin/$(SYS)
JAVADIR = ../../java

EOF

# EOF (without quotes) makes cat print out strings with expanded variables
cat <<EOF >>system_rules

GSL_LIBS = $GSL_LIBS
GSL_CFLAGS = $GSL_CFLAGS

GLIB_LIBS = $GLIB_LIBS
GLIB_CFLAGS = $GLIB_CFLAGS

PROJ_LIBS = $PROJ_LIBS
PROJ_CFLAGS = $PROJ_CFLAGS

EOF

# 'EOF' (with quotes) makes cat print out literal strings (no expanded variables [$])
cat <<'EOF' >>system_rules

CFLAGS  := $(CFLAGS) $(FLAGS) $(DEBUGFLAGS) $(GLIB_CFLAGS)
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

echo "########### Automatic configuration complete ###########"
directory=$asfDir'/bin/'$sys'/'
echo ""
echo "The ASF tools Makefile is now fully prepared."
echo ""
echo "To use the programs, you must add the binaries "
echo "  directory to your path: "$directory" "
echo ""
echo "To build the tools, type 'make'."
echo ""
echo "########### Build system is now configured  ############"

AC_OUTPUT([Makefile])


