#!/bin/sh

sarviewDir=`pwd`
osType=`uname`
versMajor=`uname -r | awk -F. '{print $1}'`
versMinor=`uname -r | awk -F. '{print $2}'`
 # perhaps a bit overdone, but it conforms with asf_tools/config
case "${osType}:${versMajor}:${versMinor}" in
	IRIX*:5:*)
		sys="irix5x" ;;
	IRIX*:*:*)
		sys="irix"  ;;
	SunOS:4:*)
		sys="sunos" ;;
	SunOS:*:*)
		sys="solaris" ;;
	AIX*:*:*)
		sys="aix" ;;
	ULTRIX:*:*)
		sys="ultrix" ;;
	OSF1:*:*)
		sys="osf1" ;;
	HP-UX:*:*)
		sys="hpux" ;;
	*BSD:*:*)
		sys="bsd" ;;
	GNU:*:*)
		sys="gnu" ;;
	Linux:*:*)
		sys="linux" ;;
	Mach:*:*)
		sys="mach" ;;
	CYGWIN*:*:*)
		sys="win32" ;;
	*:*:*)
		sys=$osType ;;
esac

cd ../../lib/${sys}
libDir=`pwd`
cd $sarviewDir

 # Figure if compiler is cc or gcc 
 # If compiler is gcc tell tcl/tk to use it
compiler=`grep CC ../../make_support/system_rules | awk '{print $3}' | head -1`
if [ ${compiler} = "gcc" ]
then
	enable="--enable-gcc"
else
	enable=""
fi

 # make tcl/tk if necessary
if [ ! -d ${libDir}/tcltk81 ] 
then
	cp tcl8.1.1.tar.Z tk8.1.1.tar.Z $libDir
# Make tcl 8.1
	cd $libDir
	uncompress tcl8.1.1.tar.Z
	tar xvf tcl8.1.1.tar
	cd ./tcl8.1.1/unix
	./configure --prefix=${libDir}/tcltk81 --disable-load $enable
	make
	make install
	cd $libDir
	rm -f tcl8.1.1.tar
# Make tk 8.1
	uncompress tk8.1.1.tar.Z
	tar xvf tk8.1.1.tar
	cd ./tk8.1.1/unix
	./configure --prefix=${libDir}/tcltk81 --disable-load $enable
	make 
	make install
	cd $libDir
	rm -f tk8.1.1.tar
fi
