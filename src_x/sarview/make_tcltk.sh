#!/bin/sh

if [ $# != 2 ]
then
	echo "USAGE:"
	echo "   make_tcltk.sh <C_compiler> <asf_lib_dir>"
	echo ""
	echo "   <asf_lib_dir>  Directory where the asf libraries are kept."
	echo "   <C_compiler>   C language compiler being used (cc or gcc)."
	echo ""
	echo "   Builds the Tcl/Tk 8.1 libraries for SARview."
	echo "   This program must be run in the sarview directory."
	echo ""
	exit 1
fi

c_compiler=$1
sarviewDir=`pwd`
cd $2
 #get absolute path
libDir=`pwd`

 # If compiler is gcc tell tcl/tk to use it
if [ $c_compiler = "gcc" ]
then
	enable="--enable-gcc"
else
	enable=""
fi

 # make tcl/tk if necessary
if [ ! -d ${libDir}/tcltk81 ] 
then
	cp ${sarviewDir}/tcl8.1.1.tar.Z $libDir
# Make tcl 8.1
	echo "Decompressing & untarring..."
	if [ `which uncompress` ]
	then
		cd $libDir
		uncompress tcl8.1.1.tar.Z
	elif [ `which gunzip` ]
	then
		cd $libDir
		gunzip tcl8.1.1.tar.Z
	else
		echo "Neither uncompress nor gunzip are available, cannot decompress data... exiting."
		exit 1
	fi
	tar xf ${libDir}/tcl8.1.1.tar
	cd ${libDir}/tcl8.1.1/unix
	./configure --prefix=${libDir}/tcltk81 --disable-load $enable
	make
	make install
	rm -f ${libDir}/tcl8.1.1.tar
# Make tk 8.1
	cp ${sarviewDir}/tk8.1.1.tar.Z $libDir
	echo "Decompressing & untarring..."
	if [ `which uncompress` ]
	then
		cd $libDir
		uncompress tk8.1.1.tar.Z
	else
		cd $libDir
		gunzip tk8.1.1.tar.Z
	fi
	tar xf ${libDir}/tk8.1.1.tar
	cd ${libDir}/tk8.1.1/unix
	./configure --prefix=${libDir}/tcltk81 --disable-load $enable
	make 
	make install
	rm -f ${libDir}/tk8.1.1.tar
# Cleanup temporary directories
	echo "Removing temporary directories..."
	rm -rf ${libDir}/tcl8.1.1
	rm -rf ${libDir}/tk8.1.1

	echo "Done installing Tcl/Tk 8.1.1!"
fi
