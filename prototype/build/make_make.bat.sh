#!/bin/sh
#
# Create the Windows "make.bat" script from a UNIX machine.
# Run this from the top-level directory.

o=`pwd`"/make.bat"

echo "rem ASF C++ Prototype Build Script: generated "`date`" by make_make.bat.sh" > $o
export MAKE_MAKE_OUT_FILE="$o"
export PATH=`pwd`"/$0.dir:$PATH"
echo "Reset PATH to $PATH"

cp build/compile.arch.win32 build/compile.arch
make dll clui all_plugins 
#| tee log
#grep "Running> cl" log | awk -F'>' '{print $2}' >> $o

cat >> $o << EOF
copy 'lib\asf_coredll.dll' bin
echo "All ASF tools built!  Now cd plugins and run"
echo "    ..\bin\clui.exe image_checksum.test"
EOF
