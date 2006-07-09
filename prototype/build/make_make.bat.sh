#!/bin/sh
#
# Create the Windows "make.bat" script from a UNIX machine.
# Run this from the top-level directory.

OPTS="$1"
o=`pwd`"/make.bat"
rm lib/*.dll

echo "rem ASF C++ Prototype Build Script: generated "`date`" by make_make.bat.sh" > $o
echo "@echo off"
echo "del /Q bin\*" >> $o
echo "del /Q lib\*" >> $o
export MAKE_MAKE_OUT_FILE="$o"
export PATH=`pwd`"/$0.dir:$PATH"
echo "Reset PATH to $PATH"

cp build/compile.arch build/compile.arch.bak
cp build/compile.arch.win32 build/compile.arch
make OPTS="$OPTS" clui TOPDIR_FLAG=""
echo "cd plugins" >> $o
#make OPTS="$OPTS" all_plugins
(cd plugins; ./build_all.sh "$OPTS")
echo "cd ..">>$o

cat >> $o << EOF
del *.obj
copy lib\asf_core.dll bin
echo "All ASF tools built!  Now cd plugins and run"
echo "    ..\bin\clui.exe image_checksum.test"
EOF
cp build/compile.arch.bak build/compile.arch

