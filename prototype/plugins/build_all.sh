#!/bin/sh
compile="../build/compile -I../src "$@" ../lib/asf_core.dll "
for f in `echo *.cpp`
do
	out="../lib/$f.dll";
	if [ "$f" -nt "$out" ]
	then
		cmd="$compile $f -o $out"
		echo "$cmd"
		$cmd 
		if [ $? -ne 0 ]
		then
			grep "$f" "compile_errors_ok.txt" || exit 1
		fi
	fi
done
