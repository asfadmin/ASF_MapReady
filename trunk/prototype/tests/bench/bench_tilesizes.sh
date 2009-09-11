#!/bin/sh
#
# Run some timing benchmark tests, varying:
#    - The size of the image (benchsize)
#    - The number of compute passes over the image (p)
#    - The size of the tiles used (tilesize)

run="../../bin/clui"

LD_LIBRARY_PATH="$LD_LIBRARY_PATH:../../lib"
export LD_LIBRARY_PATH

Time() {
	name="$1"
	shift
	echo -n "$name "
	e="tmp/errs.$$"
	(time -p "$@" ) 2> $e 1>/dev/null 
	if [ $? -ne 0 ]
	then
		echo "Error executing $@"
		cat $e
		exit 1
	fi
	grep '^real' $e \
		| awk '{
			printf("%.2f ns/pixel\n", 
				1.0e9*$2/('$benchsize'*'$benchsize'));
		}'
}
Verbose() {
	echo "$@"
	"$@" || exit 1
}


for benchsize in 1000 4000 10000
do
	echo "Benchmark size: $benchsize x $benchsize"

	mkdir -p tmp
	sed -e 's/xxxx/'$benchsize'/g' < ./benchfile_create.clu > tmp/create.clu
	chmod +x tmp/create.clu
	Time "  Creating test file on disk..." tmp/create.clu

	# Powers of 4 (coarse idea)
	testsizes="4 16 64 256 1024 4096 16000  100000 "

	# Powers of 1.4 (gory details)
	#testsizes="1 2 3 4 6 8 12 16 24 32 48 64 96 128 192 256 384 512 768 1024 1500 2048 3000 4000 6000 100000"

	for p in 0 1 2 6 10
	do
		sync; sleep 1

		echo "  Trying $p passes for various tile sizes..."
		for tilesize in $testsizes
		do
			Time "    bts.sh: $p passes, $benchsize image, $tilesize tile, " "$run" -t "$tilesize" ./stream_clamp$p.clu
		done  # tilesize

	done # passes
	rm -fr tmp
done # benchsize
