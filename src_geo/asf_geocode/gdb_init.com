
# This file works with a custom emacs lisp debugger integration code
# written by me (Britton Kerin).  Its checked in because I didn't want
# to lose it.  But you may find it useful as example command lines.
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

file asf_geocode
#set args --projection lamaz --latitude-of-origin 65 --central-meridian -145 --height 466 test8192 test_8192_projected_nn
#set args --projection lamaz --latitude-of-origin 65 --central-meridian -145 --height 466 test_data/e1_22590_290_full projected_lamaz
set args --projection lamcc --first-standard-parallel 20 --second-standard-parallel 60 --central-meridian -96 --latitude-of-origin 40 --height 466 test_data/e1_22590_290_full projected_lamcc
#set args --projection utm csr1_18270_262 projected_image
#set args --projection utm --height 0 car1_10054_164 projected_core_dumped
#set args --projection utm --zone 1 --height 0 csr1_11994_266 projected_spans_180
#set args --projection utm --height 466 test_data/e1_22590_290_full projected_image
#set args --projection utm --height 466 test_data/r1_fn1_24869_290_full projected_image
#set args --projection utm --height 466 test_data/r1_st1_32293_160_full projected_utm
#set args --projection ps --first-standard-parallel 70 --central-meridian -45 --north-pole --height 466 test_data/r1_st1_32293_160_full projected_ps

#set args --projection ps --first-standard-parallel -70 --central-meridian 165 -s cse1_23834_660 cse1_23834_660_ps

# Memory debugging:
# valgrind --tool=memcheck --leak-check=yes --show-reachable=yes --leak-resolution=high --num-callers=40 --freelist-vol=100000000 asf_geocode --projection ps --first-standard-parallel -70 --central-meridian 165 -s cse1_23834_660 cse1_23834_660_ps
