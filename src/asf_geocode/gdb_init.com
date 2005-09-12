
# This file works with a custom emacs lisp debugger integration code
# written by me (Britton Kerin).  Its checked in because I didn't want
# to lose it.  But you may find it useful as example command lines.
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

file asf_geocode
set args --projection albers --central-meridian -96.000000 --latitude-of-origin 40.000000 --first-standard-parallel 20.000000 --second-standard-parallel 60.000000 --false-northing 0.000000 --false-easting 0.000000 --datum NAD27 -log 'D:\SAR_data\output\tmpg1448.log' 'D:\SAR_data\output\030705_tmp' 'D:\SAR_data\output\030705'


#set args --projection albers --first-standard-parallel 55 --second-standard-parallel 65 --latitude-of-origin 50 --central-meridian -150 test_data/joanne_basic/test_sfc.A_tmp test_sfc_albers.A

#set args --projection utm --zone 18 test_data/joanne_recode_attempts/from_geocoded_ceos/R141507213U3S006 rcfagc

#set args --projection utm --zone 15 test_data/joanne_recode_attempts/from_not_geocoded_ceos/R129975207G3S010 ncfngc





#set args --projection utm /home/bkerin/asf/asf_tools/src_geo/asf_geocode/test_data/R133387290G1U002 R133387290G1U002_utm
#set args --projection utm --central-meridian -66.000000 --latitude-of-origin -8.000000 --datum WGS84 -log tmpg1676.log R125344879G4S034 R125344879G4S034_utm
#set args --projection ps --first-standard-parallel 70 --central-meridian -45 test_data/R133387290G1U002 ps_direct
#set args --projection ps --first-standard-parallel 70 --central-meridian -45 --north-pole speed_test recoded
# set args --resample-method nearest_neighbor --projection utm test_data/R133387290G1U002 speed_test
#set args --projection lamaz --latitude-of-origin 65 --central-meridian -145 --height 466 test8192 test_8192_projected_nn
#set args --projection lamaz --latitude-of-origin 65 --central-meridian -145 --height 466 test_data/e1_22590_290_full projected_lamaz
#set args --projection lamcc --first-standard-parallel 20 --second-standard-parallel 60 --central-meridian -96 --latitude-of-origin 40 --height 466 test_data/R133387290G1U002 test_lamcc
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
