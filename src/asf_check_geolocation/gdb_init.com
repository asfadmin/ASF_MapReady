
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

file asf_check_geolocation
set args sri.img nga_srtm3_utm10.img offset sim_amp_test.img slant_dem_test.img
cd /export/apd/rgens/terrain_correction/insar_test/e2_5421_new
