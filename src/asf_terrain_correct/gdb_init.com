
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

#file ./test_orbital_state_vector
#file ./test_model
file asf_terrain_correct
set args test_data/fail_case_sa/patagonia_utm18_dem_90m test_data/fail_case_sa/R125402776G1S008 output_image
#set args test_data/fail_case/akutan_utm3_dem_30m test_data/fail_case/r1_44978_315 output_image
#set args test_data/dem_over_akutan/akutan_utm3_dem_30m test_data/dem_over_akutan/R144978315G1S003 output_image
