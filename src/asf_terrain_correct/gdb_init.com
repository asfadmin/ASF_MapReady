
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

#file ./test_orbital_state_vector
#file ./test_model
file ../../bin/asf_terrain_correct
#set args  nga_srtm3_utmz10_90m E205421306G1S005 p
set args brooks_utmz06 E122267173G1S003 p

