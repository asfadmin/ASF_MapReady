
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

file test_float_image_statistics
set args
#cd pathname_to_change_to
