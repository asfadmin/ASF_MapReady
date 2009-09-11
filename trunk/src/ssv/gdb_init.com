
# This is the command file used at gdb startup from emacs for programs
# with source files below the directory in which this file resides.
# Unless a file with the same name as this one appears further down in
# the directory hierarchy.

#file test_graph_depth_first_search
#set args

#file test_table_file
#set detach-on-fork off
#set env EF_PROTECT_BELOW=1
#set env EF_PROTECT_FREE=1
#set env G_DEBUG=gc-friendly
#set env G_SLICE=always-malloc
#set args

#file test_ec_lock
#set args
#cd /home/bkerin/asf_2nd_time/trunk/src/ssv

#file test_pyramid
#set args 
#cd /home/bkerin/asf_2nd_time/trunk/src/ssv

#file test_table_file
#set detach-on-fork off
#set follow-fork-mode child

file ./ssv
set args E116306133G1S009
#set args -m 42 --sigmas=2 E116306133G1S009
#set args -m 42 E116306133G1S009 --offset=-300,-300 E116306133G1S009
set env EF_PROTECT_FREE=1
set env EF_PROTECT_BELOW=1
set env G_DEBUG=gc-friendly
#cd /home/bkerin/asf_2nd_time/trunk/src/ssv

# This would test the remapping functionality, if my code weren't too
# challenging for gdb to handle :)
# set args -p [inf,inf],255 -p [-inf,-inf],0 -p [nan,nan],0 -p [42e26,42e29],255 E116306133G1S009_wonky 