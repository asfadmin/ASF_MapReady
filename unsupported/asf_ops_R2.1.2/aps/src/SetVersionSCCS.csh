#!/bin/csh -f
#
#  Don't use this file again without editing it first.  
#
echo "  Don't use this file again without editing it first.  "
exit

#
#  set up a list of all directorys with SCCS-controlled contents.  
#  use the list in a foreach statement to re-set the version 
#  number to a new number to correspond to r2.1.2
#
#  New number:  5  for r2.1.2
#  Comments for the new version:  Initial version for r2.1.2 January 1998
#
#
#
#  Do these directories now:  
set DIRECTORIES = ( \
  )

foreach dir ( $DIRECTORIES )
    cd /home/aps/r2.1.2
    cd $dir
    echo $dir
    sccs edit -r5 SCCS
    sccs delget SCCS -y"Initial version for r2.1.2 January 1998"
end
exit
#
# Don't do these directories after here:  
#
# NOT DONE YET:  
#
#
# ALREADY DONE:  
./etc/aps_data/framegen/     \
./etc/aps_data/GHA_stoic/     \
./etc/config/     \
./etc/docs/SRD/     \
./etc/db/     \
./etc/db/reports/     \
./etc/db/tables/     \
./etc/db/types/     \
./etc/db/tools/R1_satsensor_recs/     \
./etc/NOTES/ADEOS/     \
./etc/NOTES/TO_DO/     \
./etc/NOTES/CSA/     \
./etc/NOTES/Coverage/     \
./etc/NOTES/DAR/     \
./etc/NOTES/DB/     \
./etc/NOTES/ErrorCode/     \
./etc/NOTES/FileProcessing/     \
./etc/NOTES/IMS/     \
./etc/NOTES/Misc/     \
./etc/NOTES/DL2DTK/     \
./etc/NOTES/manpage/     \
./etc/NOTES/Makefile/     \
./etc/NOTES/Multi_antenna/     \
./etc/NOTES/STOIC_GHA/     \
./etc/NOTES/TimeAndSpace/     \
./etc/NOTES/WFF/     \
./etc/NOTES/framegen/     \
./etc/NOTES/REQQ/     \
./etc/install/app_defaults/     \
./etc/install/sh_scripts/     \
./etc/sh_scripts/     \
./etc/app_defaults/     \
./include/local/     \
./include/global/     \
./src/CSA_dtkf_p/     \
./src/aps_encrypt/     \
./src/asap/     \
./src/cdtkopps/     \
./src/cnomcov/     \
./src/cnomorb/     \
./src/create_files/     \
./src/dtkm_segload/     \
./src/gui/     \
./src/j1_msgn/     \
./src/lib_APS/     \
./src/lib_APSdb/     \
./src/lib_F77APSdb/     \
./src/darstats/     \
./src/lib_llist/     \
./src/lib_sybint/     \
./src/lib_timec/     \
./src/mapr/     \
./src/opln_print/     \
./src/aps2hc/     \
./src/     \
./src/catephm/     \
./src/stoic/     \
./src/lib_APSmath/     \
./src/lib_phase/     \
./src/ODL_dtkf_c/     \
./src/FA_dtkf_c/     \
./src/FA_dtkf_p/     \
./src/lib_interface/     \
./src/lib_fileutils/     \
./src/framegen/     \
./src/lib_dtkm/     \
./src/lib_APSpmfutils/     \
./src/create_sv_files/     \
./src/aps_coverage_window/     \
./src/lib_framegen/     \
./src/archive/     \
./src/ant_roundup/     \
./src/CON_roundup/     \
./src/cnomask/     \
./src/ODL_dtkf_p/     \
./src/aps_mkorbcov/include/     \
./src/aps_mkorbcov/     \
./src/mko0/     \
./src/WOS_tools/     \
./src/write2syslog/     \
./src/lib_multiUser/     \
./src/rev2time/     \
./src/time2rev/     \
./src/get_sequence_number/     \
./src/frame_tools/     \
./src/DTK_tools/     \
./src/aps_upd_DAR_stat/     \
./src/DAR2DTK/    \
