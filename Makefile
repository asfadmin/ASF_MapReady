# Makefile for          :  All ASF-STEP Tools 
# Module Author(s)      :  asf_tools/make_support/makemake
# Makefile Date         :  6/99
#
#        The Zen and the Art of ASF STEP Makefiles:
#
#    Overview: a "makefile" is a sort of script which is
# read by the UNIX "make" utility.  The purpose of a 
# makefile is to quickly and automatically compile a 
# piece of software.  For the ASF STEP tools, there is 
# one makefile in each tool's directory, and one central 
# makefile (this file) in the asf_tools directory.  
#
#    The individual tools' makefiles "include" 
# make_support/system_rules to get the parameters they
# need (endian-ness, etc.)
#
#    If you're a user, your life is simple: always make 
# everything from the asf_tools directory, by typing:
#        make <toolname>
#
#    If you're an ASF programmer, however, since you're
# working on these tools, your life is (of course) 
# more complex.  When you modify a tool, you don't 
# need to change anything but that tool's makefile.  
# But if you add a tool, or change a tool's name, 
# you need to modify several other files, as well.
#
#    I (Orion Lawlor) have written a program which should
# make this process much easier for you.  It's in
# asf_tools/make_support/makemake, and it parses a file called
# asf_tools/make_support/master_program_list to generate the
# top-level makefile, as well as update our
# autotar directory.  To add a program or tool, just modify
# asf_tools/make_support/master_program_list, then run
# asf_tools/make_support/makemake.
#
#    To debug software, I suggest you make an alias like:
# alias Mag 'make OPTFLAG=-g'
# This will re-set the "OPTFLAG" variable to "-g" from "-O".
#
# Questions?
#  Contact   Tom Logan: tlogan@images.alaska.edu
#       or Rick Guritz: rguritz@images.alaska.edu
#
###########################################
include make_support/system_rules
MAKE = make


#####################################################################
# Listing of all step tools.  This file is automagically generated. #
#####################################################################

B = bin/$(SYS)
L = lib/$(SYS)
FILES=\
	bin_create\
	$(L)/asf.a\
	$(L)/asf_meta.a\
	$(L)/asf_las.a\
	$(L)/asf_concat.a\
	$(L)/asf_lt.a\
	$(L)/asf_geolib.a\
	$(L)/asf_odl.a\
	$(L)/asf_fft.a\
	$(L)/libjpeg.a\
	$(L)/libifm.a\
	$(L)/sarmodel.a\
	$(L)/tcobj.a\
	$(B)/calibrate\
	$(B)/ceosui2byte\
	$(B)/convert_geo_ddr\
	$(B)/diff_las\
	$(B)/display_pdr\
	$(B)/dspddr\
	$(B)/dump_multi_volume\
	$(B)/fftMatch\
	$(B)/get_rectypes\
	$(B)/las2jpeg\
	$(B)/las2ppm\
	$(B)/las_op\
	$(B)/las_ramp\
	$(B)/las_text\
	$(B)/makeddr\
	$(B)/mark_tpl\
	$(B)/metadata\
	$(B)/remap\
	$(B)/resample\
	$(B)/sarin\
	$(B)/sarout\
	$(B)/sr2gr\
	$(B)/ssar_crop\
	$(B)/trim\
	$(B)/accum_offset\
	$(B)/concat\
	$(B)/concat_dem\
	$(B)/concatm\
	$(B)/fit_quadratic\
	$(B)/idimage\
	$(B)/projprm\
	$(B)/psconv\
	$(B)/twoway\
	$(B)/geocode\
	$(B)/auto_sar_mosaic\
	$(B)/auto_swath_mosaic\
	$(B)/aisp\
	$(B)/amp2img\
	$(B)/avg_in_dop\
	$(B)/c2p\
	$(B)/coh\
	$(B)/cpx_spectrum\
	$(B)/cpx_autofilter\
	$(B)/deskew_dem\
	$(B)/reskew_dem\
	$(B)/deramp\
	$(B)/elev\
	$(B)/eleverr\
	$(B)/escher\
	$(B)/fico\
	$(B)/fit_line\
	$(B)/fit_plane\
	$(B)/fit_warp\
	$(B)/fix_in\
	$(B)/fix_in_fromraw\
	$(B)/igram\
	$(B)/ceos2raw\
	$(B)/lz2raw_flywheel\
	$(B)/ml\
	$(B)/p2c\
	$(B)/phase_filter\
	$(B)/resolve\
	$(B)/refine_base\
	$(B)/refine_offset\
	$(B)/trim_slc\
	$(B)/demIFM\
	$(B)/register_slc\
	$(B)/register_ccsd\
	$(B)/tandem_ifm\
	$(B)/register_lzceos\
	$(B)/register_lzraw\
	$(B)/swath_offset\
	$(B)/2dmap\
	$(B)/demimg\
	$(B)/demclip\
	$(B)/despike\
	$(B)/greycorr\
	$(B)/sargeom\
	$(B)/sarsim\
	$(B)/rtc_add\
	$(B)/tpl_mult\
	$(B)/tpl_search\
	$(B)/correlate\
	$(B)/terrcorr\
	$(B)/las_stack\
	$(B)/sdts2las\
	$(B)/usgs2las\
	$(B)/propagate\
	$(B)/sarview\
	done



#####################################################################
# Make Rules-- all and clean.  This file is automagically generated.#
#####################################################################
all:	$(FILES)

done:
	@ echo
	@ echo XXXXXXXXXXXXXX   ALL STEP TOOLS COMPILED   XXXXXXXXXXXXX
	@ echo

clean:
	@ echo
	@ echo XXXXXXXXXXXXXX   REMOVING ALL BINARIES   XXXXXXXXXXX
	@ echo
	- rm -f $(L)/*
	- rm -f $(B)/*
	- rm -fr man/*

man_create:
	@ test -d man || mkdir man
	@ test -d man/cat1 || mkdir man/cat1
	@ test -d man/man1 || mkdir man/man1

lib_create: 	
	@ test -d man || mkdir man
	@ test -d man/cat1 || mkdir man/cat1
	@ test -d man/man1 || mkdir man/man1
	@ test -d lib || mkdir lib
	@ test -d $(L) || mkdir $(L)

bin_create :	
	@ test -d man || mkdir man
	@ test -d man/cat1 || mkdir man/cat1
	@ test -d man/man1 || mkdir man/man1
	@ test -d lib || mkdir lib
	@ test -d $(L) || mkdir $(L)
	@ test -d bin || mkdir bin
	@ test -d $(B) || mkdir $(B)

#####################################################################
# LIBRARIES:                                                        #
#####################################################################

################################### Library asf ######################################
asf.a:	lib_create clean_asf $(L)/asf.a

clean_asf:
	-@ test -r development.directory || rm -f $(L)/asf.a

$(L)/asf.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf.a library... XXXXXXX
	@ echo
	cd src_lib/asf;$(MAKE)

################################### Library asf_meta ######################################
asf_meta.a:	lib_create clean_asf_meta $(L)/asf_meta.a

clean_asf_meta:
	-@ test -r development.directory || rm -f $(L)/asf_meta.a

$(L)/asf_meta.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_meta.a library... XXXXXXX
	@ echo
	cd src_lib/asf_meta;$(MAKE)

################################### Library asf_las ######################################
asf_las.a:	lib_create clean_asf_las $(L)/asf_las.a

clean_asf_las:
	-@ test -r development.directory || rm -f $(L)/asf_las.a

$(L)/asf_las.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_las.a library... XXXXXXX
	@ echo
	cd src_lib/asf_las;$(MAKE)

################################### Library asf_concat ######################################
asf_concat.a:	lib_create clean_asf_concat $(L)/asf_concat.a

clean_asf_concat:
	-@ test -r development.directory || rm -f $(L)/asf_concat.a

$(L)/asf_concat.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_concat.a library... XXXXXXX
	@ echo
	cd src_lib/asf_concat;$(MAKE)

################################### Library asf_lt ######################################
asf_lt.a:	lib_create clean_asf_lt $(L)/asf_lt.a

clean_asf_lt:
	-@ test -r development.directory || rm -f $(L)/asf_lt.a

$(L)/asf_lt.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_lt.a library... XXXXXXX
	@ echo
	cd src_lib/asf_lt;$(MAKE)

################################### Library asf_geolib ######################################
asf_geolib.a:	lib_create clean_asf_geolib $(L)/asf_geolib.a

clean_asf_geolib:
	-@ test -r development.directory || rm -f $(L)/asf_geolib.a

$(L)/asf_geolib.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_geolib.a library... XXXXXXX
	@ echo
	cd src_lib/asf_geolib;$(MAKE)

################################### Library asf_odl ######################################
asf_odl.a:	lib_create clean_asf_odl $(L)/asf_odl.a

clean_asf_odl:
	-@ test -r development.directory || rm -f $(L)/asf_odl.a

$(L)/asf_odl.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_odl.a library... XXXXXXX
	@ echo
	cd src_lib/asf_odl;$(MAKE)

################################### Library asf_fft ######################################
asf_fft.a:	lib_create clean_asf_fft $(L)/asf_fft.a

clean_asf_fft:
	-@ test -r development.directory || rm -f $(L)/asf_fft.a

$(L)/asf_fft.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling asf_fft.a library... XXXXXXX
	@ echo
	cd src_lib/asf_fft;$(MAKE)

################################### Library libjpeg ######################################
libjpeg.a:	lib_create clean_libjpeg $(L)/libjpeg.a

clean_libjpeg:
	-@ test -r development.directory || rm -f $(L)/libjpeg.a

$(L)/libjpeg.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling libjpeg.a library... XXXXXXX
	@ echo
	cd src_lib/libjpeg;$(MAKE)

################################### Library libifm ######################################
libifm.a:	lib_create clean_libifm $(L)/libifm.a

clean_libifm:
	-@ test -r development.directory || rm -f $(L)/libifm.a

$(L)/libifm.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling libifm.a library... XXXXXXX
	@ echo
	cd src_ifm/libifm;$(MAKE)

################################### Library sarmodel ######################################
sarmodel.a:	lib_create clean_sarmodel $(L)/sarmodel.a

clean_sarmodel:
	-@ test -r development.directory || rm -f $(L)/sarmodel.a

$(L)/sarmodel.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling sarmodel.a library... XXXXXXX
	@ echo
	cd src_tc/sarmodel;$(MAKE)

################################### Library tcobj ######################################
tcobj.a:	lib_create clean_tcobj $(L)/tcobj.a

clean_tcobj:
	-@ test -r development.directory || rm -f $(L)/tcobj.a

$(L)/tcobj.a:
	@ echo
	@ echo XXXXXXXXXXXXXX  Compiling tcobj.a library... XXXXXXX
	@ echo
	cd src_tc/tcobj;$(MAKE)

#####################################################################
# PROGRAMS:                                                         #
#####################################################################

################################## Program calibrate ###############################
calibrate:  bin_create testclean_calibrate asf.a asf_meta.a asf_las.a  $(B)/calibrate

testclean_calibrate:
	-@ test -r development.directory || rm -f $(B)/calibrate

clean_calibrate:
	- rm -f $(B)/calibrate

$(B)/calibrate:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program calibrate...  XXXXXXXXXXXX
	@ echo 
	cd src/calibrate; $(MAKE)
################################## Program ceosui2byte ###############################
ceosui2byte:  bin_create testclean_ceosui2byte asf.a asf_meta.a asf_las.a  $(B)/ceosui2byte

testclean_ceosui2byte:
	-@ test -r development.directory || rm -f $(B)/ceosui2byte

clean_ceosui2byte:
	- rm -f $(B)/ceosui2byte

$(B)/ceosui2byte:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program ceosui2byte...  XXXXXXXXXXXX
	@ echo 
	cd src/ceosui2byte; $(MAKE)
################################## Program convert_geo_ddr ###############################
convert_geo_ddr:  bin_create testclean_convert_geo_ddr asf.a asf_las.a  $(B)/convert_geo_ddr

testclean_convert_geo_ddr:
	-@ test -r development.directory || rm -f $(B)/convert_geo_ddr

clean_convert_geo_ddr:
	- rm -f $(B)/convert_geo_ddr

$(B)/convert_geo_ddr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program convert_geo_ddr...  XXXXXXXXXXXX
	@ echo 
	cd src/convert_geo_ddr; $(MAKE)
################################## Program diff_las ###############################
diff_las:  bin_create testclean_diff_las asf.a asf_las.a  $(B)/diff_las

testclean_diff_las:
	-@ test -r development.directory || rm -f $(B)/diff_las

clean_diff_las:
	- rm -f $(B)/diff_las

$(B)/diff_las:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program diff_las...  XXXXXXXXXXXX
	@ echo 
	cd src/diff_las; $(MAKE)
################################## Program display_pdr ###############################
display_pdr:  bin_create testclean_display_pdr asf.a asf_meta.a  $(B)/display_pdr

testclean_display_pdr:
	-@ test -r development.directory || rm -f $(B)/display_pdr

clean_display_pdr:
	- rm -f $(B)/display_pdr

$(B)/display_pdr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program display_pdr...  XXXXXXXXXXXX
	@ echo 
	cd src/display_pdr; $(MAKE)
################################## Program dspddr ###############################
dspddr:  bin_create testclean_dspddr asf.a asf_las.a asf_geolib.a  $(B)/dspddr

testclean_dspddr:
	-@ test -r development.directory || rm -f $(B)/dspddr

clean_dspddr:
	- rm -f $(B)/dspddr

$(B)/dspddr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program dspddr...  XXXXXXXXXXXX
	@ echo 
	cd src/dspddr; $(MAKE)
################################## Program dump_multi_volume ###############################
dump_multi_volume:  bin_create testclean_dump_multi_volume asf.a asf_meta.a  $(B)/dump_multi_volume

testclean_dump_multi_volume:
	-@ test -r development.directory || ( rm -f $(B)/dump_multi_volume\
	$(B)/check_name_of\
	$(B)/get_names\
	$(B)/dump_multi_volume.valid_dev ) 

clean_dump_multi_volume:
	- ( rm -f $(B)/dump_multi_volume\
	$(B)/check_name_of\
	$(B)/get_names\
	$(B)/dump_multi_volume.valid_dev ) 

$(B)/dump_multi_volume:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program dump_multi_volume...  XXXXXXXXXXXX
	@ echo 
	cd src/dump_multi_volume; $(MAKE)
################################## Program fftMatch ###############################
fftMatch:  bin_create testclean_fftMatch asf.a asf_las.a asf_fft.a  $(B)/fftMatch

testclean_fftMatch:
	-@ test -r development.directory || rm -f $(B)/fftMatch

clean_fftMatch:
	- rm -f $(B)/fftMatch

$(B)/fftMatch:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fftMatch...  XXXXXXXXXXXX
	@ echo 
	cd src/fftMatch; $(MAKE)
################################## Program get_rectypes ###############################
get_rectypes:  bin_create testclean_get_rectypes asf.a asf_meta.a  $(B)/get_rectypes

testclean_get_rectypes:
	-@ test -r development.directory || rm -f $(B)/get_rectypes

clean_get_rectypes:
	- rm -f $(B)/get_rectypes

$(B)/get_rectypes:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program get_rectypes...  XXXXXXXXXXXX
	@ echo 
	cd src/get_rectypes; $(MAKE)
################################## Program las2jpeg ###############################
las2jpeg:  bin_create testclean_las2jpeg asf.a asf_las.a libjpeg.a  $(B)/las2jpeg

testclean_las2jpeg:
	-@ test -r development.directory || ( rm -f $(B)/las2jpeg\
	$(B)/jpeg2las ) 

clean_las2jpeg:
	- ( rm -f $(B)/las2jpeg\
	$(B)/jpeg2las ) 

$(B)/las2jpeg:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program las2jpeg...  XXXXXXXXXXXX
	@ echo 
	cd src/las2jpeg; $(MAKE)
################################## Program las2ppm ###############################
las2ppm:  bin_create testclean_las2ppm asf.a asf_las.a  $(B)/las2ppm

testclean_las2ppm:
	-@ test -r development.directory || rm -f $(B)/las2ppm

clean_las2ppm:
	- rm -f $(B)/las2ppm

$(B)/las2ppm:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program las2ppm...  XXXXXXXXXXXX
	@ echo 
	cd src/las2ppm; $(MAKE)
################################## Program las_op ###############################
las_op:  bin_create testclean_las_op asf.a asf_las.a  $(B)/las_op

testclean_las_op:
	-@ test -r development.directory || rm -f $(B)/las_op

clean_las_op:
	- rm -f $(B)/las_op

$(B)/las_op:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program las_op...  XXXXXXXXXXXX
	@ echo 
	cd src/las_op; $(MAKE)
################################## Program las_ramp ###############################
las_ramp:  bin_create testclean_las_ramp asf.a asf_las.a  $(B)/las_ramp

testclean_las_ramp:
	-@ test -r development.directory || rm -f $(B)/las_ramp

clean_las_ramp:
	- rm -f $(B)/las_ramp

$(B)/las_ramp:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program las_ramp...  XXXXXXXXXXXX
	@ echo 
	cd src/las_ramp; $(MAKE)
################################## Program las_text ###############################
las_text:  bin_create testclean_las_text asf.a asf_las.a  $(B)/las_text

testclean_las_text:
	-@ test -r development.directory || rm -f $(B)/las_text

clean_las_text:
	- rm -f $(B)/las_text

$(B)/las_text:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program las_text...  XXXXXXXXXXXX
	@ echo 
	cd src/las_text; $(MAKE)
################################## Program makeddr ###############################
makeddr:  bin_create testclean_makeddr asf.a asf_las.a asf_geolib.a  $(B)/makeddr

testclean_makeddr:
	-@ test -r development.directory || rm -f $(B)/makeddr

clean_makeddr:
	- rm -f $(B)/makeddr

$(B)/makeddr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program makeddr...  XXXXXXXXXXXX
	@ echo 
	cd src/makeddr; $(MAKE)
################################## Program mark_tpl ###############################
mark_tpl:  bin_create testclean_mark_tpl asf.a asf_las.a  $(B)/mark_tpl

testclean_mark_tpl:
	-@ test -r development.directory || rm -f $(B)/mark_tpl

clean_mark_tpl:
	- rm -f $(B)/mark_tpl

$(B)/mark_tpl:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program mark_tpl...  XXXXXXXXXXXX
	@ echo 
	cd src/mark_tpl; $(MAKE)
################################## Program metadata ###############################
metadata:  bin_create testclean_metadata asf.a asf_meta.a  $(B)/metadata

testclean_metadata:
	-@ test -r development.directory || rm -f $(B)/metadata

clean_metadata:
	- rm -f $(B)/metadata

$(B)/metadata:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program metadata...  XXXXXXXXXXXX
	@ echo 
	cd src/metadata; $(MAKE)
################################## Program remap ###############################
remap:  bin_create testclean_remap asf.a asf_las.a  $(B)/remap

testclean_remap:
	-@ test -r development.directory || rm -f $(B)/remap

clean_remap:
	- rm -f $(B)/remap

$(B)/remap:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program remap...  XXXXXXXXXXXX
	@ echo 
	cd src/remap; $(MAKE)
################################## Program resample ###############################
resample:  bin_create testclean_resample asf.a asf_las.a  $(B)/resample

testclean_resample:
	-@ test -r development.directory || rm -f $(B)/resample

clean_resample:
	- rm -f $(B)/resample

$(B)/resample:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program resample...  XXXXXXXXXXXX
	@ echo 
	cd src/resample; $(MAKE)
################################## Program sarin ###############################
sarin:  bin_create testclean_sarin asf.a asf_meta.a asf_las.a  $(B)/sarin

testclean_sarin:
	-@ test -r development.directory || rm -f $(B)/sarin

clean_sarin:
	- rm -f $(B)/sarin

$(B)/sarin:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sarin...  XXXXXXXXXXXX
	@ echo 
	cd src/sarin; $(MAKE)
################################## Program sarout ###############################
sarout:  bin_create testclean_sarout asf.a asf_meta.a asf_las.a asf_odl.a  $(B)/sarout

testclean_sarout:
	-@ test -r development.directory || rm -f $(B)/sarout

clean_sarout:
	- rm -f $(B)/sarout

$(B)/sarout:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sarout...  XXXXXXXXXXXX
	@ echo 
	cd src/sarout; $(MAKE)
################################## Program sr2gr ###############################
sr2gr:  bin_create testclean_sr2gr asf.a asf_meta.a asf_las.a  $(B)/sr2gr

testclean_sr2gr:
	-@ test -r development.directory || ( rm -f $(B)/sr2gr\
	$(B)/deskew\
	$(B)/ui2byte ) 

clean_sr2gr:
	- ( rm -f $(B)/sr2gr\
	$(B)/deskew\
	$(B)/ui2byte ) 

$(B)/sr2gr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sr2gr...  XXXXXXXXXXXX
	@ echo 
	cd src/sr2gr; $(MAKE)
################################## Program ssar_crop ###############################
ssar_crop:  bin_create testclean_ssar_crop asf.a asf_meta.a  $(B)/ssar_crop

testclean_ssar_crop:
	-@ test -r development.directory || rm -f $(B)/ssar_crop

clean_ssar_crop:
	- rm -f $(B)/ssar_crop

$(B)/ssar_crop:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program ssar_crop...  XXXXXXXXXXXX
	@ echo 
	cd src/ssar_crop; $(MAKE)
################################## Program trim ###############################
trim:  bin_create testclean_trim asf.a asf_las.a  $(B)/trim

testclean_trim:
	-@ test -r development.directory || rm -f $(B)/trim

clean_trim:
	- rm -f $(B)/trim

$(B)/trim:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program trim...  XXXXXXXXXXXX
	@ echo 
	cd src/trim; $(MAKE)
################################## Program accum_offset ###############################
accum_offset:  bin_create testclean_accum_offset asf.a asf_meta.a  $(B)/accum_offset

testclean_accum_offset:
	-@ test -r development.directory || rm -f $(B)/accum_offset

clean_accum_offset:
	- rm -f $(B)/accum_offset

$(B)/accum_offset:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program accum_offset...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/accum_offset; $(MAKE)
################################## Program concat ###############################
concat:  bin_create testclean_concat asf.a asf_las.a asf_concat.a  $(B)/concat

testclean_concat:
	-@ test -r development.directory || rm -f $(B)/concat

clean_concat:
	- rm -f $(B)/concat

$(B)/concat:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program concat...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/concat; $(MAKE)
################################## Program concat_dem ###############################
concat_dem:  bin_create testclean_concat_dem asf.a asf_las.a  $(B)/concat_dem

testclean_concat_dem:
	-@ test -r development.directory || ( rm -f $(B)/concat_dem\
	$(B)/profile ) 

clean_concat_dem:
	- ( rm -f $(B)/concat_dem\
	$(B)/profile ) 

$(B)/concat_dem:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program concat_dem...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/concat_dem; $(MAKE)
################################## Program concatm ###############################
concatm:  bin_create testclean_concatm asf.a asf_las.a asf_concat.a  $(B)/concatm

testclean_concatm:
	-@ test -r development.directory || rm -f $(B)/concatm

clean_concatm:
	- rm -f $(B)/concatm

$(B)/concatm:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program concatm...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/concatm; $(MAKE)
################################## Program fit_quadratic ###############################
fit_quadratic:  bin_create testclean_fit_quadratic asf.a  $(B)/fit_quadratic

testclean_fit_quadratic:
	-@ test -r development.directory || rm -f $(B)/fit_quadratic

clean_fit_quadratic:
	- rm -f $(B)/fit_quadratic

$(B)/fit_quadratic:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fit_quadratic...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/fit_quadratic; $(MAKE)
################################## Program idimage ###############################
idimage:  bin_create testclean_idimage asf.a asf_las.a  $(B)/idimage

testclean_idimage:
	-@ test -r development.directory || ( rm -f $(B)/idimage\
	$(B)/idtable ) 

clean_idimage:
	- ( rm -f $(B)/idimage\
	$(B)/idtable ) 

$(B)/idimage:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program idimage...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/idimage; $(MAKE)
################################## Program projprm ###############################
projprm:  bin_create testclean_projprm asf.a asf_las.a asf_lt.a asf_geolib.a  $(B)/projprm

testclean_projprm:
	-@ test -r development.directory || rm -f $(B)/projprm

clean_projprm:
	- rm -f $(B)/projprm

$(B)/projprm:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program projprm...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/projprm; $(MAKE)
################################## Program psconv ###############################
psconv:  bin_create testclean_psconv asf_geolib.a  $(B)/psconv

testclean_psconv:
	-@ test -r development.directory || rm -f $(B)/psconv

clean_psconv:
	- rm -f $(B)/psconv

$(B)/psconv:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program psconv...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/psconv; $(MAKE)
################################## Program twoway ###############################
twoway:  bin_create testclean_twoway asf.a asf_meta.a asf_las.a  $(B)/twoway

testclean_twoway:
	-@ test -r development.directory || rm -f $(B)/twoway

clean_twoway:
	- rm -f $(B)/twoway

$(B)/twoway:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program twoway...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/twoway; $(MAKE)
################################## Program geocode ###############################
geocode:  bin_create testclean_geocode asf.a asf_meta.a asf_las.a asf_lt.a asf_geolib.a remap fit_quadratic  $(B)/geocode

testclean_geocode:
	-@ test -r development.directory || ( rm -f $(B)/geocode\
	$(B)/remap\
	$(B)/fit_quadratic ) 

clean_geocode:
	- ( rm -f $(B)/geocode\
	$(B)/remap\
	$(B)/fit_quadratic ) 

$(B)/geocode:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program geocode...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/geocode; $(MAKE)
################################## Program auto_sar_mosaic ###############################
auto_sar_mosaic:  bin_create testclean_auto_sar_mosaic calibrate sarin geocode concat resample idimage  $(B)/auto_sar_mosaic

testclean_auto_sar_mosaic:
	-@ test -r development.directory || ( rm -f $(B)/auto_sar_mosaic\
	$(B)/calibrate\
	$(B)/sarin\
	$(B)/geocode\
	$(B)/concat\
	$(B)/resample\
	$(B)/idimage ) 

clean_auto_sar_mosaic:
	- ( rm -f $(B)/auto_sar_mosaic\
	$(B)/calibrate\
	$(B)/sarin\
	$(B)/geocode\
	$(B)/concat\
	$(B)/resample\
	$(B)/idimage ) 

$(B)/auto_sar_mosaic:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program auto_sar_mosaic...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/auto_sar_mosaic; $(MAKE)
################################## Program auto_swath_mosaic ###############################
auto_swath_mosaic:  bin_create testclean_auto_swath_mosaic calibrate sarin accum_offset concatm resample idimage  $(B)/auto_swath_mosaic

testclean_auto_swath_mosaic:
	-@ test -r development.directory || ( rm -f $(B)/auto_swath_mosaic\
	$(B)/calibrate\
	$(B)/sarin\
	$(B)/accum_offset\
	$(B)/concatm\
	$(B)/resample\
	$(B)/idimage ) 

clean_auto_swath_mosaic:
	- ( rm -f $(B)/auto_swath_mosaic\
	$(B)/calibrate\
	$(B)/sarin\
	$(B)/accum_offset\
	$(B)/concatm\
	$(B)/resample\
	$(B)/idimage ) 

$(B)/auto_swath_mosaic:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program auto_swath_mosaic...  XXXXXXXXXXXX
	@ echo 
	cd src_geo/auto_swath_mosaic; $(MAKE)
################################## Program aisp ###############################
aisp:  bin_create testclean_aisp asf.a asf_meta.a asf_las.a asf_odl.a asf_fft.a  $(B)/aisp

testclean_aisp:
	-@ test -r development.directory || ( rm -f $(B)/aisp\
	$(B)/calc_deltas\
	$(B)/spectra\
	$(B)/estavedop\
	$(B)/quicklook ) 

clean_aisp:
	- ( rm -f $(B)/aisp\
	$(B)/calc_deltas\
	$(B)/spectra\
	$(B)/estavedop\
	$(B)/quicklook ) 

$(B)/aisp:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program aisp...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/aisp; $(MAKE)
################################## Program amp2img ###############################
amp2img:  bin_create testclean_amp2img asf.a asf_meta.a asf_las.a  $(B)/amp2img

testclean_amp2img:
	-@ test -r development.directory || rm -f $(B)/amp2img

clean_amp2img:
	- rm -f $(B)/amp2img

$(B)/amp2img:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program amp2img...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/amp2img; $(MAKE)
################################## Program avg_in_dop ###############################
avg_in_dop:  bin_create testclean_avg_in_dop asf.a asf_meta.a  $(B)/avg_in_dop

testclean_avg_in_dop:
	-@ test -r development.directory || rm -f $(B)/avg_in_dop

clean_avg_in_dop:
	- rm -f $(B)/avg_in_dop

$(B)/avg_in_dop:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program avg_in_dop...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/avg_in_dop; $(MAKE)
################################## Program c2p ###############################
c2p:  bin_create testclean_c2p asf.a  $(B)/c2p

testclean_c2p:
	-@ test -r development.directory || rm -f $(B)/c2p

clean_c2p:
	- rm -f $(B)/c2p

$(B)/c2p:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program c2p...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/c2p; $(MAKE)
################################## Program coh ###############################
coh:  bin_create testclean_coh asf.a asf_meta.a asf_las.a libifm.a  $(B)/coh

testclean_coh:
	-@ test -r development.directory || rm -f $(B)/coh

clean_coh:
	- rm -f $(B)/coh

$(B)/coh:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program coh...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/coh; $(MAKE)
################################## Program cpx_spectrum ###############################
cpx_spectrum:  bin_create testclean_cpx_spectrum asf.a asf_meta.a asf_las.a asf_fft.a  $(B)/cpx_spectrum

testclean_cpx_spectrum:
	-@ test -r development.directory || rm -f $(B)/cpx_spectrum

clean_cpx_spectrum:
	- rm -f $(B)/cpx_spectrum

$(B)/cpx_spectrum:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program cpx_spectrum...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/cpx_spectrum; $(MAKE)
################################## Program cpx_autofilter ###############################
cpx_autofilter:  bin_create testclean_cpx_autofilter asf.a asf_meta.a asf_las.a asf_fft.a cpx_spectrum  $(B)/cpx_autofilter

testclean_cpx_autofilter:
	-@ test -r development.directory || ( rm -f $(B)/cpx_autofilter\
	$(B)/cpx_spectrum\
	$(B)/cpx_filter\
	$(B)/gen_filt_params ) 

clean_cpx_autofilter:
	- ( rm -f $(B)/cpx_autofilter\
	$(B)/cpx_spectrum\
	$(B)/cpx_filter\
	$(B)/gen_filt_params ) 

$(B)/cpx_autofilter:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program cpx_autofilter...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/cpx_autofilter; $(MAKE)
################################## Program deskew_dem ###############################
deskew_dem:  bin_create testclean_deskew_dem asf.a asf_meta.a asf_las.a asf_geolib.a  $(B)/deskew_dem

testclean_deskew_dem:
	-@ test -r development.directory || rm -f $(B)/deskew_dem

clean_deskew_dem:
	- rm -f $(B)/deskew_dem

$(B)/deskew_dem:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program deskew_dem...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/deskew_dem; $(MAKE)
################################## Program reskew_dem ###############################
reskew_dem:  bin_create testclean_reskew_dem asf.a asf_meta.a asf_las.a  $(B)/reskew_dem

testclean_reskew_dem:
	-@ test -r development.directory || rm -f $(B)/reskew_dem

clean_reskew_dem:
	- rm -f $(B)/reskew_dem

$(B)/reskew_dem:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program reskew_dem...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/reskew_dem; $(MAKE)
################################## Program deramp ###############################
deramp:  bin_create testclean_deramp asf.a asf_meta.a asf_las.a  $(B)/deramp

testclean_deramp:
	-@ test -r development.directory || rm -f $(B)/deramp

clean_deramp:
	- rm -f $(B)/deramp

$(B)/deramp:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program deramp...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/deramp; $(MAKE)
################################## Program elev ###############################
elev:  bin_create testclean_elev asf.a asf_meta.a asf_las.a  $(B)/elev

testclean_elev:
	-@ test -r development.directory || rm -f $(B)/elev

clean_elev:
	- rm -f $(B)/elev

$(B)/elev:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program elev...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/elev; $(MAKE)
################################## Program eleverr ###############################
eleverr:  bin_create testclean_eleverr asf.a asf_meta.a asf_las.a  $(B)/eleverr

testclean_eleverr:
	-@ test -r development.directory || rm -f $(B)/eleverr

clean_eleverr:
	- rm -f $(B)/eleverr

$(B)/eleverr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program eleverr...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/eleverr; $(MAKE)
################################## Program escher ###############################
escher:  bin_create testclean_escher asf.a asf_las.a libifm.a  $(B)/escher

testclean_escher:
	-@ test -r development.directory || rm -f $(B)/escher

clean_escher:
	- rm -f $(B)/escher

$(B)/escher:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program escher...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/escher; $(MAKE)
################################## Program fico ###############################
fico:  bin_create testclean_fico asf.a asf_las.a asf_fft.a libifm.a  $(B)/fico

testclean_fico:
	-@ test -r development.directory || rm -f $(B)/fico

clean_fico:
	- rm -f $(B)/fico

$(B)/fico:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fico...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/fico; $(MAKE)
################################## Program fit_line ###############################
fit_line:  bin_create testclean_fit_line asf.a  $(B)/fit_line

testclean_fit_line:
	-@ test -r development.directory || rm -f $(B)/fit_line

clean_fit_line:
	- rm -f $(B)/fit_line

$(B)/fit_line:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fit_line...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/fit_line; $(MAKE)
################################## Program fit_plane ###############################
fit_plane:  bin_create testclean_fit_plane asf.a  $(B)/fit_plane

testclean_fit_plane:
	-@ test -r development.directory || rm -f $(B)/fit_plane

clean_fit_plane:
	- rm -f $(B)/fit_plane

$(B)/fit_plane:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fit_plane...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/fit_plane; $(MAKE)
################################## Program fit_warp ###############################
fit_warp:  bin_create testclean_fit_warp asf.a asf_las.a  $(B)/fit_warp

testclean_fit_warp:
	-@ test -r development.directory || rm -f $(B)/fit_warp

clean_fit_warp:
	- rm -f $(B)/fit_warp

$(B)/fit_warp:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fit_warp...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/fit_warp; $(MAKE)
################################## Program fix_in ###############################
fix_in:  bin_create testclean_fix_in asf.a asf_meta.a  $(B)/fix_in

testclean_fix_in:
	-@ test -r development.directory || rm -f $(B)/fix_in

clean_fix_in:
	- rm -f $(B)/fix_in

$(B)/fix_in:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fix_in...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/fix_in; $(MAKE)
################################## Program fix_in_fromraw ###############################
fix_in_fromraw:  bin_create testclean_fix_in_fromraw asf.a asf_meta.a  $(B)/fix_in_fromraw

testclean_fix_in_fromraw:
	-@ test -r development.directory || rm -f $(B)/fix_in_fromraw

clean_fix_in_fromraw:
	- rm -f $(B)/fix_in_fromraw

$(B)/fix_in_fromraw:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program fix_in_fromraw...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/fix_in_fromraw; $(MAKE)
################################## Program igram ###############################
igram:  bin_create testclean_igram asf.a asf_las.a  $(B)/igram

testclean_igram:
	-@ test -r development.directory || rm -f $(B)/igram

clean_igram:
	- rm -f $(B)/igram

$(B)/igram:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program igram...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/igram; $(MAKE)
################################## Program ceos2raw ###############################
ceos2raw:  bin_create testclean_ceos2raw asf.a asf_meta.a  $(B)/ceos2raw

testclean_ceos2raw:
	-@ test -r development.directory || rm -f $(B)/ceos2raw

clean_ceos2raw:
	- rm -f $(B)/ceos2raw

$(B)/ceos2raw:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program ceos2raw...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/ceos2raw; $(MAKE)
################################## Program lz2raw_flywheel ###############################
lz2raw_flywheel:  bin_create testclean_lz2raw_flywheel asf.a asf_meta.a  $(B)/lz2raw_flywheel

testclean_lz2raw_flywheel:
	-@ test -r development.directory || rm -f $(B)/lz2raw_flywheel

clean_lz2raw_flywheel:
	- rm -f $(B)/lz2raw_flywheel

$(B)/lz2raw_flywheel:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program lz2raw_flywheel...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/lz2raw_flywheel; $(MAKE)
################################## Program ml ###############################
ml:  bin_create testclean_ml asf.a asf_meta.a asf_las.a libifm.a  $(B)/ml

testclean_ml:
	-@ test -r development.directory || rm -f $(B)/ml

clean_ml:
	- rm -f $(B)/ml

$(B)/ml:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program ml...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/ml; $(MAKE)
################################## Program p2c ###############################
p2c:  bin_create testclean_p2c asf.a  $(B)/p2c

testclean_p2c:
	-@ test -r development.directory || rm -f $(B)/p2c

clean_p2c:
	- rm -f $(B)/p2c

$(B)/p2c:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program p2c...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/p2c; $(MAKE)
################################## Program phase_filter ###############################
phase_filter:  bin_create testclean_phase_filter asf.a asf_las.a asf_fft.a  $(B)/phase_filter

testclean_phase_filter:
	-@ test -r development.directory || rm -f $(B)/phase_filter

clean_phase_filter:
	- rm -f $(B)/phase_filter

$(B)/phase_filter:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program phase_filter...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/phase_filter; $(MAKE)
################################## Program resolve ###############################
resolve:  bin_create testclean_resolve asf.a asf_meta.a fftMatch amp2img c2p  $(B)/resolve

testclean_resolve:
	-@ test -r development.directory || ( rm -f $(B)/resolve\
	$(B)/fftMatch\
	$(B)/amp2img\
	$(B)/c2p ) 

clean_resolve:
	- ( rm -f $(B)/resolve\
	$(B)/fftMatch\
	$(B)/amp2img\
	$(B)/c2p ) 

$(B)/resolve:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program resolve...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/resolve; $(MAKE)
################################## Program refine_base ###############################
refine_base:  bin_create testclean_refine_base asf.a asf_meta.a asf_las.a libifm.a  $(B)/refine_base

testclean_refine_base:
	-@ test -r development.directory || rm -f $(B)/refine_base

clean_refine_base:
	- rm -f $(B)/refine_base

$(B)/refine_base:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program refine_base...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/refine_base; $(MAKE)
################################## Program refine_offset ###############################
refine_offset:  bin_create testclean_refine_offset asf.a asf_meta.a asf_las.a  $(B)/refine_offset

testclean_refine_offset:
	-@ test -r development.directory || rm -f $(B)/refine_offset

clean_refine_offset:
	- rm -f $(B)/refine_offset

$(B)/refine_offset:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program refine_offset...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/refine_offset; $(MAKE)
################################## Program trim_slc ###############################
trim_slc:  bin_create testclean_trim_slc asf.a asf_meta.a asf_las.a  $(B)/trim_slc

testclean_trim_slc:
	-@ test -r development.directory || rm -f $(B)/trim_slc

clean_trim_slc:
	- rm -f $(B)/trim_slc

$(B)/trim_slc:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program trim_slc...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/trim_slc; $(MAKE)
################################## Program demIFM ###############################
demIFM:  bin_create testclean_demIFM asf.a asf_meta.a asf_las.a asf_geolib.a dspddr fit_plane remap makeddr reskew_dem amp2img trim fftMatch  $(B)/demIFM

testclean_demIFM:
	-@ test -r development.directory || ( rm -f $(B)/demIFM\
	$(B)/dspddr\
	$(B)/fit_plane\
	$(B)/remap\
	$(B)/makeddr\
	$(B)/reskew_dem\
	$(B)/amp2img\
	$(B)/trim\
	$(B)/fftMatch\
	$(B)/dem2phase\
	$(B)/dem2seeds\
	$(B)/create_dem_grid ) 

clean_demIFM:
	- ( rm -f $(B)/demIFM\
	$(B)/dspddr\
	$(B)/fit_plane\
	$(B)/remap\
	$(B)/makeddr\
	$(B)/reskew_dem\
	$(B)/amp2img\
	$(B)/trim\
	$(B)/fftMatch\
	$(B)/dem2phase\
	$(B)/dem2seeds\
	$(B)/create_dem_grid ) 

$(B)/demIFM:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program demIFM...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/demIFM; $(MAKE)
################################## Program register_slc ###############################
register_slc:  bin_create testclean_register_slc asf.a asf_meta.a trim_slc remap resolve fico fit_plane fit_warp c2p coh igram  $(B)/register_slc

testclean_register_slc:
	-@ test -r development.directory || ( rm -f $(B)/register_slc\
	$(B)/trim_slc\
	$(B)/remap\
	$(B)/resolve\
	$(B)/fico\
	$(B)/fit_plane\
	$(B)/fit_warp\
	$(B)/c2p\
	$(B)/coh\
	$(B)/igram\
	$(B)/register_cpx\
	$(B)/register_cpx_window ) 

clean_register_slc:
	- ( rm -f $(B)/register_slc\
	$(B)/trim_slc\
	$(B)/remap\
	$(B)/resolve\
	$(B)/fico\
	$(B)/fit_plane\
	$(B)/fit_warp\
	$(B)/c2p\
	$(B)/coh\
	$(B)/igram\
	$(B)/register_cpx\
	$(B)/register_cpx_window ) 

$(B)/register_slc:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program register_slc...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/register_slc; $(MAKE)
################################## Program register_ccsd ###############################
register_ccsd:  bin_create testclean_register_ccsd aisp resolve fico fit_line coh igram register_slc  $(B)/register_ccsd

testclean_register_ccsd:
	-@ test -r development.directory || ( rm -f $(B)/register_ccsd\
	$(B)/aisp\
	$(B)/resolve\
	$(B)/fico\
	$(B)/fit_line\
	$(B)/coh\
	$(B)/igram\
	$(B)/register_slc ) 

clean_register_ccsd:
	- ( rm -f $(B)/register_ccsd\
	$(B)/aisp\
	$(B)/resolve\
	$(B)/fico\
	$(B)/fit_line\
	$(B)/coh\
	$(B)/igram\
	$(B)/register_slc ) 

$(B)/register_ccsd:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program register_ccsd...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/register_ccsd; $(MAKE)
################################## Program tandem_ifm ###############################
tandem_ifm:  bin_create testclean_tandem_ifm asf.a asf_meta.a asf_las.a las2ppm deramp elev eleverr deskew_dem ml escher refine_base phase_filter  $(B)/tandem_ifm

testclean_tandem_ifm:
	-@ test -r development.directory || ( rm -f $(B)/tandem_ifm\
	$(B)/las2ppm\
	$(B)/deramp\
	$(B)/elev\
	$(B)/eleverr\
	$(B)/deskew_dem\
	$(B)/ml\
	$(B)/escher\
	$(B)/refine_base\
	$(B)/phase_filter\
	$(B)/zeroify ) 

clean_tandem_ifm:
	- ( rm -f $(B)/tandem_ifm\
	$(B)/las2ppm\
	$(B)/deramp\
	$(B)/elev\
	$(B)/eleverr\
	$(B)/deskew_dem\
	$(B)/ml\
	$(B)/escher\
	$(B)/refine_base\
	$(B)/phase_filter\
	$(B)/zeroify ) 

$(B)/tandem_ifm:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program tandem_ifm...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/tandem_ifm; $(MAKE)
################################## Program register_lzceos ###############################
register_lzceos:  bin_create testclean_register_lzceos avg_in_dop aisp resolve fico register_slc fit_line metadata igram coh  $(B)/register_lzceos

testclean_register_lzceos:
	-@ test -r development.directory || ( rm -f $(B)/register_lzceos\
	$(B)/avg_in_dop\
	$(B)/aisp\
	$(B)/resolve\
	$(B)/fico\
	$(B)/register_slc\
	$(B)/fit_line\
	$(B)/metadata\
	$(B)/igram\
	$(B)/coh ) 

clean_register_lzceos:
	- ( rm -f $(B)/register_lzceos\
	$(B)/avg_in_dop\
	$(B)/aisp\
	$(B)/resolve\
	$(B)/fico\
	$(B)/register_slc\
	$(B)/fit_line\
	$(B)/metadata\
	$(B)/igram\
	$(B)/coh ) 

$(B)/register_lzceos:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program register_lzceos...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/register_lzceos; $(MAKE)
################################## Program register_lzraw ###############################
register_lzraw:  bin_create testclean_register_lzraw asf.a asf_meta.a lz2raw_flywheel fix_in_fromraw avg_in_dop aisp fico fit_line igram coh ml  $(B)/register_lzraw

testclean_register_lzraw:
	-@ test -r development.directory || ( rm -f $(B)/register_lzraw\
	$(B)/lz2raw_flywheel\
	$(B)/fix_in_fromraw\
	$(B)/avg_in_dop\
	$(B)/aisp\
	$(B)/fico\
	$(B)/fit_line\
	$(B)/igram\
	$(B)/coh\
	$(B)/ml\
	$(B)/calc_deltas ) 

clean_register_lzraw:
	- ( rm -f $(B)/register_lzraw\
	$(B)/lz2raw_flywheel\
	$(B)/fix_in_fromraw\
	$(B)/avg_in_dop\
	$(B)/aisp\
	$(B)/fico\
	$(B)/fit_line\
	$(B)/igram\
	$(B)/coh\
	$(B)/ml\
	$(B)/calc_deltas ) 

$(B)/register_lzraw:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program register_lzraw...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/register_lzraw; $(MAKE)
################################## Program swath_offset ###############################
swath_offset:  bin_create testclean_swath_offset asf.a asf_meta.a  $(B)/swath_offset

testclean_swath_offset:
	-@ test -r development.directory || rm -f $(B)/swath_offset

clean_swath_offset:
	- rm -f $(B)/swath_offset

$(B)/swath_offset:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program swath_offset...  XXXXXXXXXXXX
	@ echo 
	cd src_ifm/swath_offset; $(MAKE)
################################## Program 2dmap ###############################
2dmap:  bin_create testclean_2dmap asf.a asf_las.a asf_geolib.a  $(B)/2dmap

testclean_2dmap:
	-@ test -r development.directory || rm -f $(B)/2dmap

clean_2dmap:
	- rm -f $(B)/2dmap

$(B)/2dmap:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program 2dmap...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/2dmap; $(MAKE)
################################## Program demimg ###############################
demimg:  bin_create testclean_demimg asf.a asf_meta.a asf_las.a sarmodel.a tcobj.a  $(B)/demimg

testclean_demimg:
	-@ test -r development.directory || rm -f $(B)/demimg

clean_demimg:
	- rm -f $(B)/demimg

$(B)/demimg:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program demimg...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/demimg; $(MAKE)
################################## Program demclip ###############################
demclip:  bin_create testclean_demclip asf.a asf_meta.a asf_las.a asf_geolib.a  $(B)/demclip

testclean_demclip:
	-@ test -r development.directory || rm -f $(B)/demclip

clean_demclip:
	- rm -f $(B)/demclip

$(B)/demclip:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program demclip...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/demclip; $(MAKE)
################################## Program despike ###############################
despike:  bin_create testclean_despike asf.a asf_las.a  $(B)/despike

testclean_despike:
	-@ test -r development.directory || rm -f $(B)/despike

clean_despike:
	- rm -f $(B)/despike

$(B)/despike:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program despike...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/despike; $(MAKE)
################################## Program greycorr ###############################
greycorr:  bin_create testclean_greycorr asf.a asf_las.a asf_fft.a  $(B)/greycorr

testclean_greycorr:
	-@ test -r development.directory || rm -f $(B)/greycorr

clean_greycorr:
	- rm -f $(B)/greycorr

$(B)/greycorr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program greycorr...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/greycorr; $(MAKE)
################################## Program sargeom ###############################
sargeom:  bin_create testclean_sargeom asf.a asf_meta.a asf_las.a sarmodel.a tcobj.a  $(B)/sargeom

testclean_sargeom:
	-@ test -r development.directory || rm -f $(B)/sargeom

clean_sargeom:
	- rm -f $(B)/sargeom

$(B)/sargeom:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sargeom...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/sargeom; $(MAKE)
################################## Program sarsim ###############################
sarsim:  bin_create testclean_sarsim asf.a asf_meta.a asf_las.a sarmodel.a tcobj.a  $(B)/sarsim

testclean_sarsim:
	-@ test -r development.directory || rm -f $(B)/sarsim

clean_sarsim:
	- rm -f $(B)/sarsim

$(B)/sarsim:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sarsim...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/sarsim; $(MAKE)
################################## Program rtc_add ###############################
rtc_add:  bin_create testclean_rtc_add asf.a asf_las.a  $(B)/rtc_add

testclean_rtc_add:
	-@ test -r development.directory || rm -f $(B)/rtc_add

clean_rtc_add:
	- rm -f $(B)/rtc_add

$(B)/rtc_add:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program rtc_add...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/rtc_add; $(MAKE)
################################## Program tpl_mult ###############################
tpl_mult:  bin_create testclean_tpl_mult asf.a  $(B)/tpl_mult

testclean_tpl_mult:
	-@ test -r development.directory || rm -f $(B)/tpl_mult

clean_tpl_mult:
	- rm -f $(B)/tpl_mult

$(B)/tpl_mult:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program tpl_mult...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/tpl_mult; $(MAKE)
################################## Program tpl_search ###############################
tpl_search:  bin_create testclean_tpl_search asf.a asf_las.a  $(B)/tpl_search

testclean_tpl_search:
	-@ test -r development.directory || rm -f $(B)/tpl_search

clean_tpl_search:
	- rm -f $(B)/tpl_search

$(B)/tpl_search:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program tpl_search...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/tpl_search; $(MAKE)
################################## Program correlate ###############################
correlate:  bin_create testclean_correlate asf.a asf_las.a resample tpl_mult tpl_search greycorr 2dmap  $(B)/correlate

testclean_correlate:
	-@ test -r development.directory || ( rm -f $(B)/correlate\
	$(B)/resample\
	$(B)/tpl_mult\
	$(B)/tpl_search\
	$(B)/greycorr\
	$(B)/2dmap ) 

clean_correlate:
	- ( rm -f $(B)/correlate\
	$(B)/resample\
	$(B)/tpl_mult\
	$(B)/tpl_search\
	$(B)/greycorr\
	$(B)/2dmap ) 

$(B)/correlate:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program correlate...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/correlate; $(MAKE)
################################## Program terrcorr ###############################
terrcorr:  bin_create testclean_terrcorr asf.a asf_meta.a asf_las.a sarin calibrate correlate demimg demclip despike sargeom rtc_add sarsim  $(B)/terrcorr

testclean_terrcorr:
	-@ test -r development.directory || ( rm -f $(B)/terrcorr\
	$(B)/sarin\
	$(B)/calibrate\
	$(B)/correlate\
	$(B)/demimg\
	$(B)/demclip\
	$(B)/despike\
	$(B)/sargeom\
	$(B)/rtc_add\
	$(B)/sarsim ) 

clean_terrcorr:
	- ( rm -f $(B)/terrcorr\
	$(B)/sarin\
	$(B)/calibrate\
	$(B)/correlate\
	$(B)/demimg\
	$(B)/demclip\
	$(B)/despike\
	$(B)/sargeom\
	$(B)/rtc_add\
	$(B)/sarsim ) 

$(B)/terrcorr:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program terrcorr...  XXXXXXXXXXXX
	@ echo 
	cd src_tc/terrcorr; $(MAKE)
################################## Program las_stack ###############################
las_stack:  bin_create testclean_las_stack dspddr correlate fit_plane remap concatm  $(B)/las_stack

testclean_las_stack:
	-@ test -r development.directory || ( rm -f $(B)/las_stack\
	$(B)/dspddr\
	$(B)/correlate\
	$(B)/fit_plane\
	$(B)/remap\
	$(B)/concatm ) 

clean_las_stack:
	- ( rm -f $(B)/las_stack\
	$(B)/dspddr\
	$(B)/correlate\
	$(B)/fit_plane\
	$(B)/remap\
	$(B)/concatm ) 

$(B)/las_stack:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program las_stack...  XXXXXXXXXXXX
	@ echo 
	cd src/las_stack; $(MAKE)
################################## Program sdts2las ###############################
sdts2las:  bin_create testclean_sdts2las concat  $(B)/sdts2las

testclean_sdts2las:
	-@ test -r development.directory || ( rm -f $(B)/sdts2las\
	$(B)/concat\
	$(B)/sdts_ingest ) 

clean_sdts2las:
	- ( rm -f $(B)/sdts2las\
	$(B)/concat\
	$(B)/sdts_ingest ) 

$(B)/sdts2las:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sdts2las...  XXXXXXXXXXXX
	@ echo 
	cd src/sdts2las; $(MAKE)
################################## Program usgs2las ###############################
usgs2las:  bin_create testclean_usgs2las asf.a asf_las.a concat_dem  $(B)/usgs2las

testclean_usgs2las:
	-@ test -r development.directory || ( rm -f $(B)/usgs2las\
	$(B)/concat_dem\
	$(B)/usgs_ingest ) 

clean_usgs2las:
	- ( rm -f $(B)/usgs2las\
	$(B)/concat_dem\
	$(B)/usgs_ingest ) 

$(B)/usgs2las:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program usgs2las...  XXXXXXXXXXXX
	@ echo 
	cd src/usgs2las; $(MAKE)
################################## Program propagate ###############################
propagate:  bin_create testclean_propagate asf.a  $(B)/propagate

testclean_propagate:
	-@ test -r development.directory || ( rm -f $(B)/propagate\
	$(B)/gen_oe\
	$(B)/asap ) 

clean_propagate:
	- ( rm -f $(B)/propagate\
	$(B)/gen_oe\
	$(B)/asap ) 

$(B)/propagate:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program propagate...  XXXXXXXXXXXX
	@ echo 
	cd src/propagate; $(MAKE)
################################## Program sarview ###############################
sarview:  bin_create testclean_sarview asf.a asf_meta.a asf_las.a asf_geolib.a libjpeg.a  $(B)/sarview

testclean_sarview:
	-@ test -r development.directory || ( rm -f $(B)/sarview\
	$(B)/sarview_bin\
	$(B)/support ) 

clean_sarview:
	- ( rm -f $(B)/sarview\
	$(B)/sarview_bin\
	$(B)/support ) 

$(B)/sarview:
	@ echo 
	@ echo XXXXXXXXXXXXXXXX Compiling Program sarview...  XXXXXXXXXXXX
	@ echo 
	cd src_x/sarview; $(MAKE)

#####################################################################
# CATEGORIES:                                                       #
#####################################################################

