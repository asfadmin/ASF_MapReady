# SccsId = @(#)makefile	2.41 3/24/98 
#

# Define enviornment variables
ASF      = /ASF/$(ASFVER)
LOCAL    = /LOCAL/$(ASFVER)

# Define some commands for IBM SP-2
DSH      = /usr/lpp/ssp/bin/dsh
MAKE     = /usr/bin/make

# Define some directory 
APP_DEF = $(CM_HOME)/etc/app_defaults
SCRIPT  = $(CM_HOME)/etc/sh_scripts
EXE_DIR = $(CM_HOME)/bin/$(MACH)
DATA    = $(CM_HOME)/etc/data

# Define some enviornment variables 
ASF_BIN = $(ASF)/bin

# Define the executable
EXE_FILES = pre_processor     correlator   \
            post_processor    ceos_ssp2    \
            util_copy         util_save    \
            util_unsave       util_cleanup \
            asf_ssp2          util_cleanup_save \
            input_prep 

FILE_SCRIPTS = run.ceos.F       run.post_processor.F \
               run.transfer.F \
               run.correlator.F run.pre_processor.F  \
               run.ceos.S       run.post_processor.S \
               run.transfer.S \
               run.correlator.S run.pre_processor.S  \
               run.util_cleanup run.util_cleanup_save\
               run.util_copy    run.util_save        \
               run.util_unsave                       \
               changeto         kill.script          \
               run.sim

# Global include options
SPS_INC        = /ASF/$(ASFVER)/include/sps 
MAIN_INCLUDE   = -DYYPURE -I$(SPS_INC)  
MPI_INCLUDE    = /usr/lpp/ppe.poe/include

# Global library
#SPS_LIB        = $(CM_HOME)/lib/$(MACH)/global/libsps.a
#SPS_CEOS_LIB   = $(CM_HOME)/lib/$(MACH)/global/libceos.a
SPS_LIB        = /ASF/$(ASFVER)/lib/libsps.a
SPS_CEOS_LIB   = /ASF/$(ASFVER)/lib/libceos.a
GLOBAL_LIB     = $(SPS_LIB) $(SPS_CEOS_LIB)

# Local include directory
LOCAL_INCLUDE  = $(CM_HOME)/include/local

# Local library 
EPHEM_LIB      = $(CM_HOME)/lib/$(MACH)/local/libephem.a
SSP_CEOS_LIB   = $(CM_HOME)/lib/$(MACH)/local/ssp_ceos.a

LOCAL_LIB      = $(EPHEM_LIB) $(SSP_CEOS_LIB)


# System library
TIME_LIB       = /usr/lib/libc_r.a


# Compilier options
#MPP_DIRECTIVE =  -qfixed=132
#MPP_DIRECTIVE =  -C -g -I/usr/lpp/ppe.poe/include   -qfixed=132
MPP_DIRECTIVE1 =  -Pv  -Wp,-H/usr/lpp/ppe.poe/include  
MPP_DIRECTIVE2 =       -Wp,-H$(LOCAL_INCLUDE)   -O3 -qfixed=132
MPP_DIRECTIVE  =  $(MPP_DIRECTIVE1) $(MPP_DIRECTIVE2)

# Compiler Debug flag
#DEBUG         = -D
DEBUG          = 

FCOMP          = mpxlf -I$(LOCAL_INCLUDE)
CCOMP          = mpcc -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE)

# Move the execute codes to ../bin/$(MACH)
app: $(GLOBAL_LIB) $(LOCAL_LIB) $(EXE_FILES) $(FILE_SCRIPTS)
	mv $(EXE_FILES) $(EXE_DIR)
	/bin/cp $(FILE_SCRIPTS) $(EXE_DIR)


clean:
	rm -f *.o *.lst
	rm -f $(EXE_FILES)

 
# TRANSFER
#
input_prep   : main_input_prep.o input_prep.o get_node.o  \
        cio.o rcp_ssp2.o get_config.o write_log.o ssp_get_bof.o \
        HEY.o ssp_get_rqst.o
	$(FCOMP)  -bmaxdata:0x20000000 main_input_prep.o \
	   write_log.o input_prep.o get_node.o cio.o rcp_ssp2.o get_config.o \
	   ssp_get_bof.o HEY.o ssp_get_rqst.o \
	   $(EPHEM_LIB) $(SPS_LIB) -lesslp2 -lm -lbsd $(TIME_LIB) \
	   -o input_prep

main_input_prep.o  : main_input_prep.c
	$(CCOMP)   -c main_input_prep.c
input_prep.o       : input_prep.f
	$(FCOMP) $(DEBUG) $(MPP_DIRECTIVE) -c input_prep.f
get_node.o         : get_node.c
	$(CCOMP)   -c get_node.c
rcp_ssp2.o         : rcp_ssp2.c
	cc -D_NO_PROTO -I/usr/lpp/ppe.poe/include -I$(LOCAL_INCLUDE) \
	   -c rcp_ssp2.c
#
# PRE-PROCESSOR
#
pre_processor: cio.o ZERO.o main_pre_processor.o \
        ZERO_TO_ONE.o COMMON_ZERO_ONE.o COMMON_ZERO_ONE_CEOS.o \
        COMMON_ONE_TWO.o get_config.o write_log.o pta.o pta_support.o \
        DISK_AND_CEOS.o ssp_get_rep.o HEY.o ssp_get_rqst.o  \
        load_aux_parameter.o ssp_get_aux.o load_ant_parameter.o \
        ssp_get_ant.o interp.o lfit.o fix_fd_b2.o clutter_prf_amb.o io.o
	$(FCOMP) $(MPP_DIRECTIVE) -bmaxdata:0x20000000 \
	    main_pre_processor.o write_log.o \
	    ZERO.o cio.o ZERO_TO_ONE.o COMMON_ZERO_ONE.o  \
	    COMMON_ZERO_ONE_CEOS.o COMMON_ONE_TWO.o get_config.o \
	    ssp_get_rep.o HEY.o ssp_get_rqst.o load_aux_parameter.o \
	    ssp_get_aux.o pta.o pta_support.o DISK_AND_CEOS.o \
	    load_ant_parameter.o ssp_get_ant.o interp.o lfit.o \
	    fix_fd_b2.o clutter_prf_amb.o  io.o \
	    $(EPHEM_LIB) $(SPS_LIB) -lesslp2 $(TIME_LIB) \
	    -o pre_processor
#
main_pre_processor.o: main_pre_processor.c
	$(CCOMP)     -c  main_pre_processor.c
ZERO.o: $(LOCAL_INCLUDE)/ssp2_const.inc ZERO.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) ZERO.f
pta.o : $(LOCAL_INCLUDE)/ssp2_const.inc pta.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) pta.f
pta_support.o : $(LOCAL_INCLUDE)/ssp2_const.inc pta_support.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) pta_support.f
lfit.o: lfit.f
	$(FCOMP) $(DEBUG) -c lfit.f
fix_fd_b2.o : $(LOCAL_INCLUDE)/ssp2_const.inc fix_fd_b2.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) fix_fd_b2.f
clutter_prf_amb.o : $(LOCAL_INCLUDE)/ssp2_const.inc clutter_prf_amb.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) clutter_prf_amb.f

#
#
#
# CORRELATOR
#
correlator: cio.o ONE.o main_correlator.o \
        THREE.o ZERO_TO_ONE.o ONE_TO_TWO.o COMMON_ZERO_ONE.o \
        COMMON_ZERO_ONE_CEOS.o COMMON_ONE_TWO.o get_config.o \
        write_log.o DISK_AND_CEOS.o ssp_get_rep.o HEY.o      \
        ssp_get_rqst.o load_aux_parameter.o ssp_get_aux.o    \
        hist_spec_compute.o load_ant_parameter.o ssp_get_ant.o \
        interp.o pta.o pta_support.o io.o
	$(FCOMP) $(MPP_DIRECTIVE)  -bmaxdata:0x20000000 \
	    main_correlator.o hist_spec_compute.o write_log.o \
	    THREE.o ONE.o cio.o ZERO_TO_ONE.o ONE_TO_TWO.o    \
	    COMMON_ZERO_ONE.o COMMON_ZERO_ONE_CEOS.o COMMON_ONE_TWO.o \
	    get_config.o ssp_get_rep.o HEY.o ssp_get_rqst.o   \
	    load_aux_parameter.o ssp_get_aux.o                \
	    DISK_AND_CEOS.o load_ant_parameter.o ssp_get_ant.o \
	    interp.o pta.o pta_support.o io.o \
            $(EPHEM_LIB) $(SPS_LIB) -lesslp2 $(TIME_LIB) \
	   -o correlator
#
main_correlator.o: main_correlator.c
	$(CCOMP)     -c  main_correlator.c
ONE.o: $(LOCAL_INCLUDE)/ssp2_const.inc ONE.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) ONE.f
hist_spec_compute.o: $(LOCAL_INCLUDE)/ssp2_const.inc hist_spec_compute.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) hist_spec_compute.f
#
#
write_log.o: write_log.c
	$(CCOMP)  -c  write_log.c
cio.o:  cio.c
	$(CCOMP)  -c   cio.c
io.o:	io.c
	$(CCOMP)  -c   io.c
# 
#
# INTERFACE
#
load_aux_parameter.o         : load_aux_parameter.f
	$(FCOMP)  $(DEBUG) -c $(MPP_DIRECTIVE) load_aux_parameter.f
ssp_get_aux.o: ssp_get_aux.c
	$(CCOMP) -c  -I$(SPS_INC) ssp_get_aux.c
#
ssp_get_bof.o: ssp_get_bof.c
	$(CCOMP) -c  -I$(SPS_INC) ssp_get_bof.c
ssp_get_eph.o: ssp_get_eph.c
	$(CCOMP) -c  -I$(SPS_INC) ssp_get_eph.c
ssp_get_rep.o: ssp_get_rep.c
	$(CCOMP) -c  -I$(SPS_INC) ssp_get_rep.c
ssp_get_rqst.o: ssp_get_rqst.c
	$(CCOMP) -c   -I$(SPS_INC)   ssp_get_rqst.c
ssp_get_ant.o: ssp_get_ant.c
	$(CCOMP) -c  -I$(SPS_INC) ssp_get_ant.c
load_ant_parameter.o: load_ant_parameter.f
	$(FCOMP) -c $(DEBUG)  $(MPP_DIRECTIVE) load_ant_parameter.f
interp.o: interp.f
	$(FCOMP) -c $(DEBUG)  $(MPP_DIRECTIVE) interp.f
HEY.o         : HEY.f
	$(FCOMP) -c   -qfixed=132 HEY.f
# 
#
THREE.o: $(LOCAL_INCLUDE)/ssp2_const.inc THREE.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) THREE.f
ONE_TO_TWO.o: $(LOCAL_INCLUDE)/ssp2_const.inc ONE_TO_TWO.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) ONE_TO_TWO.f
ZERO_TO_ONE.o: $(LOCAL_INCLUDE)/ssp2_const.inc ZERO_TO_ONE.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) ZERO_TO_ONE.f
get_config.o: $(LOCAL_INCLUDE)/ssp2_const.inc get_config.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) get_config.f
COMMON_ONE_TWO.o:$(LOCAL_INCLUDE)/ssp2_const.inc COMMON_ONE_TWO.f
	$(FCOMP)  $(DEBUG) $(MPP_DIRECTIVE) -c  COMMON_ONE_TWO.f
COMMON_ZERO_ONE.o:$(LOCAL_INCLUDE)/ssp2_const.inc COMMON_ZERO_ONE.f
	$(FCOMP)  $(DEBUG) $(MPP_DIRECTIVE) -c  COMMON_ZERO_ONE.f
COMMON_ZERO_ONE_CEOS.o:$(LOCAL_INCLUDE)/ssp2_const.inc COMMON_ZERO_ONE_CEOS.f
	$(FCOMP)  $(DEBUG) $(MPP_DIRECTIVE) -c  COMMON_ZERO_ONE_CEOS.f
DISK_AND_CEOS.o: $(LOCAL_INCLUDE)/ssp2_const.inc DISK_AND_CEOS.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) DISK_AND_CEOS.f
#
# POST-PROCESSOR
#
post_processor: cio.o TWO.o main_post_processor.o \
        post_processor.o COMMON_ONE_TWO.o THREE.o final_combine.o \
        ONE_TO_TWO.o get_config.o write_log.o DISK_AND_CEOS.o HEY.o \
        ssp_get_rqst.o
	$(FCOMP)  $(MPP_DIRECTIVE) -bmaxdata:0x20000000 \
	    main_post_processor.o write_log.o post_processor.o TWO.o \
	    cio.o COMMON_ONE_TWO.o THREE.o final_combine.o ONE_TO_TWO.o \
	    get_config.o HEY.o ssp_get_rqst.o DISK_AND_CEOS.o \
	    $(EPHEM_LIB) $(SPS_LIB) -lesslp2 $(TIME_LIB) \
	   -o post_processor
#
main_post_processor.o: main_post_processor.c
	$(CCOMP)     -c  main_post_processor.c
post_processor.o: post_processor.f $(LOCAL_INCLUDE)/ssp2_const.inc
	$(FCOMP)  $(DEBUG) $(MPP_DIRECTIVE) -c  post_processor.f
TWO.o: $(LOCAL_INCLUDE)/ssp2_const.inc TWO.f
	$(FCOMP) -c  $(DEBUG) $(MPP_DIRECTIVE) TWO.f
final_combine.o: final_combine.f
	$(FCOMP) -c $(DEBUG) $(MPP_DIRECTIVE)  final_combine.f
#
# CEOS
#
#
ceos_ssp2: cio.o main_ceos.o ceos_and_output.o \
        DISK_AND_CEOS.o write_log.o get_config.o COMMON_ONE_TWO.o \
        HEY.o ssp_get_rqst.o COMMON_ZERO_ONE_CEOS.o TWO.o
	$(FCOMP)  $(MPP_DIRECTIVE)   -bmaxdata:0x20000000 \
	    main_ceos.o write_log.o ceos_and_output.o cio.o \
	    get_config.o HEY.o ssp_get_rqst.o DISK_AND_CEOS.o \
	    COMMON_ONE_TWO.o COMMON_ZERO_ONE_CEOS.o TWO.o \
	    $(EPHEM_LIB) $(SPS_LIB) \
	    $(SSP_CEOS_LIB) $(SPS_CEOS_LIB) $(TIME_LIB)  -lisode \
	-o ceos_ssp2
#
main_ceos.o: main_ceos.c
	$(CCOMP)     -c  main_ceos.c
ceos_and_output.o: $(LOCAL_INCLUDE)/ssp2_const.inc ceos_and_output.f
	$(FCOMP) -c  $(DEBUG) $(MPP_DIRECTIVE) ceos_and_output.f
#
#
#   UTIL
#
util_copy: util_copy.o cio.o
	$(CCOMP) -o util_copy util_copy.o cio.o
util_save: util_save.o cio.o
	$(CCOMP) -o util_save util_save.o cio.o
util_unsave: util_unsave.o cio.o
	$(CCOMP) -o util_unsave util_unsave.o cio.o
util_cleanup: util_cleanup.o cio.o
	$(CCOMP) -o util_cleanup util_cleanup.o cio.o
util_cleanup_save: util_cleanup_save.o cio.o
	$(CCOMP) -o util_cleanup_save util_cleanup_save.o cio.o

util_copy.o: util_copy.c
	$(CCOMP) -c util_copy.c
util_save.o: util_save.c
	$(CCOMP) -c util_save.c
util_unsave.o: util_unsave.c
	$(CCOMP) -c util_unsave.c
util_cleanup.o: util_cleanup.c
	$(CCOMP) -c util_cleanup.c
util_cleanup_save.o: util_cleanup_save.c
	$(CCOMP) -c util_cleanup_save.c
#
#
#  asf_ssp2 program interface with the INTERFACE
#
asf_ssp2: MAIN.o run_the_code.o ssp2_scansar.o cio.o \
        write_log.o dump_odl_to_disk.o
	xlc_r4 MAIN.o ssp2_scansar.o run_the_code.o cio.o \
	    dump_odl_to_disk.o write_log.o $(SPS_LIB) /usr/lib/libxlf90.a \
	   -o asf_ssp2
MAIN.o: MAIN.c
	xlc_r4 -c $(MAIN_INCLUDE) -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) MAIN.c
run_the_code.o: run_the_code.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) run_the_code.c
ssp2_scansar.o: ssp2_scansar.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) ssp2_scansar.c
dump_odl_to_disk.o: dump_odl_to_disk.c
	xlc_r4 -c -I$(SPS_INC)  -I$(MPI_INCLUDE)  -I$(LOCAL_INCLUDE) \
	      dump_odl_to_disk.c
