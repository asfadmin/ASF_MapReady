# SCCS_id = @(#)makefile	2.41 3/24/98

SPS_INC        = /ASF/$(ASFVER)/include/sps
MAIN_INCLUDE   = -DYYPURE -I$(SPS_INC)
MPI_INCLUDE    = /usr/lpp/ppe.poe/include
LOCAL_INCLUDE  = $(CM_HOME)/include/local

EXE_DIR        = $(CM_HOME)/bin/$(MACH)


FCOMP          = mpxlf -I$(LOCAL_INCLUDE)
CCOMP          = mpcc -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE)

# Global library
SPS_LIB        = /ASF/$(ASFVER)/lib/libsps.a

# Local library
EPHEM_LIB      = $(CM_HOME)/lib/$(MACH)/local/libephem.a

# System library
TIME_LIB       = /usr/lib/libc_r.a


MPP_DIRECTIVE =  -C -g -I/usr/lpp/ppe.poe/include   -qfixed=132

EXE_FILES      = pp_input_prep
FILE_SCRIPTS   = run.pp_transfer.F run.pp_transfer.S

app : $(EXE_FILES)
	/bin/mv $(EXE_FILES) $(EXE_DIR)
	/bin/cp $(FILE_SCRIPTS) $(EXE_DIR)

clean : 
	/bin/rm -f *.o *.lst
	/bin/rm -f $(EXE_FILES)

# TRANSFER
#
pp_input_prep   : main_pp_input_prep.o pp_input_prep.o pp_get_node.o cio.o \
		  transfer.o get_config.o write_log.o HEY.o ssp_get_rqst.o 
	$(FCOMP)  -bmaxdata:0x20000000 main_pp_input_prep.o  \
		pp_input_prep.o pp_get_node.o cio.o transfer.o\
		get_config.o write_log.o HEY.o ssp_get_rqst.o \
	    $(EPHEM_LIB) $(SPS_LIB) -lesslp2 -lm -lbsd $(TIME_LIB) \
	   -o pp_input_prep

main_pp_input_prep.o : main_pp_input_prep.c
	$(CCOMP) -c main_pp_input_prep.c
pp_input_prep.o : pp_input_prep.f
	$(FCOMP) $(MPP_DIRECTIVE) -c pp_input_prep.f
pp_get_node.o : pp_get_node.c
	$(CCOMP) -c pp_get_node.c
cio.o : cio.c
	$(CCOMP) -c cio.c
transfer.o   : transfer.c
	cc -D_NO_PROTO -I/usr/lpp/ppe.poe/include  -I$(LOCAL_INCLUDE) \
	   -c transfer.c
get_config.o: $(LOCAL_INCLUDE)/ssp2_const.inc get_config.f
	$(FCOMP) -c $(MPP_DIRECTIVE) get_config.f
write_log.o: write_log.c
	$(CCOMP)  -c  write_log.c
HEY.o         : HEY.f
	$(FCOMP) -c   -qfixed=132 HEY.f
ssp_get_rqst.o: ssp_get_rqst.c
	$(CCOMP) -c   -I$(SPS_INC)   ssp_get_rqst.c
#

