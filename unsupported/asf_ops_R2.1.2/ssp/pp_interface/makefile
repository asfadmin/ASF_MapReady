# SCCS_Id = @(#)makefile	2.41 3/24/98

ASF            = /ASF/$(ASFVER)

SPS_INC        = /ASF/$(ASFVER)/include/sps 
MAIN_INCLUDE   = -DYYPURE -I$(SPS_INC)  
MPI_INCLUDE    = /usr/lpp/ppe.poe/include

LOCAL_INCLUDE  = $(CM_HOME)/include/local
EXE_DIR        = $(CM_HOME)/bin/$(MACH)

SPS_LIB     = /ASF/$(ASFVER)/lib/libsps.a
CEOS_LIB    = /ASF/$(ASFVER)/lib/libceos.a

#
#
#  asf_pp program interface with the INTERFACE
#
OBJS =  MAIN_pp.o \
	run_pp.o  \
	pp.o \
	cio.o \
	write_log.o \
	dump_odl_to_disk.o \
	run_the_code.o \
	ssp2_scansar.o 

EXE_FILES= asf_pp

app : $(EXE_FILES)
	mv $(EXE_FILES) $(EXE_DIR)

asf_pp: $(OBJS)
	xlc_r4  -o asf_pp $(OBJS) $(SPS_LIB) /usr/lib/libxlf90.a 

MAIN_pp.o: MAIN_pp.c
	xlc_r4 -c $(MAIN_INCLUDE) -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) MAIN_pp.c 
run_pp.o: run_pp.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) run_pp.c
pp.o: pp.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) pp.c
cio.o: cio.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) cio.c
write_log.o: write_log.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) write_log.c
dump_odl_to_disk.o: dump_odl_to_disk.c
	xlc_r4 -c -I$(SPS_INC) -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) dump_odl_to_disk.c
run_the_code.o: run_the_code.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) run_the_code.c
ssp2_scansar.o: ssp2_scansar.c
	xlc_r4 -c  -I$(MPI_INCLUDE) -I$(LOCAL_INCLUDE) ssp2_scansar.c

clean:
	rm -f *.o *.lst
	rm -f $(EXE_FILES) 
