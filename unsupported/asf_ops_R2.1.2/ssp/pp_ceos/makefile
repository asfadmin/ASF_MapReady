# SCCS: @(#)makefile	2.38 1/22/98

INCLUDE = /ASF/$(ASFVER)/include/sps
LOCAL_INCLUDE = $(CM_HOME)/include/local
MPI_INCLUDE   = /usr/lpp/ppe.poe/include

EXE_DIR     = $(CM_HOME)/bin/$(MACH)

SPS_LIB     = /ASF/$(ASFVER)/lib/libsps.a
CEOS_LIB    = /ASF/$(ASFVER)/lib/libceos.a

EPHEM_LIB   = $(CM_HOME)/lib/$(MACH)/local/libephem.a

# Using the fortran compiler
FCOMP   = xlf -I$(LOCAL_INCLUDE) -I$(MPI_INCLUDE)
CCOMP   = xlc -I$(LOCAL_INCLUDE) -I$(MPI_INCLUDE)
OBJS	= main.o fill_desc_rec.o fill_proc_rec.o \
	  get_frame_rqst.o misc.o get_aux_file.o get_cali_file.o \
	  write_ceos.o read_info.o localtime_r.o config.o 
	  

FILE_SCRIPTS = run.pp_ceos.F run.pp_ceos.S
EXE_FILES   = ceos_pp

app: $(EXE_FILES)
	/bin/mv $(EXE_FILES) $(EXE_DIR)
	/bin/cp $(FILE_SCRIPTS) $(EXE_DIR)

clean:
	/bin/rm -f *.o *.lst
	/bin/rm -f $(EXE_FILES)

ceos_pp: $(OBJS)
	$(FCOMP) -o ceos_pp $(OBJS) $(SPS_LIB) $(CEOS_LIB) $(EPHEM_LIB) -lm 


#$(CCOMP) -o ceos_pp $(OBJS) $(SPS_LIB) $(CEOS_LIB) -lm  /usr/lib/libc_r.a 


#
main.o: main.c 
	$(CCOMP) -c  main.c

fill_desc_rec.o: fill_desc_rec.c 
	$(CCOMP) -c  fill_desc_rec.c

fill_proc_rec.o: fill_proc_rec.c 
	$(CCOMP) -c   fill_proc_rec.c

config.o: config.c 
	$(CCOMP) -c  config.c

get_frame_rqst.o: get_frame_rqst.c 
	$(CCOMP) -c get_frame_rqst.c  -I$(INCLUDE)

misc.o: misc.c
	$(CCOMP) -c misc.c 

get_aux_file.o: get_aux_file.c 
	$(CCOMP) -c get_aux_file.c   -I$(INCLUDE)

get_cali_file.o: get_cali_file.c
	$(CCOMP) -c get_cali_file.c   -I$(INCLUDE)

write_ceos.o: write_ceos.c
	$(CCOMP) -c write_ceos.c    -I$(INCLUDE)

read_info.o: read_info.c
	$(CCOMP) -c read_info.c 
localtime_r.o: localtime_r.c
	$(CCOMP) -c localtime_r.c
#
#
#
#
