# SccsId = @(#)makefile	2.41 3/24/98
#
# Define environment variables
ASF      = /ASF/$(ASFVER)
LOCAL    = /LOCAL/$(ASFVER)

# Define some commands for IBM SP-2
DSH      = /usr/lpp/ssp/bin/dsh
MAKE = /usr/bin/make

# Define some directory
APP_DEF = $(CM_HOME)/etc/app_defaults
SCRIPT  = $(CM_HOME)/etc/sh_scripts
EXE_DIR = $(CM_HOME)/bin/$(MACH)
DATA    = $(CM_HOME)/etc/data
LOCAL_LIBS = $(CM_HOME)/lib/$(MACH)/local


# Define some enviornment variables
ASF_BIN = $(ASF)/bin

SUBDIRS= ephem_dir scansar_ceoslib scansar_correlator \
         pp_interface pp_transfer pp_correlator pp_ceos



bin_inst: setup_asf_dir

setup_everything: setup_asf_dir setup_nodes setup_local_dir

setup_asf_dir:
	mkdir -p $(ASF_BIN)
	cd $(EXE_DIR); cp * $(ASF_BIN)
	chown sps.sps $(ASF_BIN)/* 
	chmod 755 $(ASF_BIN)/*
	chown root $(ASF_BIN)/pp_input_prep
	chmod 4755 $(ASF_BIN)/pp_input_prep
	chown root $(ASF_BIN)/input_prep
	chmod 4755 $(ASF_BIN)/input_prep

	touch $(ASF_BIN)/run.host
	echo "Need to edit $(ASF_BIN)/run.host"
	touch $(ASF_BIN)/run.save
	cd $(ASF_BIN); chmod ug+w run.*; chmod +x changeto; ./changeto F
	chown sps.sps $(ASF_BIN)/run.* 

	cp $(APP_DEF)/eng_config.sim  $(ASF)/bin/eng_config.sim
	sed s#LOCAL_DIR#$(LOCAL)# $(APP_DEF)/eng_config.install > /tmp/config
	sed s#ASF_DIR#$(ASF)# /tmp/config > $(ASF_BIN)/eng_config.ssp2
	rm -f /tmp/config
	chown sps.sps $(ASF_BIN)/eng_config.*

	mkdir -p      $(ASF_BIN)/SAVE_DIR
	rm -f         $(ASF_BIN)/SAVE_DIR/*
	chown sps.sps $(ASF_BIN)/SAVE_DIR
	chmod 775     $(ASF_BIN)/SAVE_DIR

setup_nodes:
	$(DSH) -a mkdir -p       /home/tmpdisk/ANT
	$(DSH) -a cp  $(DATA)/*  /home/tmpdisk/ANT
	$(DSH) -a chown sps.sps  /home/tmpdisk/ANT/*
	$(DSH) -a chmod 775      /home/tmpdisk/ANT/*

setup_local_dir:
	mkdir -p	                  $(LOCAL)/ssp
	touch                             $(LOCAL)/ssp/sps_frame_request.odl
	chown sps.sps                     $(LOCAL)/ssp/sps_frame_request.odl
	chmod 664                         $(LOCAL)/ssp/sps_frame_request.odl
	cp  $(APP_DEF)/sim_updated.odl    $(LOCAL)/ssp/sim_updated.odl
	chown sps.sps                     $(LOCAL)/ssp/sim_updated.odl
	chmod 664                         $(LOCAL)/ssp/sim_updated.odl
	cp $(APP_DEF)/TIME.DAT            $(LOCAL)/ssp/TIME.DAT
	chown sps.sps                     $(LOCAL)/ssp/TIME.DAT
	chmod 664                         $(LOCAL)/ssp/TIME.DAT
	cp $(APP_DEF)/time_table.dat      $(LOCAL)/ssp/time_table.dat
	chown sps.sps                     $(LOCAL)/ssp/time_table.dat
	chmod 664                         $(LOCAL)/ssp/time_table.dat
	touch                             $(LOCAL)/ssp/status.dat
	chown sps.sps                     $(LOCAL)/ssp/status.dat
	chmod 664                         $(LOCAL)/ssp/status.dat
	touch                             $(LOCAL)/ssp/error.dat
	chown sps.sps                     $(LOCAL)/ssp/error.dat
	chmod 664                         $(LOCAL)/ssp/error.dat
	touch                             $(LOCAL)/ssp/DUMP
	chown sps.sps                     $(LOCAL)/ssp/DUMP
	chmod 664                         $(LOCAL)/ssp/DUMP


app: 
	@set -e; for i in $(SUBDIRS); do (cd $$i; $(MAKE)) ; done

clean:
	@set -e; for i in $(SUBDIRS); do (cd $$i; $(MAKE) clean) ; done
	/bin/rm -f $(EXE_DIR)/*
	/bin/rm -f $(LOCAL_LIBS)/*

	  
