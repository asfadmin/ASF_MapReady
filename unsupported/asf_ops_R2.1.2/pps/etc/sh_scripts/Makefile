#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	Makefile for PPS set-up and start-up script
# Notes:	
#
# SCCS Info:
#            @(#)Makefile	1.4  10/31/97
#===============================================================================

.IMPORT:	ASFVER

SUB_PATH    =   etc/sh_scripts
DELIVER_DIR =	/ASF/$(ASFVER)/bin
CONFIG_DIR =	/LOCAL/$(ASFVER)/pps/config

SCRIPT_FILES = \
	PPS_env.csh		\
	PPS_pingserver.csh	\
	PPS_report_cron.csh	\
	PPS_startgui.csh	\
	PPS_startserver.csh	\
	PPS_stopserver.csh

CRON_FILE = pps_crontab
LOG_FILE = /LOCAL/pps/logs/PPSrun.log


default: $(SCRIPT_FILES) 
	chmod a+x $(SCRIPT_FILES)

source:
	$(MAKE) default

all:
	$(MAKE) default

install:
	(cd $(DELIVER_DIR); /bin/rm -f $(SCRIPT_FILES))
	cp -p $(SCRIPT_FILES) $(DELIVER_DIR)
	$(MAKE) $(CRON_FILE)

$(CRON_FILE):
	-/bin/rm -f $(CONFIG_DIR)/$(CRON_FILE)
	(cd $(CONFIG_DIR); \
	echo "22 0,2,4,6,8,10,12,14,16,18,20,22 * * * $(ASF)/bin/PPS_report_cron.csh >> $(LOG_FILE) 2>&1" > $(CRON_FILE))
	chmod 0444 $(CONFIG_DIR)/$(CRON_FILE)
	crontab $(CONFIG_DIR)/$(CRON_FILE)

%.csh: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.csh
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.csh
