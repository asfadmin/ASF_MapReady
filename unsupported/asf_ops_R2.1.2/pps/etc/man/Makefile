#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#               @(#)Makefile	1.1  09 Dec 1996
#===============================================================================

SUB_PATH      =   etc/man
MAN_INSTALL_DIR =   /ASF/$(ASFVER)/man
MAN3_DIR        =   man3
MAN_FILES = \
		recv_job_from_PPS.3		\
		send_status_to_PPS.3

default: $(MAN_FILES)

source:
	$(MAKE) default

all:
	$(MAKE) default

install:
	-/bin/mkdir $(MAN_INSTALL_DIR)
	-/bin/mkdir $(MAN_INSTALL_DIR)/$(MAN3_DIR)
	(cd $(MAN_INSTALL_DIR)/$(MAN3_DIR); /bin/rm -f $(MAN_FILES))
	/bin/cp -p $(MAN_FILES) $(MAN_INSTALL_DIR)/$(MAN3_DIR)

%.3: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.3
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.3
