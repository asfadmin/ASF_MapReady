#===============================================================================
# Filename:	Makefile
# Description:	makefile for config
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#               @(#)Makefile	1.1  11/20/96
#===============================================================================

CONFIG_FILE =   pps.config
SUB_PATH    =   etc/config

PIXMAP_FILES = \
		bearhd0.xpm	\
		bearwait2.xpm	\
		happybr0.xpm	\
		tigerfrown.xpm	\
		whitebear3.xpm

default: $(PIXMAP_FILES) $(CONFIG_FILE)

source:
	$(MAKE) default

all:
	$(MAKE) default

install: 
	-/bin/mkdir /LOCAL/$(ASFVER)/pps
	-/bin/mkdir /LOCAL/$(ASFVER)/pps/app-defaults
	-/bin/mkdir /LOCAL/$(ASFVER)/pps/app-defaults/pixmaps
	-/bin/rm -f /LOCAL/$(ASFVER)/pps/app-defaults/pixmaps/*.xpm
	/bin/cp -p *.xpm /LOCAL/$(ASFVER)/pps/app-defaults/pixmaps
	-/bin/mkdir /LOCAL/$(ASFVER)/pps/config


%.config: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.config
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.config

%.xpm: $(PPS_TREE)/$(SUB_PATH)/%.xpm
	/bin/cp -p $(PPS_TREE)/$(SUB_PATH)/$*.xpm .
