#===============================================================================
# Filename:	Makefile
# Description:	makefile for etc
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#               @(#)Makefile	1.2  12/18/96
#===============================================================================

default: config/Makefile db/Makefile man/Makefile sh_scripts/Makefile
	(cd config; $(MAKE) default)
	(cd db; $(MAKE) default)
	(cd man; $(MAKE) default)
	(cd sh_scripts; $(MAKE) default)

source: config/Makefile db/Makefile man/Makefile sh_scripts/Makefile
	(cd config; $(MAKE) source)
	(cd db; $(MAKE) source)
	(cd man; $(MAKE) source)
	(cd sh_scripts; $(MAKE) source)

all:
	$(MAKE) default

install: config/Makefile db/Makefile man/Makefile sh_scripts/Makefile
	(cd config; $(MAKE) install)
	(cd db; $(MAKE) install)
	(cd man; $(MAKE) install)
	(cd sh_scripts; $(MAKE) install)

%/Makefile: $(PPS_TREE)/etc/%/SCCS/s.Makefile
	(cd $*; sccs get $(PPS_TREE)/etc/$*/SCCS/s.Makefile)
