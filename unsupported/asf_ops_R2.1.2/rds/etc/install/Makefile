#
# @(#)Makefile	1.2 96/11/12 18:14:48
#
# Makefile for installing RDS configuration file(s)
#

ECHO		= /bin/echo
INSTALLDIR	= $(LOCAL)/rds

RDS_CONFIG	= rds-config.odl

CONFIGFILES	= $(RDS_CONFIG)

#MAN_PATH	= $(ASF)/man
MAN_PATH	= ../etc/man
MAN_EXT		= 1

bin_inst:	all

all:	install

install:	$(CONFIGFILES)
	install -v -m 644 -F $(INSTALLDIR) $(CONFIGFILES)

#
# No man pages
#
install.man:
	@ $(ECHO) No man pages in `pwd` -- skipping installation

#	install -v -m 444 -src $(START_CP).man \
#	    -F $(MAN_PATH)/man$(MAN_EXT) $(START_CP).$(MAN_EXT)

#INSTALLDIR	= ~sps
#SCRIPTS		=
#SRC		= $(SCRIPTS)
#
#all:
#
#install:
#	@echo Install these files manually on the SGI, Sun, IBM, and DEC Alpha
