#
# @(#)Makefile	1.4 96/10/10 12:19:47
#
# Install the CP global header files not belonging to any libraries.
#
# Usage:
#     make glib_inst               - Install global header files
#
# Builds leave executables and libraries under ./bin and ./lib.
# Installations copy files to $ASF and $LOCAL.
#
# Environment:
#     MACH should be set to the hardware platform identifier.
#     Accepted values are "alpha," "ibm," "sgi," and "sparc."
#     ("other" and "smart" are preserved for historical reasons but
#     are not officially supported for R1.4.)
#
# This Makefile was originally developed under SGI IRIX, Release 5.2.
#

#
# Set the shell explicitly because the loop directives use Bourne shell
# syntax.
#
SHELL	= /bin/sh

MKDIR		= /bin/mkdir -p
CP		= /bin/cp -p
RM		= /bin/rm -f
INCLUDEFILES	= *.h
INSTALLDIR	= $(ASF)/include/sps

glib_inst:
	-$(RM) $(INSTALLDIR)/$(INCLUDEFILES)
	-$(MKDIR) $(INSTALLDIR)
	$(CP) $(INCLUDEFILES) $(INSTALLDIR)
