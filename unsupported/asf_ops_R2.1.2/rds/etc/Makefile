#
# @(#)Makefile	1.4 96/10/10 11:40:34
#
# Install items from the RDS etc directory.
#
# Usage:
#     make bin_inst                - Install X application defaults files,
#                                    configuration files, scripts
#                                    (RDS currently has only a config file)
#     make install.man             - Install man pages
#
# Builds leave executables and libraries under ./bin and ./lib.
# Installations copy files to $ASF and $LOCAL.
#
# This Makefile was originally developed under SGI IRIX, Release 5.2.
#

#
# Set the shell explicitly because the loop directives use Bourne shell
# syntax.
#
SHELL	= /bin/sh

MKDIR		= /bin/mkdir -p
INSTALLMAN	= $(ASF)/man

bin_inst:
	@for subdir in \
            install ; \
                do (cd $$subdir; \
                    echo Installing from directory `pwd`; \
                    $(MAKE) $@ ) ; \
                done

install.man:
	-$(MKDIR) $(INSTALLMAN)/man1
	@for subdir in \
	    man1 ; \
		do ( \
		    cd man/$$subdir; \
		    echo Installing man pages from directory `pwd` ; \
		    tar cf - . | (cd $(INSTALLMAN)/$$subdir; tar xfB -) ; \
		    cd ../.. ) ; \
		done
