#
# @(#)Makefile	1.5 96/10/09 19:10:05
#
# This is the top-level Makefile for the GNU toolkit directory.
#
# Usage:
#     make all		- Build all executables within the directory tree
#     make clean	- Remove executables and object files within the
#			  directory tree
#     make install	- Install binaries
#
# bison must be built and installed before flex.
#
# This Makefile was originally developed under SGI IRIX, Release 5.2.
#
# BISON_PATH is needed for the bison installation only.  Don't try to use
# a $(BISON_PATH) construct to pass its value into make; it won't work.
# Also, make sure that BISON_PATH is not set in your environment because
# it will override the value set in this Makefile.
#

#
# Set the shell explicitly because the loop directives use Bourne shell
# syntax.
#
SHELL	= /bin/sh

glib:	all
all:
	@(BISON_PATH=`pwd`; export BISON_PATH; \
		cd bison; echo Making in `pwd`; \
		$(MAKE) all; \
		$(MAKE) install)
	@(cd flex; echo Making in `pwd`; $(MAKE) all)

clean:
	@for subdir in \
            bison flex ; \
                do (cd $$subdir; \
                    echo Cleaning directory `pwd`; \
                    $(MAKE) $@ ) ; \
                done
	-/bin/rm -rf bin/* include/* info/* lib/* man/*

install:
	@for subdir in \
            bison flex ; \
                do (BISON_PATH=`pwd`; export BISON_PATH; \
		    cd $$subdir; \
                    echo Making in `pwd`; \
		    $(MAKE) $@) ; \
                done
