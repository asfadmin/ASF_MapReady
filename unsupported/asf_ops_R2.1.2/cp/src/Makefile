#
# @(#)Makefile	1.8 96/10/09 19:09:07
#
# This is the top-level Makefile for the CP libraries and applications.
#
# Usage:
#     make glib		- Build and install bison, flex under "toolkit";
#                         then build libsps.a
#     make glib_inst    - Install global library and global header files
#     make app          - Build all executables
#     make clean	- Remove object files from the source directories
#     make install.man  - Install man pages only
#
# Note that
#     - "toolkit" binaries must be built and installed before libsps.a
#     - libsps.a should be built and installed before any of the subsystem
#       executables, e.g., "CP," "rds"
#
# This Makefile was originally developed under SGI IRIX, Release 5.2.
#
# Note that the CP-PPS application is not installed on the SGI; only its
# man page is.  The actual installation of CP-PPS must be done manually
# on the Sun.
#
#

#
# Set the shell explicitly because the loop directives use Bourne shell
# syntax.
#
SHELL	= /bin/sh

glib:
	@for subdir in \
            toolkit sps ceos ; \
                do (cd $$subdir; \
                    echo Building in `pwd`; \
                    $(MAKE) $@ ) ; \
                done

glib_inst:
	@for subdir in \
            sps ceos ; \
                do (cd $$subdir; \
                    echo Installing libraries and header files for `pwd`; \
                    $(MAKE) $@ ) ; \
                done

app:
	@for subdir in \
            CP CP-IMS CP_image_avg CP_qc CP_scan_qc \
	    ceos xgrabsc-2.3 log_browser ; \
                do (cd $$subdir; \
                    echo Making in `pwd`; \
                    $(MAKE) $@ ) ; \
                done

install.man:
	@for subdir in \
            CP CP-IMS CP-PPS CP_image_avg CP_qc CP_scan_qc \
	    log_browser xgrabsc-2.3 ; \
                do (cd $$subdir; \
                    echo Installing from directory `pwd`; \
                    $(MAKE) $@ ) ; \
                done

clean:
	@for subdir in \
            toolkit sps \
            CP CP-IMS CP-PPS CP_image_avg CP_qc CP_scan_qc \
	    ceos log_browser xgrabsc-2.3 ; \
                do (cd $$subdir; \
                    echo Cleaning directory `pwd`; \
                    $(MAKE) $@ ) ; \
                done
