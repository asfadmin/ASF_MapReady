#
# @(#)Makefile	1.2 96/10/09 19:13:37
#
# This is the top-level Makefile for the RDS libraries and applications.
#
# Usage:
#     make glib		- Build sarproc.a
#     make glib_inst    - Install global library and global header files
#     make app          - Build all executables
#     make clean	- Remove object files from the source directories
#     make install.man  - Install man pages only
#
# This Makefile was originally developed under SGI IRIX, Release 5.2.
#

#
# Set the shell explicitly because the loop directives use Bourne shell
# syntax.
#
SHELL	= /bin/sh

glib:
	@for subdir in \
            toolkit sarproc sps ; \
                do (cd $$subdir; \
                    echo Building in `pwd`; \
                    $(MAKE) $@ ) ; \
                done

glib_inst:
	@for subdir in \
            sps toolkit sarproc ; \
                do (cd $$subdir; \
                    echo Installing libraries and header files for `pwd`; \
                    $(MAKE) $@ ) ; \
                done


app:
	@for subdir in \
	    chkangl rds rewind stopit taperead ; \
                do (cd $$subdir; \
                    echo Making in `pwd`; \
                    $(MAKE) $@ ) ; \
                done

install.man:
	@for subdir in \
	    chkangl rds rewind stopit taperead ; \
                do (cd $$subdir; \
                    echo Installing from directory `pwd`; \
                    $(MAKE) $@ ) ; \
                done

clean:
	@for subdir in \
            sarproc sps toolkit\
	    chkangl rds rewind stopit taperead ; \
                do (cd $$subdir; \
                    echo Cleaning directory `pwd`; \
                    $(MAKE) $@ ) ; \
                done
