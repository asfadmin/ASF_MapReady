# This makefile unpacks and builds the Portable Network Graphics (PNG)
# library and installs it into the ASF tool tree.

include ../../make_support/system_rules

# Names of the tarball and the unpack directory.
UNPACK_DIR := libpng-1.2.5
TARBALL := $(UNPACK_DIR).tar.gz

# Following the policy of installing headers in directories with the
# same names as the modules they describe, we want to put the headers
# in this directory.
PNG_INCLUDE_DIR := $(ASF_INCLUDE_DIR)/png

# Copy the static libraries and headers into the appropriate places in
# the ASF directory structure.
install: build_stamp
	cp $(UNPACK_DIR)/libpng.a $(LIBDIR)
	install -d $(PNG_INCLUDE_DIR)
	cp $(UNPACK_DIR)/png.h $(PNG_INCLUDE_DIR)
	cp $(UNPACK_DIR)/pngconf.h $(PNG_INCLUDE_DIR)

# Build the library.
build_stamp: 
	tar xzvf $(TARBALL)
# It seems like some junk got in this header upstream, stripping it
# out makes everything work fine.  Should check if its gone when we go
# to the next libpng version.
	(cd $(UNPACK_DIR) ; \
	    perl -p -i \
                 -e 's/^\s*__png\.h__ already includes setjmp\.h;\s*\n//' \
                 pngconf.h ; \
	    perl -p -i -e 's/^\s*__dont__ include it again.;\s*\n//' \
                 pngconf.h) 
# We need to use the compiler we want, and we need to explain where to
# find the zlib library, so we change the supplied Makefile slightly,
# and do a diff to try to ensure the changes took.  Of course, some
# changes could succeed, and others fail...
	(cd $(UNPACK_DIR) ; \
	    cp scripts/makefile.std Makefile ; \
	    echo $(CC) ; \
	    perl -p -i.old \
	         -e 's/^\s*CC\s*=\s*cc\s*$$/CC=$(CC)\n/;' \
                 -e 's{^\s*ZLIBLIB\s*=\s*.*$$}{ZLIBLIB=$(LIBDIR)};' \
                 -e 's{^\s*ZLIBINC\s*=\s*.*$$}{ZLIBINC=$(ASF_INCLUDE_DIR)/z}' \
                 Makefile ; \
	    diff Makefile.old Makefile >/dev/null ; \
            diff_exit_status=$$? ; \
            if [ $$diff_exit_status -eq 0 ] ; then \
                echo "file $(UNPACK_DIR)/Makefile not changed as expected" ; \
                exit 1 ; \
            elif [ $$diff_exit_status -ge 2 ] ; then \
                echo "diff failed somehow" ; \
                exit 1 ; \
            fi ; \
	    rm Makefile.old ; \
            make test)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_stamp
