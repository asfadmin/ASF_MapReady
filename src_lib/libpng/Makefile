# This makefile unpacks and builds the Portable Network Graphics (PNG)
# library and installs it into the ASF tool tree.

include ../../make_support/system_rules

# Names of the tarball and the unpack directory.
UNPACK_DIR := libpng-1.2.5
TARBALL := $(UNPACK_DIR).tar.gz

# Following the policy of installing headers in directories with the
# same names as the modules they describe, we want to put the png.h
# heaer in this directory.
PNG_INCLUDE_DIR := $(ASF_INCLUDE_DIR)/png

# Copy the static libraries and headers for the GSL into the
# appropriate places in the ASF directory structure.
install_png: build_png_stamp
	cp $(UNPACK_DIR)/libpng.a $(LIBDIR)
	install -d $(PNG_INCLUDE_DIR)
	cp $(UNPACK_DIR)/png.h $(PNG_INCLUDE_DIR)
	cp $(UNPACK_DIR)/pngconf.h $(PNG_INCLUDE_DIR)

# Build the libpng library.
build_png_stamp: 
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
	(cd $(UNPACK_DIR) ; \
	    cp scripts/makefile.std Makefile ; \
            make test)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_png_stamp
