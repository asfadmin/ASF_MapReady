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

# Build the libpng library.
build_png_stamp: 
	tar xzvf $(TARBALL)
	(cd $(UNPACK_DIR) ; \
	    cp scripts/makefile.std Makefile ; \
            make test)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_png_stamp
