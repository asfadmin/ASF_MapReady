# This makefile unpacks and builds the zlib compression library and
# installs it into the ASF tool tree.

include ../../make_support/system_rules

# Names of the tarball and the unpack directory.
UNPACK_DIR := zlib-1.2.1
TARBALL := $(UNPACK_DIR).tar.gz

# Following the policy of installing headers in directories with the
# same names as the modules they describe, we want to put the headers
# in this directory.
ZLIB_INCLUDE_DIR := $(ASF_INCLUDE_DIR)/zlib

# Directory to stage built files on the way to installing them.
STAGE_DIR := $(shell pwd)/stage_dir

# Copy the static libraries and headers into the appropriate places in
# the ASF directory structure.
install: stage_stamp
	cp $(STAGE_DIR)/lib/libz.a $(LIBDIR)
	install -d $(ZLIB_INCLUDE_DIR)
	cp $(STAGE_DIR)/include/zlib.h $(ZLIB_INCLUDE_DIR)
	cp $(STAGE_DIR)/include/zconf.h $(ZLIB_INCLUDE_DIR)

# Build and stage the library.
stage_stamp: 
	tar xzvf $(TARBALL)
	(cd $(UNPACK_DIR) ; \
	    ./configure --prefix=$(STAGE_DIR) ; \
            make test ; \
            make install)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) $(STAGE_DIR) stage_stamp
