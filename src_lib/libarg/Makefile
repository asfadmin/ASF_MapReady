# Build and install a copy of the argtable library (a command-line options
# library) into the standard location in the ASF tool tree.

include ../../make_support/system_rules

# Where we temporarily stage the library on the way to installing it in
# the ASF tree.
ROOT_STAGEDIR := stage_dir
LIB_STAGEDIR := $(ROOT_STAGEDIR)/lib
INCLUDE_STAGEDIR := $(ROOT_STAGEDIR)/include

# These variables specify the tarball and what it unpacks into.
UNPACK_DIR := argtable2
TARFILE := argtable-2.3.tar
ZIPFILE := $(TARFILE).gz

# Copy the static libraries and header into the appropriate places in
# the ASF directory structure.
install: stage_stamp
	cp $(LIB_STAGEDIR)/*.a $(LIBDIR)
	cp -r $(INCLUDE_STAGEDIR)/* $(ASF_INCLUDE_DIR)

# Build the library into the staging dir
stage_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	gunzip --to-stdout $(ZIPFILE) >$(TARFILE)
	tar xvf $(TARFILE)
	cd $(UNPACK_DIR) ; \
           ./configure --prefix=`pwd`/../$(ROOT_STAGEDIR) ; \
	   make ; \
	   make check ; \
           make install
	touch $@

clean:
	rm -rf $(ROOT_STAGEDIR) $(TARFILE) $(UNPACK_DIR) stage_stamp
