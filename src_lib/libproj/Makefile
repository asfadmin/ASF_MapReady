# Build and install a copy of the proj library (a grocoding library)
# into the standard location in the ASF tool tree.

include ../../make_support/system_rules

# Where we temporarily stage the GSL on the way to installing it in
# the ASF tree.
ROOT_STAGEDIR := stage_dir
LIB_STAGEDIR := $(ROOT_STAGEDIR)/lib
INCLUDE_STAGEDIR := $(ROOT_STAGEDIR)/include

# These variables specify the tarball and what it unpacks into.
UNPACK_DIR := proj-4.4.8
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

# Copy the static libraries and header into the appropriate places in
# the ASF directory structure.
install: stage_stamp
	cp $(LIB_STAGEDIR)/*.a $(LIBDIR)
	cp -r $(INCLUDE_STAGEDIR)/* $(ASF_INCLUDE_DIR)

# Build the library into the staging dir, with a few slight tweaks for
# some prototypes in the API header that don't include 'void'.
stage_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	gunzip --to-stdout $(ZIPFILE) >$(TARFILE); \
	    tar xvf $(TARFILE) ; \
	    (cd $(UNPACK_DIR) ; \
		perl -p -i -e 's{\(\);}{(void);}' src/proj_api.h ; \
                ./configure --prefix=`pwd`/../$(ROOT_STAGEDIR) ; \
		make ; \
	        make check ; \
                make install) ; \
	touch $@

clean:
	rm -rf $(ROOT_STAGEDIR) $(TARFILE) $(UNPACK_DIR) stage_stamp
