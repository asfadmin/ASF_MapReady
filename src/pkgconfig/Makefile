# This make file controls the build and install of the pkg-config
# utility which glib and friends need.

include ../../make_support/system_rules

# Where we temporarily stage the GSL on the way to installing it in
# the ASF tree.
ROOT_STAGEDIR := stage_dir
BIN_STAGEDIR := $(ROOT_STAGEDIR)/bin
SHARE_STAGEDIR := $(ROOT_STAGEDIR)/share

# If we find we have to build the GSL, these variables specify the
# tarball and what it unpacks into.
UNPACK_DIR := pkgconfig-0.15.0
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

# Install from the staging area to the ASF binary location.
install_pkgconfig: stage_pkgconfig_stamp
	cp $(BIN_STAGEDIR)/pkg-config $(BINDIR)

# Copy an already build GSL into the staging area, or build it there
# if a copy doesn't appear to be available.
stage_pkgconfig_stamp:
	mkdir $(ROOT_STAGEDIR)
	gunzip --to-stdout $(ZIPFILE) >$(TARFILE)
	tar xvf $(TARFILE)
	(cd $(UNPACK_DIR) ; \
	    ./configure --prefix=`pwd`/../$(ROOT_STAGEDIR) ; \
            make ; \
            make check ; \
            make install) ; \
        touch $@

clean:
	rm -rf $(ROOT_STAGEDIR) $(TARFILE) $(UNPACK_DIR) stage_pkgconfig_stamp
