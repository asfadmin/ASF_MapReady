# This makefile ensures that a copy of the GNU Scientific Library is
# installed in the ASF tool tree.  It does this by first looking
# around the system for an already built copy of the GSL which it can
# just copy, if it doesn't find one, then it unpacks the GSL and
# builds it.  The point of this is to save time on initial builds,
# since it seems that we often end up doing fresh cvs checkouts of the
# whole unbuilt tree for one reason or another.

include ../../make_support/system_rules

# Where we temporarily stage the GSL on the way to installing it in
# the ASF tree.
ROOT_STAGEDIR := stage_dir
LIB_STAGEDIR := $(ROOT_STAGEDIR)/lib
INCLUDE_STAGEDIR := $(ROOT_STAGEDIR)/include

# Root of tree under which the GSL is already installed on the system,
# or the empty string if it doesn't seem to be installed in a way we
# can easily recognize.  We only try to use the GSL if we find the
# library and the include files under a common parent, in leaf
# directories with names 'lib' and 'include' respectively.  If for
# example the library is in /usr/local/lib, and the headers in
# /usr/local/include, this variable will be /usr/local.
EXISTING_GSL_ROOT := $(strip $(shell ./find_gsl.sh))

# If we find we have to build the GSL, these variables specify the
# tarball and what it unpacks into.
UNPACK_DIR := gsl-1.4
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

# Copy the static libraries and headers for the GSL into the
# appropriate places in the ASF directory structure.
install_gsl: stage_gsl_stamp
	cp $(LIB_STAGEDIR)/*.a $(LIBDIR)
	cp -r $(INCLUDE_STAGEDIR)/gsl $(ASF_INCLUDE_DIR)

# Copy an already build GSL into the staging area, or build it there
# if a copy doesn't appear to be available.
stage_gsl_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	if [ -n "$(EXISTING_GSL_ROOT)" ] ; then \
	    cp $(EXISTING_GSL_ROOT)/lib/libgsl.a \
               $(EXISTING_GSL_ROOT)/lib/libgslcblas.a \
               $(LIB_STAGEDIR) ; \
            cp -r $(EXISTING_GSL_ROOT)/include/gsl $(INCLUDE_STAGEDIR) ; \
	else \
	    gunzip --to-stdout $(ZIPFILE) >$(TARFILE) ; \
	    tar xvf $(TARFILE) ; \
	    (cd $(UNPACK_DIR) ; \
                ./configure --prefix=`pwd`/../$(ROOT_STAGEDIR) ; \
		make ; \
	        make check ; \
                make install) ; \
        fi
	touch $@

clean:
	rm -rf $(ROOT_STAGEDIR) $(TARFILE) $(UNPACK_DIR) stage_gsl_stamp
