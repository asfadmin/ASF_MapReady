# This makefile ensures that a copy of GLIB installed in the ASF
# tool tree.  It does this by first looking around the system
# for an already built copy of GLIB which it can just copy, if
# it doesn't find one, then it unpacks GLIB and builds it.  The
# point of this is to save time on initial builds, since it seems
# that we often end up doing fresh cvs checkouts of the whole
# unbuilt tree for one reason or another.

include ../../make_support/system_rules

# Where we temporarily stage GLIB on the way to installing it in
# the ASF tree.
ROOT_STAGEDIR := stage_dir
LIB_STAGEDIR := $(ROOT_STAGEDIR)/lib
INCLUDE_STAGEDIR := $(ROOT_STAGEDIR)/include

# Root of tree under which the GSL is already installed on the system,
# or the empty string if it doesn't seem to be installed in a way we
# can easily recognize.  We only try to use GLIB if we find the
# library and the include files under a common parent, in leaf
# directories with names 'lib' and 'include' respectively.  If for
# example the library is in /usr/local/lib, and the headers in
# /usr/local/include, this variable will be /usr/local.
EXISTING_GLIB_ROOT := $(strip $(shell ./find_glib.sh))

# If we find we have to build GLIB, these variables specify the
# tarball and what it unpacks into.
UNPACK_DIR := glib-2.4.7
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

# Copy the static libraries and headers for GLIB into the
# appropriate places in the ASF directory structure.
install_glib: stage_glib_stamp
	cp $(LIB_STAGEDIR)/*.a $(LIBDIR)
	cp -r $(INCLUDE_STAGEDIR)/glib-2.0/* $(ASF_INCLUDE_DIR)

ifeq ($(SYS),win32)

stage_glib_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	for f in *.zip; do (yes|unzip $$f -d glib); done;
	cp glib/lib/libglib-2.0.dll.a $(LIB_STAGEDIR);
	cp -r glib/include/glib-2.0 $(INCLUDE_STAGEDIR);
	cp glib/lib/glib-2.0/include/*.h $(INCLUDE_STAGEDIR)/glib-2.0;

else

# Copy an already build GLIB into the staging area, or build it there
# if a copy doesn't appear to be available.
stage_glib_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	if [ -n "$(EXISTING_GLIB_ROOT)" ] ; then \
	    cp $(EXISTING_GLIB_ROOT)/lib/libglib-2.0.a \
               $(LIB_STAGEDIR) ; \
            cp -r $(EXISTING_GLIB_ROOT)/include/glib-2.0 $(INCLUDE_STAGEDIR); \
	else \
	    gunzip --to-stdout $(ZIPFILE) >$(TARFILE) ; \
	    tar xvf $(TARFILE) ; \
	    (cd $(UNPACK_DIR) ; \
                ./configure --enable-static=yes --enable-shared=no \
		   --prefix=`pwd`/../$(ROOT_STAGEDIR) ; \
		make ; \
	        make check ; \
                make install) ; \
        fi
	touch $@

endif

clean:
	rm -rf $(ROOT_STAGEDIR) $(TARFILE) $(UNPACK_DIR) stage_glib_stamp \
	   $(ASF_INCLUDE_DIR)/glib-2.0 $(LIBDIR)/libglib-2.0.a
