# This makefile ensures that a copy of the tcl/tk $(MAIN_VERSION) libraries are 
# installed in the ASF tool tree.  It does this by first looking
# around the system for an already built copy of tcl/tk which it can
# just copy, if not, then we've got to unpack tcl/tk and build it.

include ../../make_support/system_rules

# Where we temporarily stage tcl/tk on the way to installing it in
# the ASF tree.
ROOT_STAGEDIR := stage_dir
LIB_STAGEDIR := $(ROOT_STAGEDIR)/lib
INCLUDE_STAGEDIR := $(ROOT_STAGEDIR)/include


# Root of trees under which tcl and tk are already installed on the system,
# or the empty string if it doesn't seem to be installed in a way we
# can easily recognize.  We only try to use tk if we find the
# library and the include files under a common parent, in leaf
# directories with names 'lib' and 'include' respectively.  If for
# example the library is in /usr/local/lib, and the headers in
# /usr/local/include, this variable will be /usr/local.
EXISTING_TCL_ROOT := $(strip $(shell ./find_tcl.sh))
EXISTING_TK_ROOT := $(strip $(shell ./find_tk.sh))

MAIN_VERSION = 8.1
FULL_VERSION = $(MAIN_VERSION).1

# If we find we have to build tcl/tk, these variables specify the
# tarballs and what they unpack to.
TCL_UNPACK_DIR := tcl$(FULL_VERSION)
TCL_TARFILE := $(TCL_UNPACK_DIR).tar
TCL_ZIPFILE := $(TCL_TARFILE).gz
TK_UNPACK_DIR := tk$(FULL_VERSION)
TK_TARFILE := $(TK_UNPACK_DIR).tar
TK_ZIPFILE := $(TK_TARFILE).gz

# Installs the necessary components of the tcl/tk libraries to our tree
install_tcltk: install_tcl install_tk

# Copy the static libraries and headers for tcl into the
# appropriate places in the ASF directory structure.
install_tcl: stage_tcl_stamp
	chmod ug+w $(LIB_STAGEDIR)/libtcl$(MAIN_VERSION).a  $(LIB_STAGEDIR)/tclConfig.sh /$(LIB_STAGEDIR)/tcl$(MAIN_VERSION) ; \
	cp $(LIB_STAGEDIR)/libtcl$(MAIN_VERSION).a $(LIBDIR)
	cp $(LIB_STAGEDIR)/tclConfig.sh $(LIBDIR)
	cp -r $(LIB_STAGEDIR)/tcl$(MAIN_VERSION) $(LIBDIR)
	chmod ug+w $(INCLUDE_STAGEDIR)/tcl.h $(INCLUDE_STAGEDIR)/tclDecls.h ; \
	cp $(INCLUDE_STAGEDIR)/tcl.h $(ASF_INCLUDE_DIR)
	cp $(INCLUDE_STAGEDIR)/tclDecls.h $(ASF_INCLUDE_DIR)

# Copy the static libraries and headers for tk into the
# appropriate places in the ASF directory structure.
install_tk: stage_tk_stamp
	chmod ug+w $(LIB_STAGEDIR)/libtk$(MAIN_VERSION).a  $(LIB_STAGEDIR)/tkConfig.sh /$(LIB_STAGEDIR)/tk$(MAIN_VERSION) ; \
	cp $(LIB_STAGEDIR)/libtk$(MAIN_VERSION).a $(LIBDIR)
	cp $(LIB_STAGEDIR)/tkConfig.sh $(LIBDIR)
	cp -r $(LIB_STAGEDIR)/tk$(MAIN_VERSION) $(LIBDIR)
	chmod ug+w $(INCLUDE_STAGEDIR)/tk.h $(INCLUDE_STAGEDIR)/tkDecls.h ; \
	cp $(INCLUDE_STAGEDIR)/tk.h $(ASF_INCLUDE_DIR)
	cp $(INCLUDE_STAGEDIR)/tkDecls.h $(ASF_INCLUDE_DIR)

# Copy an already built tcl into the staging area, or build it there
# if a copy doesn't appear to be available.
stage_tcl_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	if [ -n "$(EXISTING_TCL_ROOT)" ] ; then \
	    cp $(EXISTING_TCL_ROOT)/lib/libtcl$(MAIN_VERSION).a $(LIB_STAGEDIR) ; \
	    cp $(EXISTING_TCL_ROOT)/lib/tclConfig.sh $(LIB_STAGEDIR) ; \
	    cp -r $(EXISTING_TCL_ROOT)/lib/tcl$(MAIN_VERSION) $(LIB_STAGEDIR) ; \
	    cp $(EXISTING_TCL_ROOT)/include/tcl.h $(INCLUDE_STAGEDIR) ; \
	    cp $(EXISTING_TCL_ROOT)/include/tclDecls.h $(INCLUDE_STAGEDIR) ; \
	else \
	    gunzip -c $(TCL_ZIPFILE) > $(TCL_TARFILE) ; \
	    tar xvf $(TCL_TARFILE) ; \
	    (cd $(TCL_UNPACK_DIR)/unix ; \
                ./configure --enable-gcc --prefix=`pwd`/../../$(ROOT_STAGEDIR) \
		            --disable-load ; \
		make ; \
	        make check ; \
                make install) ; \
        fi
	touch $@

# Copy an already built tk into the staging area, or build it there
# if a copy doesn't appear to be available.
stage_tk_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	if [ -n "$(EXISTING_TK_ROOT)" ] ; then \
	    cp $(EXISTING_TK_ROOT)/lib/libtk$(MAIN_VERSION).a $(LIB_STAGEDIR) ; \
	    cp $(EXISTING_TK_ROOT)/lib/tkConfig.sh $(LIB_STAGEDIR) ; \
	    cp -r $(EXISTING_TK_ROOT)/lib/tk$(MAIN_VERSION) $(LIB_STAGEDIR) ; \
	    cp $(EXISTING_TK_ROOT)/include/tk.h $(INCLUDE_STAGEDIR) ; \
	    cp $(EXISTING_TK_ROOT)/include/tkDecls.h $(INCLUDE_STAGEDIR) ; \
	else \
	    gunzip -c $(TK_ZIPFILE) > $(TK_TARFILE) ; \
	    tar xvf $(TK_TARFILE) ; \
	    (cd $(TK_UNPACK_DIR)/unix ; \
                ./configure --enable-gcc --prefix=`pwd`/../../$(ROOT_STAGEDIR) \
		            --disable-load ; \
		make ; \
	        make check ; \
                make install) ; \
        fi
	touch $@


# Clean up our build directory
clean: clean_tcl clean_tk
	rm -rf $(ROOT_STAGEDIR)

clean_tcl:
	rm -rf  $(TCL_TARFILE) $(TCL_UNPACK_DIR) stage_tcl_stamp


clean_tk:
	rm -rf  $(TK_TARFILE) $(TK_UNPACK_DIR) stage_tk_stamp


# Uninstall the tcl/tk libraries
uninstall: uninstall_tcl uninstall_tk

uninstall_tcl: clean_tcl
	rm -rf $(LIBDIR)/libtcl$(MAIN_VERSION).a $(LIBDIR)/tclConfig.sh \
	    $(LIBDIR)/tcl$(MAIN_VERSION) $(ASF_INCLUDE_DIR)/tcl.h \
	    $(ASF_INCLUDE_DIR)/tclDecls.h

uninstall_tk: clean_tk
	rm -rf $(LIBDIR)/libtk$(MAIN_VERSION).a $(LIBDIR)/tkConfig.sh \
	    $(LIBDIR)/tk$(MAIN_VERSION) $(ASF_INCLUDE_DIR)/tk.h \
	    $(ASF_INCLUDE_DIR)/tkDecls.h
