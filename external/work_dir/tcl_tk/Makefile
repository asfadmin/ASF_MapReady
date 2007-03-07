# Build and install a copy of the TCL adn TK libraries
# into the standard location in the ASF tool tree.

# Where we stage tcl so the main build system can nab it
ROOT_DIR := $(shell cd ../.. ; pwd)
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include

# These variables specify the tarball and what it unpacks into.
TCL_UNPACK_DIR := tcl8.1.1
TCL_TARFILE := $(TCL_UNPACK_DIR).tar
TCL_ZIPFILE := $(TCL_TARFILE).gz

TK_UNPACK_DIR := tk8.1.1
TK_TARFILE := $(TK_UNPACK_DIR).tar
TK_ZIPFILE := $(TK_TARFILE).gz

# build the TCL for the main build system to nab
build: build_tcl_stamp build_tk_stamp

# Create tcl library & necessary components in the proper place for retrieval
# If zip file not present, skip.
build_tcl_stamp:
	if [ -f $(TCL_ZIPFILE) ]; then \
	  mkdir -p $(LIB_DIR) $(INCLUDE_DIR); \
	  gunzip --to-stdout $(TCL_ZIPFILE) > $(TCL_TARFILE) ; \
	  tar xvf $(TCL_TARFILE) ; \
	  rm -f $(TCL_TARFILE) ; \
	  (   cd $(TCL_UNPACK_DIR)/unix ; \
	      ./configure --enable-gcc --disable-load --prefix=$(ROOT_DIR) ; \
	      $(MAKE) ; \
	      $(MAKE) install ; \
	  ) && touch $@; \
	fi




# Create the tk library & necessary parts in the proper place for retrieval
# If zip file not present, skip.
build_tk_stamp: build_tcl_stamp
	if [ -f $(TK_ZIPFILE) ]; then \
	  mkdir -p $(LIB_DIR) $(INCLUDE_DIR); \
	  gunzip --to-stdout $(TK_ZIPFILE) > $(TK_TARFILE) ; \
	  tar xvf $(TK_TARFILE) ; \
	  rm -f $(TK_TARFILE) ; \
	  (   cd $(TK_UNPACK_DIR)/unix ; \
	      ./configure  --enable-gcc --disable-load --prefix=$(ROOT_DIR) ; \
	      $(MAKE) ; \
	      $(MAKE) install ; \
	  ) && touch $@; \
	fi


clean:
	rm -rf $(TCL_UNPACK_DIR) build_tcl_stamp \
	       $(TK_UNPACK_DIR) build_tk_stamp
