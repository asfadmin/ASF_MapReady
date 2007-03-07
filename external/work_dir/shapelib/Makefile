# Build and install a copy of the library into the standard location of
# the ASF tool tree.

# Where we stage the library so the main build system can nab it
ROOT_DIR := $(shell cd ../.. ; pwd)
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include

# These variables specify the tarball and what it unpacks into.
UNPACK_DIR := shapelib-1.2.10
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

build: build_stamp

# Create the library & necessary components in the proper place for retrieval
build_stamp:
	mkdir -p $(LIB_DIR) $(INCLUDE_DIR)
	gunzip --to-stdout $(ZIPFILE) > $(TARFILE)
	tar xvf $(TARFILE)
	rm -f $(TARFILE)
	(   cd $(UNPACK_DIR) ; \
	    cat Makefile ../Makefile.addendum > Makefile.2 ; \
	    mv Makefile.2 Makefile ; \
	    $(MAKE) libshp.a ; \
	    cp libshp.a $(LIB_DIR) ; \
	    cp shapefil.h $(INCLUDE_DIR) ; \
	)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_stamp
