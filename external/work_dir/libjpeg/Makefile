# Build and install a copy of the library into the standard location of
# the ASF tool tree.

# Where we stage the library so the main build system can nab it
ROOT_DIR := $(shell cd ../.. ; pwd)
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include

# These variables specify the jpeg tarball and what it unpacks into.
UNPACK_DIR := jpeg-6b

# On Windows, use patched version to match binary version bundled
# with GTK.  Patch is the lossless compression patch (checked in)
ifeq ($(SYS),win32)
ZIPFILE := jpegsrc.v6b.patched.tar.gz
else
ZIPFILE := jpegsrc.v6b.tar.gz
endif

build: build_stamp

# Create the library & necessary components in the proper place for retrieval
build_stamp:
	mkdir -p $(LIB_DIR) $(INCLUDE_DIR)
	gunzip --to-stdout $(ZIPFILE) |	tar xv
	(   cd $(UNPACK_DIR) ; \
	    ./configure --prefix=$(ROOT_DIR) ; \
	    make ; \
	    make install-lib ; \
	)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_stamp
