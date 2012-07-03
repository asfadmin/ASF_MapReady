# Build and install a copy of the library into the standard location of
# the ASF tool tree.

# Where we stage the library so the main build system can nab it
ROOT_DIR := $(shell cd ../.. ; pwd)
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include
UNAME := $(shell uname)

# These variables specify the tiff tarball and what it unpacks into.
ifeq ($(UNAME), Darwin)
UNPACK_DIR := tiff-3.7.4
else
UNPACK_DIR := tiff-4.0.1
endif
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
	    ./configure --prefix=$(ROOT_DIR) --disable-jpeg --disable-zlib --disable-shared; \
	    $(MAKE) ; \
	    $(MAKE) install ; \
	)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_stamp
