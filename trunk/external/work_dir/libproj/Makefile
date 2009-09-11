# Build and install a copy of the proj library (a geocoding library)
# into the standard location in the ASF tool tree.

# Where we stage the proj library so the main build system can nab it
ROOT_DIR := $(shell cd ../.. ; pwd)
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include

# On Solaris libproj has a bug (or, probably, triggers a gcc bug)
# if using -O2 or better.  So, force -O1 on Solaris only.
# See: http://bugzilla.remotesensing.org/show_bug.cgi?id=298
ifeq ($(SYS),solaris)
PROJ_CFLAGS := -g -O1
else
PROJ_CFLAGS := -O4
endif

# These variables specify the tarball and what it unpacks into.
UNPACK_DIR := proj-4.4.8
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

# NAD27 grid shifts file
NAD27FILE := proj-nad27-1.1.tar.gz

build: build_stamp

# Create the library & necessary components in the proper place for retrieval
# The perl call is a tweak to fix some prototypes in the API header that
#   don't include 'void'.
build_stamp:
	mkdir -p $(LIB_DIR) $(INCLUDE_DIR)
	gunzip --to-stdout $(ZIPFILE) > $(TARFILE)
	tar xvf $(TARFILE)
	rm -f $(TARFILE)
	(   cp $(NAD27FILE) $(UNPACK_DIR)/nad; \
	    cd $(UNPACK_DIR) ; \
	    perl -p -i -e 's{\(\);}{(void);}' src/proj_api.h ; \
	    cd nad; tar xzvf $(NAD27FILE); cd ..; \
	    CFLAGS="$(PROJ_CFLAGS)" ./configure --prefix=$(ROOT_DIR) ; \
	    $(MAKE) ; \
	    $(MAKE) install ; \
	)
	touch $@

clean:
	rm -rf $(UNPACK_DIR) build_stamp
