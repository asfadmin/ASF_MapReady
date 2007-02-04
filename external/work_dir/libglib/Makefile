# Build and install a copy of the library into the standard location
# in the ASF tool tree.

# needed for the $(SYS) var
include ../../../make_support/system_rules

# Where we stage glib so the main build system can nab it
ROOT_DIR := $(shell cd ../.. ; pwd)
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include

# These variables specify the tarball and what it unpacks into.
GLIB_UNPACK_DIR := glib-2.4.7
GLIB_ZIPFILE := $(GLIB_UNPACK_DIR).tar.gz

ICONV_UNPACK_DIR := libiconv-1.9.1
ICONV_ZIPFILE := $(ICONV_UNPACK_DIR).tar.gz

# build the gLib for the main build system to nab
build: build_glib_stamp

ifeq ($(SYS),win32)

build_glib_stamp:
	mkdir -p $(LIB_DIR) $(INCLUDE_DIR)
	rm -rf *.zip
	cp ../../../../support/win32/gtk/glib-*.zip .
	cp ../../../../support/win32/gtk/libiconv-*.zip .	
	for f in *.zip; do (yes|unzip $$f -d glib); done;
	cp glib/lib/libglib-2.0.dll.a $(LIB_DIR)/libglib-2.0.a;
	cp glib/lib/libiconv.a $(LIB_DIR);
	cp -r glib/include/glib-2.0 $(INCLUDE_DIR);
	cp glib/lib/glib-2.0/include/*.h $(INCLUDE_DIR)/glib-2.0;
	touch build_glib_stamp

else

# Create glib & necessary components in the proper place for retrieval
build_glib_stamp: build_iconv_stamp
	gunzip --to-stdout $(GLIB_ZIPFILE) | tar xv
	(   cd $(GLIB_UNPACK_DIR) ; \
	    LDFLAGS="-L$(LIB_DIR)" \
	    CFLAGS="-I$(INCLUDE_DIR)" \
	    ./configure --enable-static=yes --enable-shared=no  \
	        --with-libiconv=gnu --prefix=$(ROOT_DIR) ; \
	    make ; \
	    make install ; \
	)
	touch $@

endif

# Create the iconv library & necessary parts in the proper place for retrieval
build_iconv_stamp:
	mkdir -p $(LIB_DIR) $(INCLUDE_DIR)
	gunzip --to-stdout $(ICONV_ZIPFILE) | tar xv
	(   cd $(ICONV_UNPACK_DIR) ; \
	    ./configure --enable-static=yes --enable-shared=yes \
	        --enable-relocatable --prefix=$(ROOT_DIR) ; \
	    make ; \
	    make install ; \
	)
	touch $@


clean:
	rm -rf $(GLIB_UNPACK_DIR) build_glib_stamp \
	       $(ICONV_UNPACK_DIR) build_iconv_stamp
