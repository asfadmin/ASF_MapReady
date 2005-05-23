# This makefile ensures that a copy of the Fastest Fourier Transform
# in the West library is installed in the ASF tool tree.

include ../../make_support/system_rules

# Where we temporarily stage the library on the way to installing it
# in the ASF tree.
ROOT_STAGEDIR := stage_dir
LIB_STAGEDIR := $(ROOT_STAGEDIR)/lib
INCLUDE_STAGEDIR := $(ROOT_STAGEDIR)/include

# Names of the library distrubution direction, tar file, and gzipped
# tar file, respectively.
UNPACK_DIR := fftw-3.0.1
TARFILE := $(UNPACK_DIR).tar
ZIPFILE := $(TARFILE).gz

# Copy the static libraries and headers for the library into the
# appropriate places in the ASF directory structure.
install: stage_stamp
	cp $(LIB_STAGEDIR)/*.a $(LIBDIR)
	cp -r $(INCLUDE_STAGEDIR)/*.h $(ASF_INCLUDE_DIR)

# Build the library in the staging area.
stage_stamp:
	mkdir -p $(LIB_STAGEDIR) $(INCLUDE_STAGEDIR)
	gunzip --to-stdout $(ZIPFILE) >$(TARFILE)
	tar xvf $(TARFILE)
	(cd $(UNPACK_DIR) ; \
	    ./configure --prefix=`pwd`/../$(ROOT_STAGEDIR) ; \
	    make ; \
	    make check ; \
	    make install)
	touch $@

clean:
	rm -rf $(ROOT_STAGEDIR) $(TARFILE) $(UNPACK_DIR) stage_stamp
