# This makefile just unpacks and build the GNU Scientific Library,
# then installs the static libraries and headers into the standard
# ASF locations.

include ../../make_support/system_rules

UNPACK_DIR = gsl-1.4

TARBALL = $(UNPACK_DIR).tar.gz

# Copy the static libraries and headers for the GSL into the
# appropriate places in the ASF directory structure.
install_gsl: build_gsl_stamp
	cp ./stage_dir/lib/*.a $(LIBDIR)
	cp -r ./stage_dir/include/gsl $(ASF_INCLUDE_DIR)

# Build the GSL from its tarball and install it in a staging directory.
build_gsl: $(TARBALL)
	tar xzvf $(TARBALL)
	cd $(UNPACK_DIR) ; ./configure --prefix=`pwd`/../stage_dir ; \
			   make ; \
			   make check ; \
                           make install
	touch build_gsl_stamp

clean:
	rm -rf $(UNPACK_DIR) stage_dir build_gsl_stamp
