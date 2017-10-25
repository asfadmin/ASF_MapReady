To compile and install the tools, make sure you are in the "asf_tools"
directory (the same directory this file is in), then:

    ./configure --prefix=<<installation location>>
    make
    make install

You need permissions to put files into <<installation location>>!  The
default is /usr/local.

-------------------------------------------------------------------------
EXAMPLE 1:  Install ASF tools into /usr/local/bin (and shared files in
            /usr/local/share/asf_tools) :

    ./configure
    make
    make install

-------------------------------------------------------------------------
EXAMPLE 2:  Install ASF tools to a folder "local" in your home directory:

    ./configure --prefix=~/local
    make
    make install

#########################################################################
Prerequisites:

The ASF Tools depend on a number of libraries that you'll need to have
installed on your system. In the list below, some of the listed packages
will have additional dependencies that your package management system
should automatically pull in.

On Debian 9/Ubuntu 16.04, the following packages are necessary:

* gcc
* g++
* bison
* flex
* libcunit1-dev
* libexif-dev
* libfftw3-dev
* libgdal-dev
* libgeotiff-dev
* libglade2-dev
* libglib2.0-dev
* libgsl-dev
* libgtk2.0-dev
* libjpeg-dev
* libpng-dev
* libproj-dev
* libshp-dev
* libtiff5-dev
* libxml2-dev

On Fedora/CentOS, the following packages are necessary:

* gcc
* gcc-c++
* bison
* flex
* CUnit-devel
* fftw-devel
* gdal-devel
* gsl-devel
* gtk2-devel
* libcurl-devel
* libgeotiff-devel
* libglade2-devel
* libjpeg-turbo-devel
* libtiff-devel
* netcdf-devel
* proj-devel
* shapelib-devel

On other systems, the package names may vary.
  
#########################################################################  
The following commands on the most recent Ubuntu should get you up
and running:

* sudo apt-get install git bison flex g++ gcc libgdal-dev libgtk2.0-dev libglade2-dev
* git clone git@github.com:asfadmin/ASF_MapReady.git ASF_MapReady
* cd ASF_MapReady
* ./configure
* make

Optional:
* sudo make install

As noted above, if you skip the last step, add "/path/to/ASF_MapReady/bin" to your
PATH in ~/.bashrc.
