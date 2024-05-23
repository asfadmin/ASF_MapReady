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

* sudo apt-get install git bison flex g++ gcc libgdal-dev libgtk2.0-dev libglade2-dev libgsl-dev libproj-dev libtiff-dev libgeotiff-dev shapelib libshp-dev libcunit1-dev
* git clone git@github.com/asfadmin/ASF_MapReady.git ASF_MapReady
* cd ASF_MapReady
* ./configure
* make

Optional:
* sudo make install

As noted above, if you skip the last step, add "/path/to/ASF_MapReady/bin" to your
PATH in ~/.bashrc.

#########################################################################  

### [MapReady In Docker](https://github.com/asfadmin/MapReadyDocker)

Independent project to build ASF MapReady in a containerized modern OS ([Redhat 8.10 Universal Base Image](https://github.com/asfadmin/MapReadyDocker)). Project offers a [public image](https://github.com/asfadmin/MapReadyDocker/pkgs/container/mapready) and the source [Dockerfile](https://github.com/asfadmin/MapReadyDocker/blob/main/build/asf_mapready.Dockerfile). If trying to build on another OS, see the [specific-dependencies](https://github.com/asfadmin/MapReadyDocker/blob/main/README.md#specific-dependencies) breakdown of workable dependency versions. 

`MapReady in Docker` **does not** offer any new functionality or bug fixes, it is only an effort to enable ongoing workflow development/maintenance on a newer OS. 

#### Usage

```
# Pull public image from GitHub Container Registry
docker run -it ghcr.io/asfadmin/mapready:v1.1.0

# Add /usr/local/lib/ to LD_LIBRARY_PATH
[root@43b365c2b259 /]# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/

# Do MapReady Stuff
[root@43b365c2b259 /]# /usr/local/bin/asf_import 

Usage:
   asf_import [-amplitude | -sigma | -gamma | -beta | -power] [-db]
              [-format <inputFormat>] [-ancillary-file <file>]
              [-colormap <colormap_file>] [-band <band_id | all>]
              [-no-ers2-gain-fix] [-image-data-type <type>] [-lut <file>]
              [-lat <lower> <upper>] [-prc] [-log <logFile>]
              [-quiet] [-real-quiet] [-license] [-version] [-multilook]
              [-azimuth-look-count <looks>] [-range-look-count <looks>]
              [-azimuth-scale[=<scale>] | -fix-meta-ypix[=<pixsiz>]]
              [-range-scale[=<scale>] [-complex] [-metadata <file>]
              [-interferogam <file>] [-coherence <file>] [-slave <file>]
              [-baseline <file>] [-cpx_gamma <file>] [-line <start line subset>]
              [-sample <start sample subset>] [-width <subset width>]
              [-height <subset height>] [-uavsar <type>]
              [-subset <latUL> <lonUL> <latLR> <lonLR>] [-help]
              <inBaseName> <outBaseName>
```


