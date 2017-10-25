#########################################################################
QUICK START:

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
INTRODUCTION:

If you've gotten far enough to read this note, congratulations! You've
unzipped the software correctly. This note will tell you how to compile
and install this ASF software.

WHAT YOU'VE DOWNLOADED:

This file is in a directory called asf_tools, which was created when you
unzipped the software. Also present in this asf_tools directory are
several sub-directories. Depending on what has been downloaded, not all
of the following may be present. Items below starting with a '*' are
generated at build time.

asf_tools+---*bin/               These are the actual executables
         |                       (or at least, this is where they
         |                       will go once you build them, when
         |                       you do a "make")
         |
         +----configure          A script that determines your machine
         |                       type and creates the Makefile
         |
         +----doc/               Help documentation for the tools is here
         |
         +----external/          Libraries needed but not produced by ASF
         |                       are stored and built here if they are
         |                       not present on your system. Once they
         |                       are compiled the necessary components
         |                       are moved to the asf_tools/lib and
         |                       asf_tools/include directories
         |
         +----include/           Location of many headers used by
         |                       the source code
         |
         +---*lib/               This is where the libraries used to
         |                       build the binaries (executables) will go
         |
         +----COPYING            The user license we ship with our tools
         |
         +----Makefile.in        File used by the configure script to
         |                       produce the makefile that will build the
         |                       tools on your system
         |
         +---*Makefile           File used to build the tools when 'make'
         |                       or 'make install' is given at the
         |                       command line
         |
         +----make_support/      Files used during the build process
         |
         +----README_src.txt     This file
         |
         +---*share-+
         |          |
         |          \-asf_tools/  Contains additional files needed by
         |                       the tools (projection parameter
         |                       files, etc). Like the bin directory,
         |                       it won't be populated until you
         |                       actually build the tools
         |
         \----src-+---<tools>    This is the source code for the programs
                  |
                  +---<more tools>
                  |
                  \---...


#########################################################################
INSTALLATION:


1. CONFIGURE THE SOFTWARE:

Since you did not download pre-compiled executable binaries, you have
to compile the software. To do so, you need to do three things--
first, create the appropriate makefile for your system; then run
that makefile; then put the results into the desired location.

The makefile is created by a configuration script called "configure".
This script determines several details of your machine type (endian-ness,
system type, which of the libraries the tools require are already on
your system, etc.). After these details are determined, the script will
create a Makefile tailored to your system.

   ./configure --prefix=<<installation directory>>

   (as is always the case in UNIX, case matters (capitals vs. lowercase))

In place of "<<installation directory>>", put the directory in which
you want to install the tools.  (But, don't put the "/bin" or "/share"
part, those are added automatically.)

The default is /usr/local, if you are satisfied with that, you can
leave off the "--prefix=<<installation directory>>" completely, and
just type "./configure". If you do plan to install the tools in
/usr/local, don't forget that you will likely need to have root access!

For a configure command line example, to install the tools in
"/opt/asf_tools", you would type:

   ./configure --prefix=/opt/asf_tools

In this example, after step 3 (below) is finished, all of the
ASF tool binaries would be placed in /opt/asf_tools/bin.

Once "configure" completes, you should see something like this:

########### Automatic configuration complete ###########

The ASF tools Makefile is now fully prepared.

2.  COMPILE THE SOFTWARE

To compile, type "make":

    make

After typing make the tools will begin building. The process will inform
you of every tiny step in this process, but you can safely ignore
everything that scrolls by. However, when the compilation completes, the
last thing it prints should be:

  XXXXXXXXXXXX All ASF Tools Compiled Successfully! XXXXXXXXXXX

If you don't get this message, then something went wrong, and unless you
feel like rewriting our C code (which we enthusiastically encourage, by
the way) then contact us! Please see below for contact information. We'll
try to figure out what's wrong and get it fixed as soon as possible.

If you did get the success message, then the make file has created the
necessary libraries (which went in asf_tools/lib/), compiled the
relevant source (from asf_tools/src/<program>), and put the resulting
binary executable into asf_tools/bin/. If it did so, then you're ready to
proceed to the next step.


3. INSTALL THE SOFTWARE

The binaries have been built into "asf_tools/bin".  To install them into
the location you specified when you ran "configure", you type:

    make install

For this, you need to have the required permissions to put programs into
the directory you selected. For "/usr/local", this often means you must
be root.

If you don't have root access, you can install the programs somewhere
under your home directory, where you will have the needed permissions.
For example:

    ./configure --prefix="~/local"
    make
    make install

After installation, the binaries will be in "<instdir>/bin" and
the shared files in "<instdir>/share/asf_tools".


4. ADD THE BINARIES TO YOUR PATH:

Now that you have binaries for your system in the desired directory, you
have to add this directory to your PATH environment variable. If you
installed in a standard location such as "/usr/local" then you can
probably skip this step, since it will already be in your path.

Exactly how you do this depends on where you installed the software and
which UNIX shell you're using. For example, if the software was installed
in your home directory called '/home/username/local', do the following:

For sh, ksh, bash, and similar shells add these lines to your ~/.profile
or ~/.bashrc file:
PATH=/home/username/local/bin:$PATH
export PATH

For csh, tcsh, and similar shells add this line to the ~/.cshrc file:
setenv PATH /home/username/local/bin:$PATH

Once those lines have been added to the proper file, open a new terminal
and the tools should be available.


#########################################################################
OTHER HELP & CONTACT INFORMATION:

For help running the software, getting test data, or downloading more
software, you have several resources. Our web page has all the software
we offer, tutorials, further contacts, and more. The User Services Office
(USO) is available via e-mail, phone, fax, and mail. There are also help
pages for each program available via the --help option
(e.g. asf_import --help).

Mailing address:
Alaska Satellite Facility
Geophysical Institute
University of Alaska Fairbanks
P.O. Box 757320
Fairbanks, AK 99775-7320

Phone: (907) 474-6166

Fax: (907) 474-2665

Web site: http://www.asf.alaska.edu

E-mail: uso@asf.alaska.edu
