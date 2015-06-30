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

These are names on Ubuntu systems.  On CentOS, a few of the names are
different.  On other systems, the packages may have different names.

	* bison
	* flex
	* g++
	* gcc
	* libgdal-dev
	* libgtk2.0-dev
  * libglade2-dev
  
#########################################################################  
The following commands on the most recent Ubuntu should get you up
and running:

sudo apt-get install git bison flex g++ gcc libgdal-dev libgtk2.0-dev libglade2-dev
git clone git@github.com:asfadmin/ASF_MapReady.git ASF_MapReady
cd ASF_MapReady
./configure
make

Optional:
sudo make install

As noted above, if you skip the last step, add the "<PATH>/ASF_MapReady/bin" to your
PATH in ~/.bashrc.
