Alaska Satellite Facility (ASF)
Software Read Me file-- Source Installation, extra help.
#########################################################################
QUICK START:

To compile and install the tools, make sure you're in the "asf_tools"
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
EXAMPLE 2:  Install ASF tools in the "asf_tools" folder of your home
            directory:

    ./configure --prefix=~/asf_tools
    make
    make install

#########################################################################
INTRODUCTION:

If you've gotten far enough to read this note, congratulations! You've
uncompressed and untarred the software correctly. This note will tell
you how to compile and install this ASF-TSO software you just untarred.

WHAT YOU'VE DOWNLOADED:

This file is in a directory called asf_tools, which was created when you
untarred the software. Also present in this asf_tools directory are several
sub-directories (depending on what you've downloaded, not all of these may be
present):


asf_tools+----README_src.txt     This file.
         |  
         +----configure          A script which determines your
         |                       machine type and creates the Makefile.
         |
         +----bin                These are the actual executables.
         |                       (or at least, this is where they
         |                       will go once you build them, when
         |                       you do a "make".
         |
         +----share+
         |         |
         |         +--asf_tools  Contains additional files needed by
         |                       the tools (projection parameter
	 |       		 files, etc).  Like the bin directory,
         |                       it won't be populated until you
         |                       actually build the tools.
         |
         \----src-+---<tools>    This is the source code for the programs.
                  |
                  +---<more tools>
                  |
                  \---...

#########################################################################
INSTALLATION:

1. CONFIGURE THE SOFTWARE:

Since you did not download pre-compiled executable binaries, you have
to compile the software.  To do so, you need to do three things--
first,  create the appropriate makefile for your system; then run 
that makefile; then put the results into the desired location.

The makefile is created by a configuration script called "configure". 
This script determines several details of your machine type (endian-ness, 
system type, which of the libraries the tools require are already on
your system, etc.)  After these details are determined, the script will
create a Makefile tailored to your system.

   ./configure --prefix=<<installation directory>>
   
   (as is always the case in UNIX, case matters (capitals vs. lowercase))

In place of "<<installation directory>>", put the directory in which
you want to install the tools.  (But, don't put the "/bin" or "/share"
part, those are added automatically.)

The default is /usr/local, if you are satisfied with that, you can
leave off the "--prefix=<<installation directory>>" completely, and
just type "./configure".

For example, to install the tools in "/opt/asf_tools", you would type:

   ./configure --prefix=/opt/asf_tools

In this example, after you've finished step (3) below, all of the
ASF tool binaries would be placed in /opt/asf_tools/bin.

Once "configure" completes, you should see something like this:

########### Automatic configuration complete ###########

The ASF tools Makefile is now fully prepared.

To build the tools, type 'make'.
To install the tools, type 'make install'.

The tools will be installed in this location:
  /usr/local

########### Build system is now configured  ############

You should see whatever you put in as the installation directory
above.

2.  COMPILE THE SOFTWARE

To compile, type "make":

    make

After typing make and the name of your program, the make file will compile any
needed libraries and then the program.  It will inform you of every
tiny step in this process, but you can safely ignore everthing that
scrolls by.  However, when the  compilation completes, the last thing
it prints should be:

  XXXXXXXXXXXX All ASF Tools Compiled Successfully! XXXXXXXXXXX

If you don't get this message, then something went wrong, and unless feel like
rewriting our C code (which, by the way, we really don't mind you doing, if
you're so inclined) then contact us! (uso@asf.alaska.edu). We'll try to figure
out what's wrong and get it fixed as soon as possible.

If you did get the success message, then the make file has created the
necessary libraries (which went in  asf_tools/lib/), compiled the
relevant source (from asf_tools/src/<program>), and put the resulting
binary executable into asf_tools/bin/.  If it did so, then you're
ready to proceed to the next step.

3. INSTALL THE SOFTWARE

The binaries have been built into "bin".  To install them into the
location you specified when you ran "configure", you type:

    make install

For this, you do need to have the required permissions to put programs
into the directory you selected.  For "/usr/local", this often means
you must be root.

If you don't have root access, you can install the programs somewhere
under your home directory, where you will have the needed permissions.
For example:

    ./configure --prefix="~/asf_tools"
    make
    make install

After installation, the binaries will be in "<instdir>/bin" and
the shared files in "<instdir>/share/asf_tools".

4. ADD THE BINARIES TO YOUR PATH:

Now that you have binaries for your system in the desired directory,
you have to add this directory to your PATH environment variable.  If
you installed in a standard location such as "/usr/local" then you
can probably skip this step, since it will already be in your path.

Exactly how you do this  depends on where you installed the software
and what UNIX  shell (ksh or csh) you're using.  For example, if
you're running  Solaris and installed the software in your home
directory  called '/user1/yourName/asf_tools', you need to:

(ksh-- add these lines to your ~/.profile file)
	PATH=/user1/yourName/asf_tools/bin/solaris:$PATH
	export PATH
      
(csh-- add this line to your ~/.cshrc file) 
	setenv PATH /user1/yourName/asf_tools/bin/solaris:$PATH

(confused? Contact uso, uso@asf.alaska.edu)

##############################################################################
OTHER HELP:

For help running the software, getting test data, or downloading more software,
you have several resources. First, our web page at http://www.asf.alaska.edu/
is large (and growing)-- it has all the ASF software we offer, tutorials,
further contacts, and more. Second, USO is available via e-mail at
uso@asf.alaska.edu. Finally, there are help pages for each program.
 
(confused? Contact TSO, tso@asf.alaska.edu)
