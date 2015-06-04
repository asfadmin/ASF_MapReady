ASF Software Read Me file for (Linux/Unix) binary installations.

####################################################################
INTRODUCTION:

If you've gotten far enough to read this note, congratulations! You've
uncompressed and untarred the software correctly. This note will tell you how
to install this ASF software you just untarred.


WHAT YOU'VE DOWNLOADED:

This file is in a directory called asf_tools, which was created when you
untarred the software. Also present in this asf_tools directory are several
sub-directories (depending on what you've downloaded, not all of these may be
present):


asf_tools+----COPYING          Description of the license that goes with the
         |                     software.
         |
         +----README_bin.txt   How to install the binary executables and
         |                     quick-start on documentation (if you downloaded
         |                     executables for your platform)
         |
         |
         +----README_rpm.txt   How to validate and install the rpm (if you
         |                     downloaded it)
         |
         |
         +----README_src.txt   How to build and install the software (if you
         |                     downloaded source code)
         |
         |
         +----bin              This directory contains the executables.
         |
         |
         +----man              If his directory is present it contains manual
         |                     pages for our older executables.
         |
         +----share            This directory contains information to assist
                               the executables and in some cases the user.


##############################################################################
INSTALLATION:

1. ADD THE BINARIES TO YOUR PATH:

Since when you untarred our software it installed the binaries in the 
<your_directory>/asf_tools/bin directory, you have to add this directory to
your PATH environment variable. Exactly how you do this depends on where you 
installed the software and which UNIX shell you are using. For example, if you 
installed the software in your home directory called
'/user1/yourName', you need to:

For sh, ksh, bash, or the like, add these lines to your ~/.profile or ~/.bashrc
file:
     PATH=/user1/yourName/asf_tools/bin:$PATH
     export PATH

For csh, tcsh, or the like, add this line to you ~/.cshrc file:
     setenv PATH /user1/yourName/asf_tools/bin:$PATH

Once you have added the PATH information to a start up file as described,
exit the shell and log back in (or open a new terminal window, etc)


##############################################################################
DOCUMENTATION:

The MapReady manual is in the 'doc' subdirectory.  It is a .PDF file, and
can be read with any pdf viewer.  It is 'mapready_manual.pdf'.

All of the command-line tools feature built-in documentation.  Just use
the tool name, followed by the '-help' flag, for example:

     asf_import -help

The command-line tools are all described briefly in the MapReady manual,
the complete documentation for each tool is available with -help.
##############################################################################
OTHER HELP:

1. Our web page at http://www.asf.alaska.edu has all the ASF software we offer, 
tutorials, further contacts, and more.

2. There are also extensive tutorials available at the ASF website,
describing how to perform some common use cases involving MapReady and
other ASF Tools.

3. In the <your_directory>/asf_tools/doc directory, you will find manuals for
the tools that you have installed, i.e. Convert2Vector, MapReady, the SAR
Training Processor, etcetera. 

