ASF Software Read Me file for binary installations.

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


asf_tools+----LICENSE.txt      Description of the license that goes with the
         |                     software.
         |
         +----README_bin.txt   This file.
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


##############################################################################
DOCUMENTATION:

All of our newer tools feature built-in documentation.  Just use the tool name,
followed by the '-help' flag, like for example this:

     asf_import -help


In the case a 'man' directory is present, you will need to add the asf_tools/man 
directory to your MANPATH environment variable. For example, if you installed 
the ASF Tools in your home directory, the MANPATH variable should have 
<your_home_directory>/asf_tools/man added to it. The exact way you do this 
depends on which UNIX shell you're using:

sh/ksh/bash-- add these lines to your ~/.profile or ~/.bashrc file
        MANPATH=/user1/yourName/asf_tools/man:$MANPATH
        export MANPATH

(sh/tcsh-- add this line to your ~/.cshrc file
        setenv MANPATH /user1/yourName/asf_tools/man:$MANPATH

If the above worked, you should be able to type:
        man <tool_name>
and get a few pages of information about each tool you've downloaded.


##############################################################################
OTHER HELP:

Our web page at http://www.asf.alaska.edu has all the ASF software we offer, 
tutorials, further contacts, and more.
