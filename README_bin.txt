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


asf_tools+----LICENSE            Description of which license goes with what
         |                       software.
         |
         +----README_bin.txt     This file.
         |
         |
         +----bin+---irix        These directories contain the actual
                 |               executables.
                 |
                 +---linux              
                 |
                 \---solaris
          

##############################################################################
INSTALLATION:

1. ADD THE BINARIES TO YOUR PATH:

Since when you untarred our software it installed the binaries in the
<your_directory>/asf_tools/bin/<your_system> directory, you have to
add this directory to your PATH environment variable.  Exactly how you
do this depends on where you installed the software and which UNIX
shell you are using. For example, if you are running Solaris and
installed the software in your home directory called
'/user1/yourName', you need to:

For bash or the like, add these lines to your ~/.profile or ~/.bashrc file:

     PATH=/user1/yourName/asf_tools/bin/solaris:$PATH
     export PATH

For csh or the like, add this line to you ~/.cshrc file:

     setenv PATH /user1/yourName/asf_tools/bin/solaris:$PATH


##############################################################################
DOCUMENTATION:

The tools feature built-in documentation.  Just use the tool name,
followed by the '-help' flag, like for example this:

     asf_import -help


##############################################################################
OTHER HELP:

Our web page at http://www.asf.alaska.edu is large (and growing) -- it
has all the ASF software we offer, tutorials, further contacts, and
more.


