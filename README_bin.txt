ASF Software Read Me file-- Binary Installation.

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
         |                         software
         +----README_bin.txt     This file.
         |
         |
         |
         +----bin+---irix        These directories contain the actual
         |       |                 executables.
         |       \---solaris
         |
         |
         |
         \----man+---man1        These are the ASF Tools' man pages.
                 |
                 \---cat1        This is text documentation.


#################################################################
INSTALLATION:

1. ADD THE BINARIES TO YOUR PATH:

Since when you untarred our software it installed the binaries in the 
<your_directory>/asf_tools/bin/<your_system> directory, you have to add this 
directory to your PATH environment variable. Just like adding the man pages, 
below, exactly how you do this depends on where you installed the software and 
which UNIX shell you are using. For example, if you are running Solaris and 
installed the software in your home directory called '/user1/yourName', you need 
to:

(sh/ksh/bash-- add these lines to your ~/.profile or ~/.bashrc file)
	PATH=/user1/yourName/asf_tools/bin/solaris:$PATH
	export PATH

(csh/tcsh-- add this line to your ~/.cshrc file)
	setenv PATH /user1/yourName/asf_tools/bin/solaris:$PATH


##############################################################################
OTHER HELP:

For help running the software, getting test data, or downloading more software,
you have several resources.  First, our web page at http://www.asf.alaska.edu is
large (and growing)-- it has all the ASF software we offer, tutorials, further
contacts, and more. Second, we are available via e-mail at uso@asf.alaska.edu
Lastly, there are manual pages for each program. To access the manual (man)
pages, you need to do a few more steps:

HOW TO ACCESS MANUAL PAGES:

To access installed man pages, you need to add the asf_tools/man directory to
your MANPATH environment variable. For example, if you installed the ASF Tools
in your home directory, the MANPATH variable should have
<your_home_directory>/asf_tools/man added to it. The exact way you do this
depends on which UNIX shell you're using:

(sh/ksh/bash-- add these lines to your ~/.profile or ~/.bashrc file)
	MANPATH=/user1/yourName/asf_tools/man:$MANPATH
	export MANPATH

(csh/tcsh-- add this line to your ~/.cshrc file)
	setenv MANPATH /user1/yourName/asf_tools/man:$MANPATH

If the above worked, you should be able to type:
        man <tool_name>
and get a few pages of information about each tool you've downloaded.
