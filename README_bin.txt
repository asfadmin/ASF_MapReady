ASF STEP Software Read Me file-- Binary Installation, extra help.
####################################################################
INTRODUCTION:

    If you've gotten far enough to read this note, congratulations!
You've uncompressed and untarred the software correctly.  This note will
tell you how to install this ASF-STEP software you just untarred.

WHAT YOU'VE DOWNLOADED:

    This file is in a directory called asf_tools, which was
created when you untarred the software.  Also present in this
asf_tools directory are several sub-directories (depending
on what you've downloaded, not all of these may be present):


asf_tools+----README_bin              This file.
         |  
         |
         |
         +----bin+---irix        These are the actual executables.
         |       |
         |       +---solaris
         |       |
         |       \---sunos
         |         
         |
         +----html+--index.html  This is HTML-formatted documentation
         |                       for each tool.
         |
         |
         |
         \----man+---man1        These are the ASF Tools' man pages.
                 |
                 \---cat1        This is text documentation.

#################################################################
INSTALLATION:

1. ADD THE BINARIES TO YOUR PATH:

   Since when you untarred our software it installed the binaries 
in the <your directory>/asf_tools/bin/<your system> directory, you 
have to add this directory to your PATH environment variable.  Just 
like adding the man pages, below, exactly how you do this 
depends on where you installed the software and what UNIX 
shell (ksh or csh) you're using.  For example, if you're running 
Solaris and installed the software in your home directory 
called '/user1/yourName', you need to:

(ksh-- add these lines to your ~/.profile file)
	PATH=/user1/yourName/asf_tools/bin/solaris:$PATH
	export PATH
      
(csh-- add this line to your ~/.cshrc file) 
	setenv PATH /user1/yourName/asf_tools/bin/solaris:$PATH

(confused? Contact APD below)


##############################################################################
OTHER HELP:

   For help running the software, getting test data, or downloading
more software, you have several resources.  First, our web page at
http://www.images.alaska.edu is large (and growing)-- it has all
the ASF software we offer, tutorials, further contacts, and more.
Second, the APD Lab is available via e-mail at apd@asf.alaska.edu

Third, there are additional README files in aisp which describes this
package in more detail.  Fourth, if you downloaded dump_multi_volume,
there are a few additional installation steps you need for this program.
Read all about it in the comments in the actual dump_multi_volume script.
Finally, there are manual pages for each program.  To acess the manual 
(man) pages, you need to do a few more steps:

HOW TO ACCESS MANUAL PAGES:

  To access installed man pages, you need to add the asf_tools/man 
directory to your MANPATH environment variable.  For example, if 
you installed the ASF-STEP Tools in your home directory, the 
MANPATH variable should have <your home directory>/asf_tools/man added to it. The 
exact way you do this depends on which UNIX shell (ksh or csh)
you're using:

(ksh-- add these lines to your ~/.profile file)
	MANPATH=/user1/yourName/asf_tools/man:$MANPATH
	export MANPATH
      
(csh-- add this line to your ~/.cshrc file) 
	setenv MANPATH /user1/yourName/asf_tools/man:$MANPATH

(Some systems may not have a MANPATH environment variable defined.
In this case you can set your own by replacing the $MANPATH above
with the rest of the man directories on your system as listed in
the man manual page. For example, an IRIX .cshrc file could contain:
        setenv MANPATH /user1/yourName/asf_tools/man:/usr/share/catman:/usr/share/man:/usr/catman:/usr/man
)

   If the above worked, you should be able to type
        man <tool name>
and get a few pages of information about each tool you've downloaded.

(confused? Contact APD, above)

