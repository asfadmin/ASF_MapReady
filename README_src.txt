ASF STEP Software Read Me file-- Source Installation, extra help.
####################################################################
INTRODUCTION:

    If you've gotten far enough to read this note, congratulations!
You've uncompressed and untarred the software correctly.  This note will
tell you how to compile and install this ASF-STEP software you just 
untarred.

WHAT YOU'VE DOWNLOADED:

    This file is in a directory called asf_tools, which was
created when you untarred the software.  Also present in this
asf_tools directory are several sub-directories (depending
on what you've downloaded, not all of these may be present):


asf_tools+----README             This file.
         |  
         +----config             A script which determines your
         |                       machine type and creates the Makefile.
         |
         +----make_support       A directory containing fragments of the
         |                       Makefile, assembled by config.
         |
         |
         +----bin+---irix        These are the actual executables.
         |       |               (or at least, this is where they
         |       +---solaris      will go once you build them.)
         |       |
         |       \---...
         |
         |
         +----lib+---irix        These are the ASF libraries.
         |       |
         |       +---solaris
         |       |
         |       \---...        
         |
         |
         +----html+--index.html  This is HTML-formatted documentation
         |                       for each tool.
         |
         |
         +----man+---man1        These are the ASF Tools' man pages.
         |       |
         |       +---man5
         |       |
         |       \---cat1        
         |
         |
         \----src*+---<tools>    This is the source code for the programs.
                  |
                  +---<more tools>
                  |
                  \---...

#################################################################
INSTALLATION:

1. COMPILE THE SOFTWARE:

   Since you did not download pre-compiled executable binaries, you
have to compile the software.  To do so, you need to do two things-- 
first, create the appropriate makefile for your system; then run 
that makefile.

   First, a note if you're running SunOS: since our software is written
in ANSI C, and the standard SunOS C compiler doesn't support ANSI C,
we use GCC (the GNU C Compiler-- see http://www.fsf.org).  Type "gcc"
to find out if you already have GCC installed on your system.  If you get
"gcc: No input files", then you already have GCC installed on your system,
and you can proceed.  If not (you'll probably get "gcc: Command not found"),
then you have two choices: either download GCC from http://www.fsf.org, 
compile it, and install it on your system; or you can get a prebuilt 
binary copy of gcc for Sparc-based SunOS machines from our website, at 
http://www.images.alaska.edu.  If you use our binary copy,
you'll need to use "make.sunos_gcc" instead of "make.sunos", and 
you'll need to install gcc in your path.  Read "make.sunos_gcc" for details.
If you have the real GCC installed on your system, just use "make.sunos".
Other platforms don't have to deal with these issues-- you should
really consider updating your operating system.

   We've written a configuration script called "config", in the asf_tools
directory, which determines several details of your machine type (endian-ness, 
system type, etc.)  After these details are determined, the config script
will create a Makefile tailored to your system.

   ./config
      <answer questions about your system>
   make
   
   (as is always the case in UNIX, case matters (capitals vs. lowercase))

   After typing make and the name of your program, the make file
will compile any needed libraries and then the program.  It will
inform you of every tiny step in this process, but you can
safely ignore everthing that scrolls by.  However, when the 
compilation completes, the last thing it prints should be:

  XXXXXXXXXXXX  <toolname> Compiled Successfully!    XXXXXXXXXXX


   If you don't get this message, then something went wrong,
and unless feel like rewriting our C code (which, by the way,
we really don't mind you doing, if you're so inclined) then
contact APD, below.  We'll try to figure out what's 
wrong and get it fixed as soon as possible.

   If you did get the sucess message, then the make file has
created the neccessary libraries (which went in 
asf_tools/lib/<system>), compiled the relevant source (from
asf_tools/src/<program>), and put the resulting binary executable
into asf_tools/bin/<system>.  If it did so, then you're
ready to proceed to the next step.


2. ADD THE BINARIES TO YOUR PATH:

   Now that you have binaries for your system in the  
<your directory>/asf_tools/bin/<your system> directory, you 
have to add this directory to your PATH environment variable.  

   When you ran the "config" script above, it asked you if
you wanted it to automatically add the binaries to your path.
If you told it to automatically add them, you don't have to
add them yourself.

Just like adding the man pages, below, exactly how you do this 
depends on where you installed the software and what UNIX 
shell (ksh or csh) you're using.  For example, if you're running 
Solaris and installed the software in your home directory 
called '/user1/yourName', you need to:

(ksh-- add these lines to your ~/.profile file)
	PATH=/user1/yourName/asf_tools/bin/solaris:$PATH
	export PATH
      
(csh-- add this line to your ~/.cshrc file) 
	setenv PATH /user1/yourName/asf_tools/bin/solaris:$PATH

(confused? Contact APD, below)

##############################################################################
OTHER HELP:

   For help running the software, getting test data, or downloading
more software, you have several resources.  First, our web page at
http://www.images.alaska.edu is large (and growing)-- it has all
the ASF software we offer, tutorials, further contacts, and more.
Second, the APD lab is available via e-mail at apd@asf.alaska.edu.
Third, there are additional README files in aisp which describes this
package in more detail.  Fourth, if you downloaded dump_multi_volume,
there are a few additional installation steps you need for this program.
Read all about it in the comments in the actual dump_multi_volume script.
Finally, there are manual pages for each program. To acess the 
manual (man) pages, you need to do a few more steps:
   
HOW TO ACCESS MANUAL PAGES:

  To access installed man pages, you need to add the asf_tools/man 
directory to your MANPATH environment variable.  For example, if 
you installed the ASF-STEP Tools in your home directory, the 
MANPATH variable should have <your home directory>/asf_tools/man added to it. 
The exact way you do this depends on which UNIX shell (ksh or csh)
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
