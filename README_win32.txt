ASF STEP Software Read Me file-- Win32 Installation, extra help.
####################################################################
INTRODUCTION:

    If you've gotten far enough to read this note, congratulations!
You've uncompressed and untarred the software correctly.  This note will
tell you how to install this ASF-STEP software you just unzipped.

WHAT YOU'VE DOWNLOADED:

    This file is in a directory called asf_tools, which was
created when you untarred the software.  Also present in this
asf_tools directory are several sub-directories (depending
on what you've downloaded, not all of these may be present):


asf_tools+----README_bin              This file.
         |  
         |
         |
         +----bin+---win32       These are the actual executables.
         |
         |
         +----html+--index.html  This is HTML-formatted documentation
         |                       for each tool.
         |
         |
         \----man+---man1        These are the ASF Tools' man pages.
                 |
                 \---cat1        This is text-format documentation.

#################################################################
INSTALLATION:

1. ADD THE BINARIES TO YOUR PATH:

   Since when you untarred our software it installed the binaries 
in the <some directory>\asf_tools\bin\win32\ directory, you 
have to add this directory to your PATH environment variable.
To do this, add the following line to the end of your autoexec.bat
file:
SET PATH=%PATH%;c:\<some directory>\asf_tools\bin\win32\

2. Note that Windows support is still quite experimental.
For doing real work, we recommend using a UNIX operating
system, such as Linux or FreeBSD.

3. To use the scripts provided, you'll need a Bourne Shell interpreter.
You can download one as part of Cygnus Solutions' UNIX compatibility
package Cygwin, which you can download at
	http://sourceware.cygnus.com/cygwin/

##############################################################################
OTHER HELP:

   For help running the software, getting test data, or downloading
more software, you have several resources.  First, our web page at
http://www.images.alaska.edu is large (and growing)-- it has all
the ASF software we offer, tutorials, further contacts, and more.
Second, the APD Lab is available via e-mail at apd@asf.alaska.edu
