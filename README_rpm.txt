ASF Software Read Me for rpm installations
------------------------------------------

This package contains a signed .rpm file for installing the
ASF Tools on Linux systems that use the Red Hat package management
system.

(0) VALIDATE THE SOFTWARE

While not necessary in order to install this RPM, you may wish
to validate the signature prior to installation.  Note that when
you extract the files from the archive that you received, that
you should find not only the RPM file and the README file(s),
you should also find the ASCII format exported public key
from ASF, i.e. asf_tools-pubkey.asc.

Prior to validating the software with the provided public key,
the key will need to be imported to the rpm public keys database
(and yes, you need root access for this):

  rpm --import asf_tools-pubkey.asc

Once the public key has been imported, validate the RPM as
follows:

  rpm -K asf_mapready-X.X.X-X.i386.rpm

When you validate the software, it positive results assure you
of the following:

1. The package was generated on a genuine Red Hat system, and

2. The package is unchanged from the original copy on that
   system.

If all is well, you should see a response that includes a
'dsa' (the key) and "OK", i.e. a typical response might
look like this:

  asf_mapready-X.X.X-X.src.rpm: (sha1) dsa sha1 md5 gpg OK

If the response includes the term "NOT OK" or "KEYS MISSING" or
"dsa" (for ASF tools that is) is missing, then it means that
either the public key import (above) didn't work or something
is amuck with the RPM itself.


(1) INSTALL THE SOFTWARE

To install this RPM, you need to have root access.  If you don't,
you'll need to persurade your system administrator to install the
package for you.  If that isn't feasible or you don't want to,
you can instead download the Source package, and compile the tools
yourself, where you can install them in your own home directory
without requiring root privileges.

To install the package:  (Do this as root)

  rpm -i asf_mapready-X.X.X-X.i386.rpm

(Of course you need to replace the X's with whatever version you
downloaded, for example: "rpm -i asf_mapready-2.1.5-1.i386.rpm")

After the package is installed, you can find out where it was
put by using this rpm command, which does not require root
access:

  rpm -ql asf_mapready | grep asf_import

You should see something like the following:

  /usr/local/bin/asf_import

Which tells you that the packages have been installed in /usr/local,
which is the default.

This directory needs to be added to your path, in order to run the
tools.

(2) ADD THE TOOLS BIN DIRECTORY TO YOUR PATH

Exactly how you do this depends on which UNIX shell you are using.
For example, if you installed the software in '/opt/asf_tools', you
need to:

For bash or the like, add these lines to your ~/.profile or ~/.bashrc
file:

     export PATH=/opt/asf_tools/bin:$PATH

For csh or the like, add this line to your ~/.cshrc file:

     setenv PATH /opt/asf_tools/bin:$PATH


If you've gone with the default installation location of /usr/local,
you may already have /usr/local/bin in your path, in which case you
don't need to do anything.
