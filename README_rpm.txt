ASF Software Read Me for rpm installations
------------------------------------------

This package contains a .rpm file for installing the ASF Tools
on Linux systems that use the Red Hat package management
system.

(1) INSTALL THE SOFTWARE

To install this RPM, you need to have root access.  If you don't,
you'll need to persurade your system administrator to install the
package for you.  If that isn't feasible or you don't want to,
you can instead download the Source package, and compile the tools
yourself, where you can install them in your own home directory
without requiring root privileges.

To install the package:  (Do this as root)

  rpm -i asf_convert-X.X.X-X.i386.rpm

(Of course you need to replace the X's with whatever version you
downloaded, for example: "rpm -i asf_convert-2.1.5-1.i386.rpm")

After the package is installed, you can find out where it was
put by using this rpm command, which does not require root
access:

  rpm -ql asf_convert | grep asf_import

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
