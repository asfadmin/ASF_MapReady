All gtk-mac integration documentation can be found at https://live.gnome.org/GTK%2B/OSX
It is best to read that first and all the sections it links to.

Make sure that your system does not have MacPorts, Fink or Homebrew installed. Those package managers have a habit of breaking the build.

Download gtk-osx-setup.sh from the above site

$ sh gtk-osx-build-setup.sh
$ jhbuild bootstrap --skip=libiconv && jhbuild build meta-gtk-osx-bootstrap && jhbuild build meta-gtk-osx-core && jhbuild build libglade && jhbuild build meta-gtk-osx-themes

freetype has been failing to build. When it fails, drop into a shell and make clean && ./configure --prefix=/Users/etrochim/gtk/inst && make && make install then exit and select ignore error.
This did not happen in the build I did 01/13 --KH

Make sure that /usr/bin/pkg-config links to the pkg-config in /Users/etrochim/gtk/inst/bin/pkg-config. This is needed because the Makefiles in mapready are hardcoded to use /usr/bin/pkg-config. That should probably be changed at some point.  If ./configure fails to detect GTK, this is likely the cause.

Now build the mapready sources.

First, libtiff 4.0.1 does not build correctly on Mac OS X, even though it should. A quick work around is to revert back to 3.7.4, at least until they fix their building problems.
Also, during the build, gdal had problems for me. In the Makefile I set all the libraries needed to use the gdal internal version.

$ cd ASF_MapReady
$ ./configure
$ make

Now cd back into your home directory and get the mac bundler. This tools is what creates the .app bundles for our tools and pulls in all the shared libraries that it uses.

$ git clone git://git.gnome.org/gtk-mac-bundler
$ cd gtk-mac-bundler
$ make install

*** These are notes on creating a new bundle file. Ignore them if you are just going to use an existing bundle file (bundles files exist for mapready and asf_view)
copy ~/gtk-mac-bundler/examples/gtk-demo.bundle to asf_tools/mapready.bundle

add to mapready.bundle:
  <binary>
    ${prefix}/lib/gdk-pixbuf-2.0/${pkg:${gtk}:gtk_binary_version}/*.so
  </binary>

and change main-binary to:
  <main-binary>${prefix}/bin/mapready</main-binary>
*** End

create a bundle by running gtk-mac-bundler <bundle_file_name> (e.g. gtk-mac-bundler mapready.bundle)
