This is the ASF C++ Prototype, a tile-based, dynamically loaded,
next-generation implementation of the ASF tools.

To build and test everything on a Linux box, just do
	make

To build Doxygen documentation in doc/html/index.html, do
	make docs

To build in Windows, use
	make.bat

---------------------- v0.7 (2006/07/06) --------------
Rudimentary metadata support.

For the Windows version:
	copy build\compile.arch.win32 build\compile.arch
	make
	copy lib\asf_coredll.dll bin\
(that way Windows can find asf_coredll.dll)

---------------------- v0.6 (2006/05/11) --------------
Now includes image I/O.  See plugins/image_*.

At the moment, this program relies on the ASF_LIBRARY_PATH
environment variable to find its plugins.  So if you get an 
error like:

FATAL ERROR: asf::registry> No such plugin image_testpattern

Then you just need to set your ASF_LIBRARY_PATH environment variable:
(bash)
	export ASF_LIBRARY_PATH=`pwd`/../lib
(csh/ksh)
	setenv ASF_LIBRARY_PATH `pwd`/../lib

Yup, this is annoying.  I need to hardcode a variety of search 
paths into the executable.

---------------------- v0.4 (2005/10/20) --------------

Builds without warnings using GCC 3.4.

---------------------- v0.3 (2005/10/12) --------------
Now includes image processing.

To build everything and run tests:
	make test

---------------------- v0.2 (2005/09/15) --------------
Now includes control flow (asf/plugin_control.h), a
command-line parser (clui/clui.cpp), and dynamic library
loading (asf/loader.*).

See plugins/ for example plugins (.cpp files) and test
scripts (.test files) along with outputs (.test.good files).

To run:
	make
	cd plugins
	./build_all.sh
	./test_all.sh

---------------------- v0.1 (2005/09/01) --------------
A very bare-bones prototype of the new ASF Tools 
"plugin" architecture.  See asf/plugin.h for the main flow,
or asf/test_* for examples.  "make test && ./test" to see
the thing in action.

	Orion Sky Lawlor, olawlor@acm.org, 2005/09/01

(pup/ is my code, but may be contaminated with UIUC IP)
