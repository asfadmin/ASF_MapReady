#=============================================================================
#
# Name - simple.mak
#
# Version:	1.16
#
# ccsid:	@(#)simple.mak	1.16 - 06/28/93 09:13:44
# from: 	ccs/s.simple.mak
# date: 	06/28/93 09:14:49
#
# Description: make file for xgrabsc.  Use "make -f simple.mak"
#
# Read through the options up to "all: $(PROGRAMS)" and change as needed
#
#=============================================================================

#CC=gcc
CP=cp
XRDB=/usr/X11/bin/xrdb

# change WEXT if you want to build the Motif version of the window
# interface (recommended if you have Motif 1.2 libraries).
WEXT = xm
#WEXT = xaw

# if your X include/library files aren't in normal places, use these:
XINC = -I/usr/X11/include
XLIB = -L/usr/X11/lib

# if you are building xgrabxm, and your Motif include/library files aren't
# in the normal places, use these:
MOTIFINC = -I/home1/motif/include
MOTIFLIB = -L/home1/motif/lib

# set your favorite compilation flags
#CFLAGS = -g $(XINC)
CFLAGS = -O $(XINC)

# set your favorite linker flags
#LDFLAGS = $(XLIB) -Wl,-a,archive
LDFLAGS = $(XLIB)

# change INSTALL_PATH to the directory in which you want xgrabsc installed
INSTALL_PATH    = /usr/local/bin

# change XAPPLOADDIR to the directory in which your want xgrab app-defaults
# installed
XAPPLOADDIR = /usr/X11/lib/app-defaults

# change MAN_PATH to point to your man page top directory
MAN_PATH        = /usr/local/man

# change MAN_EXT to the section for xgrabsc
MAN_EXT         = 1

PROGRAMS = xgrabsc xgrab

all: $(PROGRAMS)

xgrabsc: xgrabsc.o
	rm -f xgrabsc
	$(CC) $(LDFLAGS) $(CFLAGS) -o xgrabsc xgrabsc.o -lX11

xgrabsc.o: xgrabsc.c checkvm.h patchlevel.h cpyright.h \
  process.hc get.hc mem.hc convert.hc write.hc cmdopts.h config.h

xgrab: xgrab$(WEXT) XGrab.ad
	$(CP) xgrab$(WEXT) xgrab

XGrab.ad: XGrab$(WEXT).ad
	$(CP) XGrab$(WEXT).ad XGrab.ad

xgrabxaw: xgrabxaw.o
	rm -f xgrabxaw
	$(CC) $(LDFLAGS) $(CFLAGS) -o xgrabxaw xgrabxaw.o -lXaw -lXt -lXmu -lXext -lX11

xgrabxaw.o: xgrabxaw.c xgrabxaw_ad.h config.h

xgrabxaw_ad.h: XGrabxaw.ad
	rm -f xgrabxaw_ad.h
	sed -n '1,/! ====/p' XGrabxaw.ad | sed -n -f ad2c.sed >xgrabxaw_ad.h

XGrabxaw.ad: XGrabxaw.res config.h
	rm -f XGrabxaw.ad
	$(XRDB) -n XGrabxaw.res >XGrabxaw.ad


xgrabxm: xgrabxm.o
	rm -f xgrabxm
	$(CC) $(LDFLAGS) $(CFLAGS) -o xgrabxm xgrabxm.o $(MOTIFLIB) -lXm -lXt -lXext -lX11

xgrabxm.o: xgrabxm.c xgrabxm_ad.h config.h
	$(CC) -c $(CFLAGS) $(MOTIFINC) xgrabxm.c

xgrabxm_ad.h: XGrabxm.ad
	rm -f xgrabxm_ad.h
	sed -n '1,/! ====/p' XGrabxm.ad | sed -n -f ad2c.sed >xgrabxm_ad.h

XGrabxm.ad: XGrabxm.res config.h
	rm -f XGrabxm.ad
	$(XRDB) -n XGrabxm.res >XGrabxm.ad


install: all
	install -c -s xgrabsc $(INSTALL_PATH)
	install -c xgrab $(INSTALL_PATH)
	install -c XGrab.ad $(XAPPLOADDIR)/XGrab

install.man::
	install -c -m 644 xgrabsc.man \
		$(MAN_PATH)/man$(MAN_EXT)/xgrabsc.$(MAN_EXT)
	install -c -m 644 xgrab.man \
		$(MAN_PATH)/man$(MAN_EXT)/xgrab.$(MAN_EXT)

clean::
	rm -f core *.o xgrabsc xgrab xgrabxm xgrabxaw XGrab.ad *.log xgrab_ad.h test*
	rm -f xgrabxaw_ad.h xgrabxm_ad.h
	rm -f XGrabxaw.ad XGrabxm.ad

