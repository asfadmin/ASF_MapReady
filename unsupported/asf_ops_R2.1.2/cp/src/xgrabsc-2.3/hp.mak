#=============================================================================
#
# Name - hp.mak
#
# Version:	1.3
#
# ccsid:	@(#)hp.mak	1.3 - 06/25/93 12:54:20
# from: 	ccs/s.hp.mak
# date: 	06/28/93 09:14:48
#
# Description: make file for xgrabsc for HPUX 9.01.  Use "make -f hp.mak"
#
# Read through the options up to "all: $(PROGRAMS)" and change as needed
#
#=============================================================================

# change WEXT if you want to build the Athena version of the window
# interface (not recommended if you have Motif 1.2 libraries).  The default
# for HP machines is to build the Motif version, but you need v1.2
# which is shipped with HPUX 9.0 and later.  If you have 8.07 you should
# build the xaw version.
WEXT = xm
#WEXT = xaw

#CC=gcc
CP=cp
XRDB=/usr/bin/X11/xrdb

# if your X include/library files aren't in normal places, use these:
XINC = -I/usr/include/X11R5
XLIB = -L/usr/lib/X11R5

# if you are building xgrabxm, and your Motif include/library files aren't
# in the normal places, use these:
MOTIFINC = -I/usr/include/Motif1.2
MOTIFLIB = -L/usr/lib/Motif1.2

# set your favorite compilation flags
#CFLAGS = -g $(XINC)
CFLAGS = +O3 $(XINC)

# set your favorite linker flags
#LDFLAGS = $(XLIB) -Wl,-a,archive
LDFLAGS = $(XLIB)

# change INSTALL_PATH to the directory in which you want xgrabsc installed
INSTALL_PATH    = /usr/bin/X11

# change XAPPLOADDIR to the directory in which your want xgrab app-defaults
# installed
XAPPLOADDIR = /usr/lib/X11/app-defaults

# change MAN_PATH to point to your man page top directory
MAN_PATH        = /usr/man

# change MAN_EXT to the section for xgrabsc
MAN_EXT         = n

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

