# Tag Image File Format Library
#
# Copyright (c) 1988-1997 Sam Leffler
# Copyright (c) 1991-1997 Silicon Graphics, Inc.
# 
# Permission to use, copy, modify, distribute, and sell this software and 
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Sam Leffler and Silicon Graphics.
# 
# THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
# 
# IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
# ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
# OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
# LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#

# Hacked up to a great extent by Patrick Denny (ASF) Oct. 2002

include ../../make_support/system_rules

AR	= ar
AROPTS	= rc

# file deletion command
RM      = rm -f

# If JPEG support is to be included and the Independent JPEG
# Software distribution is not installed then DIR_JPEG must
# refer to the directory where the include files reside.
#
# Similarly, if the libgz distribution is not installed, then
# DIR_LIBGZ must refer to the directory where the include files
# are located.  Note that recent versions 
#

ifeq ($(SYS),solaris)
 ifeq ($(CC),cc)
   COMPILE_POS_INDEP = -K PIC
 else
   COMPILE_POS_INDEP = -fpic
 endif
endif

# To enable JPEG support include -DJPEG_SUPPORT here.
# To enable Deflate support add a -DZIP_SUPPORT here.
CFLAGS := $(COMPILE_POS_INDEP) $(CFLAGS)

OBJS  = tif_aux.o \
	tif_close.o \
	tif_codec.o \
	tif_compress.o \
	tif_dir.o \
	tif_dirinfo.o \
	tif_dirread.o \
	tif_dirwrite.o \
	tif_dumpmode.o \
	tif_error.o \
	tif_fax3.o \
	tif_fax3sm.o \
	tif_getimage.o \
	tif_jpeg.o \
	tif_flush.o \
	tif_luv.o \
	tif_lzw.o \
	tif_next.o \
	tif_open.o \
	tif_packbits.o \
	tif_pixarlog.o \
	tif_predict.o \
	tif_print.o \
	tif_read.o \
	tif_swab.o \
	tif_strip.o \
	tif_thunder.o \
	tif_tile.o \
	tif_unix.o \
	tif_version.o \
	tif_warning.o \
	tif_write.o \
	tif_zip.o

all: tif_fax3sm.c libtiff.a
	mv libtiff.a $(LIBDIR)
	$(RM) ${OBJS}

libtiff.a: ${OBJS}
	$(RM) libtiff.a
	${AR} ${AROPTS} libtiff.a $(OBJS)
	$(RANLIB) libtiff.a

tif_fax3sm.c: mkg3states.c tif_fax3.h
	${CC} -o mkg3states ${CFLAGS} mkg3states.c
	$(RM) tif_fax3sm.c; ./mkg3states -c const tif_fax3sm.c

clean:
	$(RM) ${OBJS} mkg3states tif_fax3sm.c libtiff.a
	
