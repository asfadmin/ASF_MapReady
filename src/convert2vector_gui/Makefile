CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = c2v
SRCS = c2v.c utility.c font.c browse.c process.c alos2kml.c google.c asf_fork.c

# Unfortunately compilation is completely different on windows
ifeq ($(SYS),win32)

### Begin Windows Section

GCC = gcc
GTK_INC = $(GTKWIN32_DIR)/include
GTK_LIB = $(GTKWIN32_DIR)/lib

ASF_LIB = ../../lib

INCLUDE = \
	-I$(GTK_INC) \
	-I$(GTK_INC)/glib-2.0 \
	-I$(GTK_INC)/pango-1.0 \
	-I$(GTK_INC)/gtk-2.0 \
	-I$(GTK_INC)/freetype2/freetype \
	-I$(GTK_LIB)/glib-2.0/include \
	-I$(GTK_LIB)/gtk-2.0/include \
	-I$(GTK_INC)/atk-1.0 \
	-I$(GTK_INC)/libglade-2.0 \
	-I$(ASF_INCLUDE_DIR)

LIB = \
	$(LDFLAGS) \
	-L$(GTK_LIB) \
	-L$(GTK_LIB)/glib-2.0 \
	-L$(GTK_LIB)/gtk-2.0 \
	-L$(GTK_LIB)/pango \
	-L$(GTK_LIB)/libglade \
	-llibgthread-2.0 \
	-lintl \
	-liconv \
	-lm \
	-lgtk-win32-2.0 \
	-lgobject-2.0 \
	-llibglade-2.0 \
	-llibgdk_pixbuf-2.0 \
	-lpango-1.0 \
	-L/usr/lib/w32api \
	-lgdi32 -llibgdk-win32-2.0 \
	$(ASF_LIB)/libasf_vector.a \
	$(ASF_LIB)/libasf_import.a \
	$(ASF_LIB)/libasf_raster.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/libshp.a \
	$(ASF_LIB)/asf_meta.a \
	$(ASF_LIB)/asf.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(PROJ_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS = -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall -DBUILD_REQ

$(TARGET).exe: $(TARGET).c
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET).png $(SHAREDIR)

clean:
	rm -rf $(GTK_DIR) $(TARGET).exe core.*

### End Windows Section

else

### Begin Unix Section

CFLAGS += \
	$(GTK_CFLAGS) \
	$(shell /usr/bin/pkg-config --cflags libglade-2.0) \
	$(GSL_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS)

LDLIBS = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(PROJ_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS) \
	$(GTK_LIBS) \
	$(shell /usr/bin/pkg-config --libs libglade-2.0) \
	$(shell /usr/bin/pkg-config --libs gthread-2.0) \
	-lm

ifeq ($(SYS),darwin)
LDFLAGS = -dynamic
else
LDFLAGS = -Wl,--export-dynamic
endif

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDLIBS) $(LDFLAGS)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET).png $(SHAREDIR)

%.c: %.h

clean:
	rm -f *.o $(BINDIR)/$(TARGET)* $(TARGET).txt \
	    $(SHAREDIR)/$(TARGET)*

### End Unix Section

endif

