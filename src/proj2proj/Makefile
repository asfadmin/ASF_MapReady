CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = proj2proj
SRCS = proj2proj.c utility.c font.c projfile.c geocode.c execute.c new.c

REQUIRED_LIBS = \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	$(PROJ_LIBS) -lm

CFLAGS += $(GLIB_CFLAGS)

default: asf_proj2proj proj2proj
cli.o: cli.c

asf_proj2proj: cli.o
	$(CC) $(CFLAGS) -o asf_proj2proj cli.o $(REQUIRED_LIBS) $(LDFLAGS)
	mv asf_proj2proj$(BIN_POSTFIX) $(BINDIR)

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
	-lm \
	-lgtk-win32-2.0 \
	-lgobject-2.0 \
	-llibglade-2.0 \
	-llibgdk_pixbuf-2.0 \
	-lpango-1.0 \
	-L/usr/lib/w32api \
	-lgdi32 \
	-llibgdk-win32-2.0 \
	$(ASF_LIB)/libasf_convert.a \
	$(ASF_LIB)/libasf_import.a \
	$(ASF_LIB)/libasf_export.a \
	$(ASF_LIB)/libasf_geocode.a \
	$(ASF_LIB)/libasf_terrcorr.a \
	$(ASF_LIB)/libasf_ardop.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/libasf_sar.a \
	$(ASF_LIB)/libasf_vector.a \
	$(ASF_LIB)/libasf_raster.a \
	$(ASF_LIB)/libshp.a \
	$(ASF_LIB)/asf_meta.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/asf.a \
	$(ASF_LIB)/asf_fft.a \
	$(XML_LIBS) \
	-liconv \
	$(GSL_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(PROJ_LIBS) \
	$(ZLIB_LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS += -DLARGE64_FILES -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall

$(TARGET): $(TARGET).c
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET)_icon.ico $(SHAREDIR)

clean:
	rm -rf $(GTK_DIR) $(TARGET).exe asf_$(TARGET) core.*

### End Windows Section

else

### Begin Unix Section

CFLAGS += \
	$(GTK_CFLAGS) \
	$(shell /usr/bin/pkg-config --cflags libglade-2.0) \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(JPEG_CFLAGS)

LDLIBS = \
	$(LIBDIR)/libasf_metadata.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(SHAPELIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
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

proj2proj: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDLIBS) $(LDFLAGS)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET)_icon.ico $(SHAREDIR)

%.c: %.h

clean:
	rm -f *.o $(BINDIR)/$(TARGET)* $(TARGET).txt \
	    $(SHAREDIR)/$(TARGET)* asf_$(TARGET)

### End Unix Section

endif

