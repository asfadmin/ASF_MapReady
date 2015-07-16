CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
include ../../make_support/system_rules

TARGET = asf_view
PLANNER_TARGET = asf_planner
SRCS = asf_view.c \
	utility.c \
	font.c \
	small_image.c \
	read.c \
	read_asf.c \
	read_ceos.c \
	read_jpeg.c \
	read_tiff.c \
	read_png.c \
	read_pgm.c \
	read_airsar.c \
	read_terrasar.c \
	read_roipac.c \
	read_ras.c \
	read_envi.c \
	read_generic.c \
	read_uavsar.c \
	read_seasat_h5.c \
	big_image.c \
	meta.c \
	stats.c \
	google.c \
	new.c \
	cache.c \
	subset.c \
	bands.c \
	info.c \
	lut.c \
	planner.c \
	pan.c \
	goto.c \
	shape.c \
	csv.c \
	excel.c

CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(SHAPELIB_CFLAGS)

# Unfortunately compilation is completely different on windows
ifeq ($(SYS),win32)

### Begin Windows Section

GCC = gcc
PLANNER_IMAGERY = ../../../support/planner_imagery

GTK_INC = $(GTKWIN32_DIR)/include
GTK_LIB = $(GTKWIN32_DIR)/lib

ASF_LIB = ../../lib

INCLUDE = \
	$(JPEG_CFLAGS) \
	$(XML_CFLAGS) \
	-I$(ASF_INCLUDE_DIR) \
	-I$(GTK_INC) \
	-I$(GTK_INC)/glib-2.0 \
	-I$(GTK_INC)/pango-1.0 \
	-I$(GTK_INC)/gtk-2.0 \
	-I$(GTK_INC)/freetype2/freetype \
	-I$(GTK_LIB)/glib-2.0/include \
	-I$(GTK_LIB)/gtk-2.0/include \
	-I$(GTK_INC)/atk-1.0 \
	-I$(GTK_INC)/libglade-2.0

LIB = \
	$(LDFLAGS) \
	-L$(GTK_LIB) \
	-L$(GTK_LIB)/glib-2.0 \
	-L$(GTK_LIB)/gtk-2.0 \
	-L$(GTK_LIB)/pango \
	-L$(GTK_LIB)/libglade \
	-llibgthread-2.0 \
	-lintl \
	-lm -lz \
	-lgtk-win32-2.0 \
	-lgobject-2.0 \
	-llibglade-2.0 \
	-llibgdk_pixbuf-2.0 \
	-lpango-1.0 \
	-L/mingw/lib -L/usr/lib/w32api \
	-lgdi32 \
	-llibgdk-win32-2.0 \
	$(ASF_LIB)/libasf_vector.a \
	$(ASF_LIB)/libasf_import.a \
	$(ASF_LIB)/libasf_geocode.a \
	$(ASF_LIB)/libasf_plan.a \
	$(ASF_LIB)/libasf_sar.a \
	$(ASF_LIB)/libasf_sgpsdp.a \
	$(ASF_LIB)/libshp.a \
	$(ASF_LIB)/libasf_raster.a \
	$(ASF_LIB)/asf_meta.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/asf.a \
	$(XML_LIBS) \
	-liconv \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(ZLIB_LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS += -DLARGE64_FILES -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall

$(TARGET).exe: $(TARGET).c
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	cp $(TARGET).exe $(PLANNER_TARGET).exe
	cp planner_icon.ico $(SHAREDIR)
	mv $(TARGET).exe $(BINDIR)
	mv $(PLANNER_TARGET).exe $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	-cp $(PLANNER_IMAGERY)/* $(SHAREDIR)
	cp startup.jpg $(SHAREDIR)
	cp asf_view_icon.ico $(SHAREDIR)
	cp asf_view_cfg.csv $(SHAREDIR)
#	cp $(TARGET).ico $(BINDIR)

clean:
	rm -f $(TARGET).exe core.*

### End Windows Section

else

### Begin Unix Section

CFLAGS += $(W_ERROR) \
	$(GTK_CFLAGS) \
	$(shell /usr/bin/pkg-config --cflags libglade-2.0) \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PNG_CFLAGS) \
	$(HDF5_CFLAGS)

LDLIBS = \
	$(LIBDIR)/libasf_metadata.a \
	$(LIBDIR)/libasf_plan.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_sgpsdp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(SHAPELIB_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PNG_LIBS) \
	$(GTK_LIBS) \
	$(shell /usr/bin/pkg-config --libs libglade-2.0) \
	$(shell /usr/bin/pkg-config --libs gthread-2.0) \
	-lm -lz

ifeq ($(SYS),darwin)
LDFLAGS = -dynamic
else
LDFLAGS = -Wl,--export-dynamic
endif

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDLIBS) $(LDFLAGS)
	cp $(TARGET) $(PLANNER_TARGET)
	cp planner_icon.ico $(SHAREDIR)
	mv $(TARGET) $(BINDIR)
	mv $(PLANNER_TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp startup.jpg $(SHAREDIR)
	cp asf_view_icon.ico $(SHAREDIR)
	cp asf_view_cfg.csv $(SHAREDIR)
	cp planner_icon.ico $(SHAREDIR)

%.c: %.h

clean:
	rm -f *.o $(BINDIR)/$(TARGET)* $(TARGET).txt \
	    $(SHAREDIR)/$(TARGET)* *.bak

### End Unix Section

endif

