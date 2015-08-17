CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = stp
SRCS = stp.c
OBJS = $(patsubst %.c, %.o, $(SRCS))

LIBS  = \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(PNG_LIBS) \
	$(GSL_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(HDFEOS5_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

# different procedures for win/unix

# Windows build instructions
ifeq ($(SYS),win32)

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
	-L/mingw/lib \
	-lgdi32 \
	-llibgdk-win32-2.0 \
	$(LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS += -D_REENTRANT -mms-bitfields -g -Wall

$(TARGET).exe: $(SRCS)
	$(GCC) -o stp.exe stp.c $(INCLUDE) $(LIB) $(CFLAGS)
	$(GCC) -o stp_cli.exe stp_cli.c $(INCLUDE) $(LIB) $(CFLAGS)
	mv stp.exe stp_cli.exe $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp *.gif $(SHAREDIR)
	cp $(TARGET)_icon.ico $(SHAREDIR)

clean:
	-rm -f $(TARGET).exe core.*

# UNIX build instructions
else

CFLAGS += -Wall -g $(shell /usr/bin/pkg-config --cflags gtk+-2.0) \
         $(shell /usr/bin/pkg-config --cflags libglade-2.0) \
         -I$(ASF_INCLUDE_DIR)

LDLIBS = $(LIBS) \
         $(shell /usr/bin/pkg-config --libs gtk+-2.0) \
	 $(shell /usr/bin/pkg-config --libs libglade-2.0) \
         $(shell /usr/bin/pkg-config --libs gthread-2.0)

ifeq ($(SYS),darwin)
LDFLAGS = -dynamic
else
LDFLAGS = -Wl,--export-dynamic
endif

default: $(OBJS)
	gcc $(OBJS) $(LDLIBS) -o $(TARGET) $(LDFLAGS)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp *.gif $(SHAREDIR)
	cp $(TARGET)_icon.ico $(SHAREDIR)

clean:
	-rm -f *.o $(BINDIR)/$(TARGET)*

doc:
	docbook2html -u stp_install.sgml
	docbook2html -u stp_usage.sgml

endif

%.c: %.h

