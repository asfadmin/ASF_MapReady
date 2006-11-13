include ../../make_support/system_rules

TARGET = tmp1
#SRCS = main.cpp calc.cpp win_font.cpp
#SRCS = main.cpp helloworld.cpp win_font.cpp
SRCS = main.cpp ips.cpp

# Unfortunately compilation is completely different on windows
ifeq ($(SYS),win32)

### Begin Windows Section

GCC = g++
GTK_DIR = gtkwin32
GTK_ZIP = gtkwin32zip
GTK_ZIP_LOC = ../../../support/win32/gtkmm

GTK_INC = $(GTK_DIR)/include
GTK_LIB = $(GTK_DIR)/lib

ASF_LIB = ../../lib

INCLUDE = \
	-I$(GTK_INC) \
	-I$(GTK_INC)/cairo \
	-I$(GTK_INC)/glib-2.0 \
	-I$(GTK_INC)/glibmm-2.4 \
	-I$(GTK_INC)/gdkmm-2.4 \
	-I$(GTK_INC)/pango-1.0 \
	-I$(GTK_INC)/pangomm-1.4 \
	-I$(GTK_INC)/gtk-2.0 \
	-I$(GTK_INC)/gtkmm-2.4 \
	-I$(GTK_INC)/sigc++-2.0 \
	-I$(GTK_INC)/freetype2/freetype \
	-I$(GTK_LIB)/glib-2.0/include \
	-I$(GTK_LIB)/glibmm-2.4/include \
	-I$(GTK_LIB)/gtk-2.0/include \
	-I$(GTK_LIB)/gtkmm-2.4/include \
	-I$(GTK_LIB)/gdkmm-2.4/include \
	-I$(GTK_LIB)/sigc++-2.0/include \
	-I$(GTK_INC)/atk-1.0 \
	-I$(GTK_INC)/atkmm-1.6 \
	-I$(GTK_INC)/libglade-2.0 \
	-I$(GTK_INC)/libglademm-2.4 \
	-I$(ASF_INCLUDE_DIR)

LIB = \
	$(LDFLAGS) \
	-L$(GTK_LIB) \
	-L$(GTK_LIB)/glib-2.0 \
	-L$(GTK_LIB)/glibmm-2.4 \
	-L$(GTK_LIB)/gtk-2.0 \
	-L$(GTK_LIB)/gtkmm-2.4 \
	-L$(GTK_LIB)/pango \
	-L$(GTK_LIB)/libglade \
	-L$(GTK_LIB)/libglademm-2.4 \
	-llibgthread-2.0 \
	-lpthread \
	-lintl \
	-liconv \
	-lm \
	-lgtk-win32-2.0 \
	-lgtkmm-2.4 \
	-lglib-2.0 \
	-lglibmm-2.4 \
	-lsigc-2.0 \
	-latkmm-1.6 \
	-lgobject-2.0 \
	-llibglademm-2.4 \
	-llibglade-2.0 \
	-llibgdk_pixbuf-2.0 \
	-lpango-1.0 \
	-L/usr/lib/w32api \
	-lgdi32 \
	-llibgdk-win32-2.0 \
#	$(ASF_LIB)/libasf_convert.a \
	$(ASF_LIB)/libasf_import.a \
	$(ASF_LIB)/libasf_export.a \
	$(ASF_LIB)/libasf_geocode.a \
	$(ASF_LIB)/libasf_terrcorr.a \
	$(ASF_LIB)/libasf_ardop.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/libasf_sar.a \
	$(ASF_LIB)/libasf_vector.a \
	$(ASF_LIB)/asf_meta.a \
	$(ASF_LIB)/asf_odl.a \
	$(ASF_LIB)/asf.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/libasf_raster.a \
	$(ASF_LIB)/asf_fft.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(PROJ_LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS = -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall

$(TARGET).exe: $(SRCS)
	if [ ! -f $(GTK_DIR)/README.txt ]; then\
	  rm -rf $(GTK_ZIP); \
	  mkdir $(GTK_ZIP); \
	  cp $(GTK_ZIP_LOC)/*.zip $(GTK_ZIP); \
	  for f in $(GTK_ZIP)/*.zip; do( yes| unzip $$f -d $(GTK_DIR) ); done;\
	  chmod +x $(GTK_DIR)/bin/*.dll; \
	fi
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe $(BINDIR)
	cp $(GTK_DIR)/bin/* $(BINDIR)
	mkdir -p $(BINDIR)/etc $(BINDIR)/lib
	cp -R $(GTK_DIR)/lib/* $(BINDIR)/lib
	cp -R $(GTK_DIR)/etc/* $(BINDIR)/../etc
#	cp $(TARGET).glade $(SHAREDIR)
#	cp $(TARGET).ico $(SHAREDIR)

clean:
	rm -rf $(GTK_DIR) $(GTK_ZIP) $(TARGET).exe core.* *.html *.hhc *.hhp *.chm

### End Windows Section

else

### Begin Unix Section

CFLAGS += -Wall -Werror \
	$(GSL_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GTK_CFLAGS) \
	$(shell /usr/bin/pkg-config --cflags libglade-2.0) \
	$(shell /usr/bin/pkg-config --cflags gthread-2.0)

LDLIBS = \
	$(LIBDIR)/libasf_convert.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(GTK_LIBS) \
	$(shell /usr/bin/pkg-config --libs libglade-2.0) \
	$(shell /usr/bin/pkg-config --libs gthread-2.0)

LDFLAGS = -Wl,--export-dynamic

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: clean doc $(OBJS)
	$(CC) $(OBJS) $(LDLIBS) $(LDFLAGS) -o $(TARGET)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET).ico $(SHAREDIR)
	cp earth2.gif $(SHAREDIR)
	cp information_icon.gif $(SHAREDIR)
	cp $(TARGET).txt $(SHAREDIR)

%.c: %.h

doc: asf_convert_gui.txt

DB2T = $(shell which docbook2txt 2>/dev/null)

asf_convert_gui.txt:
	if test "X$(DB2T)" = "X"; then \
	  echo "No documentation built - docbook2 tools not found."; \
	else \
	  docbook2txt asf_convert_gui.sgml; \
	  cp $(TARGET).txt $(SHAREDIR); \
	fi

clean:
	rm -f *.o $(BINDIR)/$(TARGET)* \
	    $(SHAREDIR)/$(TARGET)* $(DOCDIR)/$(TARGET)*

### End Unix Section

endif

