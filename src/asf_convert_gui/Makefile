include ../../make_support/system_rules

TARGET = asf_convert_gui
SRCS = asf_convert_gui.c find_in_path.c execute.c callbacks.c utility.c dnd.c popup_menu.c settings.c file_list.c help.c rename_output.c file_selection.c state.c change_output_dir.c naming_scheme.c metadata.c geocode.c summary.c ceos_thumbnail.c win_font.c projfile.c view_output.c share.c

# Unfortunately compilation is completely different on windows
ifeq ($(SYS),win32)

### Begin Windows Section

GCC = gcc
GTK_DIR = gtkwin32
GTK_ZIP = gtkwin32zip

GTK_INC = $(GTK_DIR)/include
GTK_LIB = $(GTK_DIR)/lib

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
	-lpthread \
	-lintl \
	-liconv \
	-lm \
	-lgtk-win32-2.0 \
	-lgobject-2.0 \
	-llibglade-2.0 \
	-llibgdk_pixbuf-2.0 \
	-lpango-1.0 \
	-L/usr/lib/w32api \
	-lgdi32 \
	-llibgdk-win32-2.0 \
	$(ASF_LIB)/libasf_raster.a \
	$(ASF_LIB)/libasf_meta.a \
	$(ASF_LIB)/asf_meta.a \
	$(ASF_LIB)/asf.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS = -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall

$(TARGET).exe: $(TARGET).c
	if [ ! -f $(GTK_DIR)/README.txt ]; then\
	  for f in $(GTK_ZIP)/*.zip; do( yes| unzip $$f -d $(GTK_DIR) ); done;\
	  chmod +x $(GTK_DIR)/bin/*.dll; \
	fi
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET).ico $(SHAREDIR)
	cp htmlhelp.chm $(SHAREDIR)/asf_convert_gui.chm

doc:
	xsltproc --nonet /usr/share/docbook-xsl/htmlhelp/htmlhelp.xsl \
	  asf_convert_gui.sgml
	/cygdrive/c/Program\ Files\HTML\ Help\ Workshop\hhc.exe htmlhelp.hhp


clean:
	rm -rf $(GTK_DIR) $(TARGET).exe core.* *.html *.hhc *.hhp *.chm

### End Windows Section

else

### Begin Unix Section

CFLAGS = -Wall -O0 -g $(shell /usr/bin/pkg-config --cflags gtk+-2.0) \
         $(shell /usr/bin/pkg-config --cflags libglade-2.0) \
         -I$(ASF_INCLUDE_DIR)

LDLIBS = $(LIBDIR)/asf_meta.a \
         $(LIBDIR)/libasf_raster.a \
	 $(LIBDIR)/libgsl.a \
	 $(LIBDIR)/libasf_proj.a \
	 $(LIBDIR)/libproj.a \
	 $(LIBDIR)/libasf_reporting.a \
         $(LIBDIR)/libgslcblas.a \
	 $(JPEG_LIBS) \
         $(shell /usr/bin/pkg-config --libs gtk+-2.0) \
	 $(shell /usr/bin/pkg-config --libs libglade-2.0) \
         $(shell /usr/bin/pkg-config --libs gthread-2.0)

LDFLAGS = -Wl,--export-dynamic

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	gcc $(OBJS) $(LDLIBS) $(LDFLAGS) -o $(TARGET)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)
	cp $(TARGET).ico $(SHAREDIR)
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
	rm -f *.o $(BINDIR)/$(TARGET)* $(TARGET).txt \
	    $(SHAREDIR)/$(TARGET)* $(DOCDIR)/$(TARGET)*

### End Unix Section

endif

