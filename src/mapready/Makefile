CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = mapready
DOC_BUILD_MACHINE = dq.asf.alaska.edu
SRCS = mapready.c find_in_path.c execute.c callbacks.c utility.c dnd.c popup_menu.c settings.c file_list.c help.c rename_output.c file_selection.c state.c change_output_dir.c naming_scheme.c metadata.c geocode.c summary.c ceos_thumbnail.c win_font.c projfile.c view_output.c share.c terrcorr.c log.c blow_up_in.c blow_up_out.c png_util.c plugins.c tiff_util.c
CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(SHAPELIB_CFLAGS)

# Unfortunately compilation is completely different on windows
ifeq ($(SYS),win32)

### Begin Windows Section

GCC =gcc 
GTK_INC = $(GTKWIN32_DIR)/include
GTK_LIB = $(GTKWIN32_DIR)/lib

ASF_LIB = ../../lib

INCLUDE = \
	$(JPEG_CFLAGS) $(XML_CFLAGS) \
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
	$(XML_LIBS) \
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
	$(ASF_LIB)/libasf_convert.a \
	$(ASF_LIB)/libasf_import.a \
	$(ASF_LIB)/libasf_export.a \
	$(ASF_LIB)/libasf_terrcorr.a \
	$(ASF_LIB)/libasf_geocode.a \
	$(ASF_LIB)/libasf_ardop.a \
	$(ASF_LIB)/libasf_sar.a \
	$(ASF_LIB)/libasf_vector.a \
	$(ASF_LIB)/libshp.a \
	$(ASF_LIB)/asf_meta.a \
	$(ASF_LIB)/libasf_raster.a \
	$(ASF_LIB)/asf_fft.a \
	$(ASF_LIB)/libasf_proj.a \
	$(ASF_LIB)/asf.a \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(ZLIB_LIBS) \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS += -DLARGE64_FILES -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall

$(TARGET).exe: $(TARGET).c
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe $(BINDIR)/$(TARGET).exe
	cp $(TARGET).glade $(SHAREDIR)/$(TARGET).glade
	cp $(TARGET).ico $(SHAREDIR)/$(TARGET).ico
	-cp -f plugins.cfg $(SHAREDIR)
	-cp htmlhelp.chm $(SHAREDIR)/$(TARGET).chm

htmlhelp.chm: mapready.sgml
	xsltproc --nonet /usr/share/docbook-xsl/htmlhelp/htmlhelp.xsl \
	  mapready.sgml
	-/cygdrive/c/Program\ Files/HTML\ Help\ Workshop/hhc.exe htmlhelp.hhp

clean:
	rm -f $(TARGET).exe core.* *.html *.hhc *.hhp *.chm

### End Windows Section

else

### Begin Unix Section

CFLAGS += $(W_ERROR) -Wall \
	$(GSL_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PNG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GTK_CFLAGS) \
	$(XML_CFLAGS) \
	$(shell /usr/bin/pkg-config --cflags libglade-2.0) \
	$(shell /usr/bin/pkg-config --cflags gthread-2.0)

LDLIBS = \
	$(LIBDIR)/libasf_convert.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-ljpeg \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(GTK_LIBS) \
	$(PNG_LIBS) \
	$(XML_LIBS) \
	$(shell /usr/bin/pkg-config --libs libglade-2.0) \
	$(shell /usr/bin/pkg-config --libs gthread-2.0) \
	-lm -lz

ifeq ($(SYS),darwin)
LDFLAGS = -dynamic -framework Carbon 
else
LDFLAGS = -Wl,--export-dynamic
endif

OBJS = $(patsubst %.c, %.o, $(SRCS))

DB2H = $(shell which docbook2html 2>/dev/null)
DB2T = $(shell which docbook2txt 2>/dev/null)

#default: clean doc $(OBJS)
default: $(OBJS)
	$(CC) $(OBJS) $(LDLIBS) $(LDFLAGS) -o $(TARGET)
	mv $(TARGET) $(BINDIR)/$(TARGET)
	cp $(TARGET).glade $(SHAREDIR)/$(TARGET).glade
	cp $(TARGET).ico $(SHAREDIR)/$(TARGET).ico
	-cp -f $(TARGET).txt $(SHAREDIR)/$(TARGET).txt
	-cp -f plugins.cfg $(SHAREDIR)
	-mv -f how_to_install.html $(SHAREDIR)/how_to_install_$(TARGET).html

%.c: %.h

doc: mapready.txt how_to_install.html

ifeq "$(NODENAME)" "$(DOC_BUILD_MACHINE)"

# Documents CAN be built ...we're on the right machine!!
# (the one with the right version of doctools)

mapready.txt: mapready.sgml
	@if test "X$(DB2T)" = "X"; then \
	  echo "No documentation built - docbook2* tools not found."; \
	else \
	  echo "Building the help documentation."; \
	  docbook2txt mapready.sgml; \
	fi

else

# Documents CANNOT be built ...we're not on the right machine!!
# If the .sgml files are newer than the text file versions (if
# they exist), and the current build is intended to be a release
# build ...GO BUILD ON THE RIGHT MACHINE (see DOC_BUILD_MACHINE
# at the top of this make file)

mapready.txt: mapready.sgml
	@echo "Building the help documentation."
	@if [ ! -f $(TARGET).txt ] ; then \
	  echo "\n**********\nWARNING: Help file text documents ($(TARGET).txt) cannot"; \
	  echo "be built on this machine.  If this build is a release build"; \
	  echo "then you must build (and check in) the documents on"; \
	  echo "$(DOC_BUILD_MACHINE) instead, then check out the built"; \
	  echo "documents here.\n"; \
	  echo "For now however, a blank $(TARGET).txt file will be"; \
	  echo "created.  This will result in a \"Help file not found\" error"; \
	  echo "message to show up in the help window in the MapReady GUI application.\n"; \
	  echo "=> Do NOT check in the $(TARGET).txt file created here!  It's blank!!!"; \
	  echo "   Do your required edits on $(DOC_BUILD_MACHINE), build the docs there, and THEN check them in"; \
	  echo "   from there!\n**********\n"; \
	  touch $(TARGET).txt; \
	else \
	  echo "\n**********\nWARNING: Help file text documents ($(TARGET).txt) cannot"; \
	  echo "be built on this machine.  If this build is a release build"; \
	  echo "then you must build (and check in) the documents on"; \
	  echo "$(DOC_BUILD_MACHINE) instead, then check out the built"; \
	  echo "documents here.\n"; \
	  echo "=> $(TARGET).txt was FOUND here ...but you need to check to see if it"; \
	  echo "   is non-empty and current if you are doing a release build, i.e. compare the .txt"; \
	  echo "   file date/time to the date/time on the $(TARGET).sgml file etc.\n"; \
	  echo "=> Do NOT check in the $(TARGET).txt file unless it is non-empty AND correct!"; \
	  echo "   Do your required edits on $(DOC_BUILD_MACHINE), build the docs there, and THEN check them in"; \
	  echo "   from there!\n**********\n"; \
	fi

# END of doc build section for mapready.txt help file creation
endif

# Non-RedHat doctools work fine for HTML creation ...only the text
# output from docbook2text doesn't seem to work (yet?)
#
how_to_install.html: how_to_install.sgml
	@if test "X$(DB2H)" = "X"; then \
	  echo "No documentation built - docbook2* tools not found."; \
	else \
	  echo "Building the installation documentation."; \
	  docbook2html --error no-valid --nochunks --output . \
	    how_to_install.sgml; \
	fi

clean:
	rm -f *.o \
	  $(BINDIR)/$(TARGET) \
	  $(SHAREDIR)/how_to_install_$(TARGET).html  \
	  $(SHAREDIR)/$(TARGET).* \
	  $(DOCDIR)/$(TARGET)* 

### End Unix Section

endif

