include ../../make_support/system_rules

TARGET = asf_convert_gui
SRCS = asf_convert_gui.c find_in_path.c execute.c callbacks.c utility.c dnd.c popup_menu.c settings.c file_list.c help.c rename_output.c file_selection.c state.c change_output_dir.c

# Unfortunately compilation is completely different on windows
ifeq ($(SYS),win32)

### Begin Windows Section

GCC = gcc
GTK_DIR = gtkwin32
GTK_ZIP = gtkwin32zip

GTK_INC = $(GTK_DIR)/include
GTK_LIB = $(GTK_DIR)/lib

INCLUDE = -I$(GTK_INC) -I$(GTK_INC)/glib-2.0 -I$(GTK_INC)/pango-1.0 -I$(GTK_INC)/gtk-2.0 -I$(GTK_INC)/freetype2/freetype -I$(GTK_LIB)/glib-2.0/include -I$(GTK_LIB)/gtk-2.0/include -I$(GTK_INC)/atk-1.0 -I$(GTK_INC)/libglade-2.0

LIB = -L$(GTK_LIB) -L$(GTK_LIB)/glib-2.0 -L$(GTK_LIB)/gtk-2.0 -L$(GTK_LIB)/pango -L$(GTK_LIB)/libglade -lintl -liconv -lm -lgtk-win32-2.0 -lgobject-2.0 -llibglade-2.0 -lglib-2.0

CFLAGS = -mms-bitfields -Dwin32 -g -Wall

$(TARGET).exe: $(TARGET).c
	if [ ! -f $(GTK_DIR)/README.txt ]; then\
	  for f in $(GTK_ZIP)/*.zip; do( yes| unzip $$f -d $(GTK_DIR) ); done;\
	  chmod +x $(GTK_DIR)/bin/*.dll; \
	fi
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe ../../bin/win32
	cp $(TARGET).glade ../../bin/win32
	cp $(TARGET)_help.txt ../../bin/win32

clean:
	rm -rf $(GTK_DIR) $(TARGET).exe core.*

### End Windows Section

else

### Begin Unix Section

CFLAGS = -Wall -O0 -g $(shell /usr/bin/pkg-config --cflags gtk+-2.0) \
         $(shell /usr/bin/pkg-config --cflags libglade-2.0)

LDLIBS = $(shell /usr/bin/pkg-config --libs gtk+-2.0) \
	 $(shell /usr/bin/pkg-config --libs libglade-2.0)

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	gcc $(OBJS) $(LDLIBS) -o $(TARGET)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(BINDIR)
	cp $(TARGET)_help.txt $(BINDIR)

clean:
	rm *.o $(TARGET)

### End Unix Section

endif

