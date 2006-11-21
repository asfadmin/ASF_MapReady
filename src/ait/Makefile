include ../../make_support/system_rules

TARGET = ait
SRCS = ait.c config.c

LIBS  = \
	$(LIBDIR)/libasf_ips.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-lm

# different procedures for win/unix
ifeq ($(SYS),win32)

GCC = gcc
GTK_DIR = gtkwin32
GTK_ZIP = gtkwin32zip
GTK_ZIP_LOC = ../../../support/win32/gtk

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
	$(LIBS) \
	$(ASF_LIB)/libjpeg.a \
	-lglib-2.0 \
	-lcomdlg32

CFLAGS = -D_REENTRANT -mms-bitfields -Dwin32 -g -Wall

$(TARGET).exe: $(TARGET).c
	if [ ! -f $(GTK_DIR)/README.txt ]; then\
	  rm -rf $(GTK_ZIP); \
	  mkdir $(GTK_ZIP); \
	  cp $(GTK_ZIP_LOC)/*.zip $(GTK_ZIP); \
	  for f in $(GTK_ZIP)/*.zip; do( yes| unzip $$f -d $(GTK_DIR) ); done;\
	  chmod +x $(GTK_DIR)/bin/*.dll; \
	fi
	$(GCC) -o $(TARGET).exe $(SRCS) $(INCLUDE) $(LIB) $(CFLAGS)
	mv $(TARGET).exe $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)

clean:
	rm $(TARGET).exe core.*

else

CFLAGS += -Wall -g $(shell /usr/bin/pkg-config --cflags gtk+-2.0) \
         $(shell /usr/bin/pkg-config --cflags libglade-2.0) \
         -I$(ASF_INCLUDE_DIR)

LDLIBS = $(LIBS) \
         $(shell /usr/bin/pkg-config --libs gtk+-2.0) \
	 $(shell /usr/bin/pkg-config --libs libglade-2.0) \
         $(shell /usr/bin/pkg-config --libs gthread-2.0)

LDFLAGS = -Wl,--export-dynamic

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	gcc $(OBJS) $(LDLIBS) -o $(TARGET) $(LDFLAGS)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(SHAREDIR)

clean:
	rm -f *.o $(BINDIR)/$(TARGET)*

endif

%.c: %.h


