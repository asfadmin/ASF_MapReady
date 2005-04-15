include ../../make_support/system_rules

TARGET = aisp_gui
SRCS = aisp_gui.c find_in_path.c

CFLAGS = -Wall -O0 -g $(shell /usr/bin/pkg-config --cflags gtk+-2.0) \
         $(shell /usr/bin/pkg-config --cflags libglade-2.0) \
         -I$(ASF_INCLUDE_DIR)

LDLIBS = $(shell /usr/bin/pkg-config --libs gtk+-2.0) \
	 $(shell /usr/bin/pkg-config --libs libglade-2.0) \
         $(shell /usr/bin/pkg-config --libs gthread-2.0)

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	gcc $(OBJS) $(LDLIBS) -o $(TARGET)
	mv $(TARGET) $(BINDIR)
	cp $(TARGET).glade $(BINDIR)
#	cp $(TARGET).ico $(BINDIR)

%.c: %.h

clean:
	rm -f *.o $(BINDIR)/$(TARGET)*

