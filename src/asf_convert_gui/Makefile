PROGRAM = asf_convert_gui

CFLAGS = -Wall -O0 -g $(shell pkg-config --cflags libgtk+-2.0) \
         $(shell pkg-config --cflags libglade-2.0)

LDLIBS = $(shell pkg-config --libs gtk+-2.0) \
	 $(shell pkg-config --libs libglade-2.0)

SRCS = asf_convert_gui.c

OBJS = $(patsubst %.c, %.o, $(SRCS))

default: $(OBJS)
	gcc $(OBJS) $(LDLIBS) -o $(PROGRAM)

clean:
	rm *.o $(PROGRAM)
