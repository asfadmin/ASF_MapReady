all: mko0 

CFLAGS = -I../../include/global

mko0: mko0.c ../lib_framegen/fgtime.o
	$(CC) -o mko0 mko0.c ../lib_framegen/fgtime.o

clean:
	rm -f mko0 

