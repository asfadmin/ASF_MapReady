# Makefile for: Makemake
# Makefile author: Orion Lawlor
# Makefile date: 6/17/98

CC=gcc

CFLAGS=$(FLAGS)
OBJS=makemake.o

all: makemake
	rm *.o

makemake: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o makemake

dist: clean

clean:
	-rm makemake
