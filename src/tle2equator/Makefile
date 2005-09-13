# Makefile for          :  tle2equator
# Module Author         :  Rudi Gens 

OBJS = 	tle2equator.o
CFLAGS += -Wall -g
CC = gcc

tle2equator: $(OBJS)
	gcc -o tle2equator $(OBJS) -lm
	rm *.o
	cp tle2equator ../../bin/solaris


