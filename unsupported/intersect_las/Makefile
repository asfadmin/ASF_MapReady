#	intersect_las
#	by Orion Lawlor, ASF, 1997
#	A program to compute the diffence between two LAS images.


SYS     = solaris
INCLUDE = -I../asf_inc 
LIBDIR  = ../../lib/$(SYS)
CFLAGS  = ${INCLUDE} -O $(FLAGS)
LIBS    =  -L$(LIBDIR) $(LIBDIR)/asf_las.a $(LIBDIR)/caplib.a -lm
OBJS=lasIO.o

intersect_las:	intersect_las.c $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o intersect_las intersect_las.c $(OBJS) ${LIBS}
	mv intersect_las ../../bin/$(SYS)
	rm *.o

clean:
	rm -f core ${OBJS} *.o
