#	las_op
#	by Orion Lawlor, ASF, 1997
#	A program to compute any mathematical function of a set of las images.


SYS     = solaris
INCLUDE = -I../asf_inc 
LIBDIR  = ../../lib/$(SYS)
CFLAGS  = ${INCLUDE} -g $(FLAGS)
LIBS    =  -L$(LIBDIR) $(LIBDIR)/asf_las.a $(LIBDIR)/caplib.a -lm
OBJS=lasIO.o 

las_op:	$(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o las_op $(OBJS) ${LIBS}
	mv las_op ../../bin/$(SYS)
#	rm *.o

clean:
	rm -f core ${OBJS} *.o
