#	parse_ceos
#	by Orion Lawlor, ASF, 1997
#	A program to output the frames of CEOS structures.


SYS     = solaris
INCLUDE = -I../asf_inc 
LIBDIR  = ../../lib/$(SYS)
CFLAGS  = ${INCLUDE} -g $(FLAGS)
LIBS    =  -L$(LIBDIR) $(LIBDIR)/asf_las.a -lm

parse_ceos:	parse_ceos.c $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o parse_ceos parse_ceos.c $(OBJS) ${LIBS}
	mv parse_ceos ../../bin/$(SYS)

clean:
	rm -f core ${OBJS} *.o
