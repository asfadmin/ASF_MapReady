#	stVec_ers
#	by Orion Lawlor, ASF, 1998

SYS     = solaris
INCLUDE = -I../asf_inc 
LIBDIR  = ../../lib/$(SYS)
CFLAGS  = ${INCLUDE}  $(FLAGS)
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a -lm
OBJS= dateUtil.o odlUtil.o odlTceUtil.o esaUtil.o nasdaUtil.o

all: stVec_ers stVec_ers_dump stVec_jers stVec_jers_dump
	-rm *.o

stVec_ers:	$(OBJS) main.c
	${CC} ${CFLAGS} ${LDFLAGS} -o stVec_ers -DERS=1 main.c $(OBJS) ${LIBS}
	mv stVec_ers ../../bin/$(SYS)

stVec_ers_dump:	$(OBJS) main.c
	${CC} ${CFLAGS} ${LDFLAGS} -o stVec_ers_dump -DERS=1 -DDUMP=1 main.c $(OBJS) ${LIBS}
	mv stVec_ers_dump ../../bin/$(SYS)

stVec_jers:	$(OBJS) main.c
	${CC} ${CFLAGS} ${LDFLAGS} -o stVec_jers -DJERS=1 main.c $(OBJS) ${LIBS}
	mv stVec_jers ../../bin/$(SYS)

stVec_jers_dump:	$(OBJS) main.c
	${CC} ${CFLAGS} ${LDFLAGS} -o stVec_jers_dump -DJERS=1 -DDUMP=1 main.c $(OBJS) ${LIBS}
	mv stVec_jers_dump ../../bin/$(SYS)


clean:
	rm -f core ${OBJS} *.o
