#	stVec_test
#	by Orion Lawlor, ASF, 1998

SYS     = solaris
INCLUDE = -I. -I../asf_inc 
LIBDIR  = ../../lib/$(SYS)
CFLAGS  = ${INCLUDE}  $(FLAGS)
LIBS    = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf_ceos.a $(LIBDIR)/asf.a -lm
OBJS= dateUtil.o odlUtil.o main.o prop.o

all: stVec_test
	-rm *.o

stVec_test: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o stVec_test $(LIBS)
	mv stVec_test ../../bin/$(SYS)
