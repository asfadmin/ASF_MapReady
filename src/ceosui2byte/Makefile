# Makefile for          :  ceosui2byte
# Module Author         :  Tom Logan 
# Module Version        :  3.0
# Platform              :  IRIX 
# Makefile Date         :  2/00  

include ../../make_support/system_rules
 
LIBS =	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a

OBJS =	ceosui2byte.o create_ddr.o

all: ceosui2byte clean

ceosui2byte : $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ceosui2byte $(OBJS) $(LIBS)
	mv ceosui2byte ../../bin/$(SYS)
	cp ceosui2byte.man ../../man/cat1/ceosui2byte.1

clean:
	-rm *.o
