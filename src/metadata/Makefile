# Makefile for 	:  metadata.c 
# Module Author 	:  Tom Logan & Mike Shindle  
# Module Version	:  1.1

include ../../make_support/system_rules

OBJLIB = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a \
	 prn_facdr.o \
	 prn_raddr.o \
	 prn_ifiledr.o \
	 prn_mpdr.o \
	 prn_dssr.o \
	 prn_dhr.o \
	 prn_ppdr.o \
	 prn_atdr.o \
	 prn_rsr.o \
	 prn_dqsr.o \
	 prn_fdr.o

all: metadata
	rm *.o
 
metadata:	metadata.c $(OBJLIB)
	$(CC) $(CFLAGS) -o metadata metadata.c $(OBJLIB) -lm
	mv  metadata ../../bin/$(SYS)/metadata
	cp metadata.man ../../man/cat1/metadata.1 

clean:
	rm -f *.o core
