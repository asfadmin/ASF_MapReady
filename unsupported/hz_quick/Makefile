######################################################################
# Makefile for 		:  aisp.c 
# Module Author 	:  Tom Logan
# Module Version	:  1.0
# Platform		:  Solaris 
# Makefile Date		:  5/98 
# Written to handle SYSV specs.
######################################################################
# 
# Set the path to the ASF Step Tools include & library directories
#
INCDIR = ../asf_inc
LIBDIR = ../../lib/$(SYS)

#
# Set compile options, libraries list, and list of objects required
#
CFLAGS =  -I. -I$(INCDIR) $(FLAGS)
FFLAGS= $(FLAGS)
LIBS   = $(LIBDIR)/asf_fft.a $(LIBDIR)/asf_ceos.a \
        $(LIBDIR)/asf_meta.a $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a
LIBC = $(LIBS) -lm 

OBJLIB = cfft1d_risc.o \
	hz_quick.o

all: hz_quick
	rm *.o

hz_quick: $(OBJLIB) 
	$(F77) $(FFLAGS) -o hz_quick $(OBJLIB) $(LIBC)
	mv hz_quick ../../bin/$(SYS)
