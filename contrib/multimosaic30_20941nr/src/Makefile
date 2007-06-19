# Makefile for:
#       multimosaic
#	multiaffine3d

FC     = g77
OPTFLAG = -O2
FFLAGS = $(OPTFLAG) -ffixed-line-length-132

MOSAIC_OBJS = multimosaic.o
	
NR = ../../NR
AFFINE_OBJS = \
	$(NR)/dgaussh.o \
	$(NR)/dpythag.o \
	$(NR)/dqrdcmp.o \
	$(NR)/dsvbksb.o \
	$(NR)/dsvdcmp.o \
	$(NR)/dsvdvar.o \
	multiaffine3d.o

all: multimosaic multiaffine3d clean

multimosaic: $(MOSAIC_OBJS)
	$(FC) $(FFLAGS) -o multimosaic $(MOSAIC_OBJS)

multiaffine3d: $(AFFINE_OBJS)
	$(FC) $(FFLAGS) -o multiaffine3d $(AFFINE_OBJS)
	
clean:
	rm -f *.o
