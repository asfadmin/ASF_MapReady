# Makefile for:
#       multimosaic
#	multiaffine3d

include ../Makerules.mk

MOSAIC_OBJS = multimosaic.o
	
NR = ../NR
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
	mv multimosaic$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX  Mutlimosaic Built!  XXXXXXXX"
	@echo ""

multiaffine3d: $(AFFINE_OBJS)
	$(FC) $(FFLAGS) -o multiaffine3d $(AFFINE_OBJS)
	mv multiaffine3d$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX  Mutliaffine3d Built!  XXXXXXXX"
	@echo ""
	
clean:
	rm -f *.o
