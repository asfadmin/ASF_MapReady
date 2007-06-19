# Makefile for:
#	multimatch	an image matching program
#	multicull	description
#	multitps	description


BINDIR  = ../bin
XHOME   = -L/usr/X11R6/lib
LIBS    = $(XHOME) -lXm -lXt -lX11 -lm
OPTFLAG = -O2
HP_SGI = -DSGI

FC     = g77
FFLAGS = $(OPTFLAG) $(HP_SGI) -ffixed-line-length-132

CC     = gcc
CFLAGS = $(OPTFLAG) $(HP_SGI)

MATCH_C_SRC = graphx.c
MATCH_C_OBJ = $(MATCH_C_SRC:.c=.o)
MATCH_F_SRC = multimatch.f
MATCH_F_OBJ = $(MATCH_F_SRC:.f=.o)
MATCH_OBJS = $(MATCH_F_OBJ) $(MATCH_C_OBJ)

NR = ../../NR
CULL_OBJS = \
	$(NR)/dgaussh.o \
	$(NR)/dpythag.o \
	$(NR)/dqrdcmp.o \
	$(NR)/dsvbksb.o \
	$(NR)/dsvdcmp.o \
	$(NR)/dsvdvar.o \
	multicull.o

TPS_OBJS = \
	multitps.o

all: multimatch multicull multitps

$(MATCH_C_OBJ): $(MATCH_C_SRC)
	$(CC) $(CFLAGS) -c $(MATCH_C_SRC)
$(MATCH_F_OBJ): $(MATCH_F_SRC)
	$(FC) $(FFLAGS) -c $(MATCH_F_SRC)
multimatch: $(MATCH_OBJS) Makefile 
	$(FC) $(OPTFLAG) -o multimatch $(MATCH_OBJS) $(LIBS)

multicull: $(CULL_OBJS)
	$(FC) $(FFLAGS) -o multicull $(CULL_OBJS)

multitps: $(TPS_OBJS)
	$(FC) $(FFLAGS) -o multitps $(TPS_OBJS)

clean:
	rm -f *.o

#f77 -extend_source -bytereclen -O3 multimosaic.f -o multimosaic
#f77 -extend_source -bytereclen -O multitps.f -o multitps
#
##f77 -extend_source -bytereclen -O multicull.f -o multicull
#f77 -extend_source -bytereclen -O multicull.f -o multicull ../../NR/*.f
#
#g77 -ffixed-line-length-132 -O3 multimosaic.f -o multimosaic
