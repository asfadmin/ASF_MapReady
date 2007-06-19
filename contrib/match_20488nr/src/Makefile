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

all: multimatch multicull multitps clean
	
$(MATCH_C_OBJ): $(MATCH_C_SRC)
	$(CC) $(CFLAGS) -c $(MATCH_C_SRC)
$(MATCH_F_OBJ): $(MATCH_F_SRC)
	$(FC) $(FFLAGS) -c $(MATCH_F_SRC)
multimatch: $(MATCH_OBJS) Makefile
	$(FC) $(OPTFLAG) -o multimatch $(MATCH_OBJS) $(LIBS)
	cp -p multimatch$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX   Multimatch Built!   XXXXXXXX"
	@echo ""

multicull: $(CULL_OBJS) Makefile
	$(FC) $(FFLAGS) -o multicull $(CULL_OBJS)
	cp -p multicull$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX   Multicull Built!   XXXXXXXX"
	@echo ""

multitps: $(TPS_OBJS) Makefile
	$(FC) $(FFLAGS) -o multitps $(TPS_OBJS)
	cp -p multitps$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX   Multitps Built!   XXXXXXXX"
	@echo ""

clean:
	rm -f *.o
