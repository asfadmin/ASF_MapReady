# Makefile for:
#	multimatch	an image matching program
#	multicull	description
#	multitps	description

include ../Makerules.mk

CFLAGS += -DSGI
FFLAGS += -DSGI

MATCH_C_SRC = graphx.c
MATCH_C_OBJ = $(MATCH_C_SRC:.c=.o)
MATCH_F_SRC = multimatch.f
MATCH_F_OBJ = $(MATCH_F_SRC:.f=.o)
MATCH_OBJS = $(MATCH_F_OBJ) $(MATCH_C_OBJ)

NR = ../NR
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
	$(FC) $(FFLAGS) -o multimatch $(MATCH_OBJS) $(X_LIBS)
	mv multimatch$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX   Multimatch Built!   XXXXXXXX"
	@echo ""

multicull: $(CULL_OBJS) Makefile
	$(FC) $(FFLAGS) -o multicull $(CULL_OBJS)
	mv multicull$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX   Multicull Built!   XXXXXXXX"
	@echo ""

multitps: $(TPS_OBJS) Makefile
	$(FC) $(FFLAGS) -o multitps $(TPS_OBJS)
	mv multitps$(BIN_POSTFIX) $(BINDIR)
	@echo "XXXXXXXX   Multitps Built!   XXXXXXXX"
	@echo ""

clean:
	rm -f *.o
