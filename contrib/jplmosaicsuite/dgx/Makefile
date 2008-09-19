# Makefile for subsystem Graph program (dgx)

include ../../make_support/system_rules

BINDIR = ../../bin
LIBS   = -L/usr/X11R6/lib -lXm -lXt -lX11 -lm
HP_SGI = -DM10 -DM4 -DIO64 

FC     = g77
FFLAGS = $(OPTFLAG) $(HP_SGI) $(LARGEFILE_FLAGS) \
         -fno-globals -Wno-globals -ffixed-line-length-132

CC     = gcc
CFLAGS = $(OPTFLAG) $(HP_SGI) $(LARGEFILE_FLAGS)

DGX_C_SRC = graphx.c
DGX_C_OBJ = $(DGX_C_SRC:.c=.o)
# use .fpp extension instead of .f so that the preprocessor runs on it
DGX_F_SRC = dgx.fpp
DGX_F_OBJ = $(DGX_F_SRC:.fpp=.o)
DGX_OBJS = $(DGX_F_OBJ) $(DGX_C_OBJ)

all: dgx clean

$(DGX_C_OBJ): $(DGX_C_SRC)
	$(CC) $(CFLAGS) -c $(DGX_C_SRC)
$(DGX_F_OBJ): $(DGX_F_SRC)
	$(FC) $(FFLAGS) -c $(DGX_F_SRC)
dgx: $(DGX_OBJS) Makefile 
	$(FC) $(CFLAGS) -o dgx $(DGX_OBJS) $(LIBS)
	mv dgx $(BINDIR)

clean:
	rm -f *.o

