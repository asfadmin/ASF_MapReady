# Makefile for subsystem Graph program (dgx)

include ../Makerules.mk

CFLAGS += -DM10 -DM4 -DIO64
FFLAGS += -DM4 -DIO64

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
	$(FC) $(CFLAGS) -o dgx $(DGX_OBJS) $(X_LIBS)
	mv dgx $(BINDIR)

clean:
	rm -f *.o

