FLGS=$(FLAGS) -g
FFLAGS=$(FLGS)
CFLAGS=$(FLGS)

all: newtprop.f main.c
	f77 $(FFLAGS) -c newtprop.f
	$(CC) $(CFLAGS) main.c newtprop.o -o newtprop
	mv newtprop ../../bin/$(SYS)
	