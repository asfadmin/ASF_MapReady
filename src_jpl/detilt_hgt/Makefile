
include ../../make_support/system_rules

# 
# For SGI
# 
#INCDIR  = /usr/Motif-2.1/include/
#
#FC     = /usr/bin/f77
#FFLAGS = -O2 -DSGI -bytereclen -extend_source -OPT:Olimit=2500 -I$(INCDIR)

#
# For Sun
#
FC     = f77
FFLAGS = -O2

FOBJECTS = detilt_hgt.o

all: detilt_hgt clean

# Compile fortran code.
detilt_hgt: $(FOBJECTS)
	$(FC) $(FFLAGS) -o detilt_hgt $(FOBJECTS)

clean:
	$(FOBJECTS)
