
include ../../make_support/system_rules

INCDIR  = /usr/Motif-2.1/include/
LIBS    =

# 
# For SGI
# 
#FC     = /usr/bin/f77
#FFLAGS = -O2 -DSGI -bytereclen -extend_source -OPT:Olimit=2500 -I$(INCDIR) 
#LC     = /usr/bin/f77  
#LFLAGS = $(LIBS)

#
# For Sun
#
FC     = f77
FFLAGS = -O2
LC     = f77
LFLAGS = $(LIBS)

FSOURCES = ampscheby.f
FOBJECTS = $(patsubst %.f, %.o, $(FSOURCES))

BINLIST	= $(BINDIR)/ampscheby

# Build into place.  Rebuilds on changes to Makefile.
$(BINLIST): $(FOBJECTS) Makefile
	$(LC) -o $@ $(FOBJECTS) $(LFLAGS)

# Compile fortran code.
%.o: %.f
	$(FC) $(FFLAGS) -c $<

clean:
	rm *.o
