include ../../make_support/system_rules

PROGRAM = filter_tie_points.perl

all: install clean

install: $(PROGRAM)
	cp $(PROGRAM) $(BINDIR)
	chmod 0775 $(BINDIR)/$(PROGRAM)

clean:
	rm -f core *~ TAGS

