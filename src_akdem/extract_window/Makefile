include ../../make_support/system_rules

PROGRAM = extract_window

all: install clean

install: $(PROGRAM)
	cp $(PROGRAM) $(BINDIR)
	chmod 0775 $(BINDIR)/$(PROGRAM)

clean:
	rm -f core *~ TAGS
