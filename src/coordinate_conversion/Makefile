include ../../make_support/system_rules

MODULE_FILE = CoordinateConversion.pm 

all: install clean

install: $(MODULE_FILE)
	cp $(MODULE_FILE) $(LIBDIR)/perl/ASF
	chmod 0664 $(LIBDIR)/perl/ASF/$(MODULE_FILE)

clean:
	rm -f core *~ TAGS
