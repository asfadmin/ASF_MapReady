# Makefile for Sprocket's SEQUEL database

include ../Make_rules

TARGETS=$(BINDIR)/list_ground_targets.sql \
	$(BINDIR)/list_sites.sql

all: $(TARGETS)

$(BINDIR)/list_ground_targets.sql: list_ground_targets.sql
# Put sqsh version 1.7 in the binary directory
	cp sqsh $(BINDIR)
	chmod 775 $(BINDIR)/sqsh
# Get the absolute path to sqsh and point the sql script to it
	@ cd $(BINDIR) ; echo '#!'`pwd`'/sqsh -i' > unga
	@ mv $(BINDIR)/unga unga
	@ cp list_ground_targets.sql bunga
	@ cat unga bunga > list_ground_targets.temp
	@ rm -f unga bunga
# Actually make list_ground_targets.sql
	mv list_ground_targets.temp $(BINDIR)/list_ground_targets.sql
	chmod 775 $(BINDIR)/list_ground_targets.sql

$(BINDIR)/list_sites.sql: list_sites.sql
# Put sqsh version 1.7 in the binary directory
	cp sqsh $(BINDIR)
	chmod 775 $(BINDIR)/sqsh
# Get the absolute path to sqsh and point the sql script to it
	@ cd $(BINDIR) ; echo '#!'`pwd`'/sqsh -i' > unga
	@ mv $(BINDIR)/unga unga
	@ cp list_sites.sql bunga
	@ cat unga bunga > list_sites.temp
	@ rm -f unga bunga
# Actually make list_sites.sql
	mv list_sites.temp $(BINDIR)/list_sites.sql
	chmod 775 $(BINDIR)/list_sites.sql

clean:
	rm -f $(BINDIR)/list_sites.sql $(BINDIR)/list_ground_targets.sql $(BINDIR)/sqsh
