# Makefile for Sprocket's SEQUEL database

include ../Make_rules

TARGETS=$(BINDIR)/list_ground_targets.sql \
	$(BINDIR)/list_sites.sql

all: $(TARGETS)

$(BINDIR)/list_ground_targets.sql: list_ground_targets.sql
	cp list_ground_targets.sql $(BINDIR)

$(BINDIR)/list_sites.sql: list_sites.sql
	cp list_sites.sql $(BINDIR)
