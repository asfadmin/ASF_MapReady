# Makefile for java point target fetcher
include ../mk.common

TARGETS= $(BIN)/list_ground_targets.sql  $(BIN)/list_sites.sql

all: $(TARGETS)

$(BIN)/list_ground_targets.sql:list_ground_targets.sql
	cp list_ground_targets.sql $(BIN)

$(BIN)/list_sites.sql:list_sites.sql
	cp list_sites.sql $(BIN)

clean:
	rm -f $(TARGETS)

