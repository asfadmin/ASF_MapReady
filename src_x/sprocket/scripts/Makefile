# Makefile for Sprocket scripts

include ../Make_rules


all: standard  complex  sprocket  vexcel_complex  target_selector  iq  geo

standard: standard.sh
	@ echo ""
	cp standard.sh $(BINDIR)/standard
	chmod 775 $(BINDIR)/standard

complex: complex.sh
	@ echo ""
	cp complex.sh $(BINDIR)/complex
	chmod 775 $(BINDIR)/complex

sprocket: sprocket.sh
	@ echo ""
	cp sprocket.sh $(BINDIR)/sprocket
	chmod 775 $(BINDIR)/sprocket

vexcel_complex: vexcel_complex.sh
	@ echo ""
	cp vexcel_complex.sh $(BINDIR)/vexcel_complex
	chmod 775 $(BINDIR)/vexcel_complex

target_selector: target_selector.sh
	@ echo ""
	cp target_selector.sh $(BINDIR)/target_selector
	chmod 775 $(BINDIR)/target_selector

iq: iq.sh
	@ echo ""
	cp iq.sh $(BINDIR)/iq
	chmod 775 $(BINDIR)/iq

geo: geo.sh
	@ echo ""
	cp geo.sh $(BINDIR)/geo
	chmod 775 $(BINDIR)/geo
