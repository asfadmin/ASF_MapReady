# Makefile for Sprocket scripts

include ../Make_rules

SOURCE= $(BINDIR)/standard.zsh \
	$(BINDIR)/complex.zsh \
	$(BINDIR)/pvs_gui \
	$(BINDIR)/vexcel_complex.zsh \
	$(BINDIR)/target_selector.zsh \
	$(BINDIR)/iq.zsh \
	$(BINDIR)/geo.zsh

all: $(SOURCE)

$(BINDIR)/standard.zsh: standard.zsh
	cp standard.zsh $(BINDIR)/standard.zsh

$(BINDIR)/complex.zsh: complex.zsh
	cp complex.zsh $(BINDIR)/complex.zsh

$(BINDIR)/pvs_gui: pvs_gui
	cp pvs_gui $(BINDIR)/pvs_gui

$(BINDIR)/vexcel_complex.zsh: vexcel_complex.zsh
	cp vexcel_complex.zsh $(BINDIR)/vexcel_complex.zsh

$(BINDIR)/target_selector.zsh:target_selector.zsh
	cp target_selector.zsh $(BINDIR)/target_selector.zsh

$(BINDIR)/iq.zsh:iq.zsh
	cp iq.zsh $(BINDIR)/iq.zsh

$(BINDIR)/geo.zsh:geo.zsh
	cp geo.zsh $(BINDIR)/geo.zsh
