# Makefile for ASF Converter
include ../mk.common

all: $(SCRIPTS)/standard.zsh $(SCRIPTS)/complex.zsh $(SCRIPTS)/pvs_gui $(SCRIPTS)/vexcel_complex.zsh $(SCRIPTS)/target_selector.zsh $(SCRIPTS)/iq.zsh $(SCRIPTS)/geo.zsh

$(SCRIPTS)/standard.zsh: standard.zsh
	cp standard.zsh $(SCRIPTS)/standard.zsh

$(SCRIPTS)/complex.zsh: complex.zsh
	cp complex.zsh $(SCRIPTS)/complex.zsh

$(SCRIPTS)/pvs_gui: pvs_gui
	cp pvs_gui $(SCRIPTS)/pvs_gui

$(SCRIPTS)/vexcel_complex.zsh: vexcel_complex.zsh
	cp vexcel_complex.zsh $(SCRIPTS)/vexcel_complex.zsh

$(SCRIPTS)/target_selector.zsh:target_selector.zsh
	cp target_selector.zsh $(SCRIPTS)/target_selector.zsh

$(SCRIPTS)/iq.zsh:iq.zsh
	cp iq.zsh $(SCRIPTS)/iq.zsh

$(SCRIPTS)/geo.zsh:geo.zsh
	cp geo.zsh $(SCRIPTS)/geo.zsh


clean:
	rm -f $(SCRIPTS)/standard.zsh
	rm -f $(SCRIPTS)/complex.zsh
	rm -f $(SCRIPTS)/pvs_gui
	rm -f $(SCRIPTS)/vexcel_complex.zsh
	rm -f $(SCRIPTS)/target_selector.zsh
	rm -f $(SCRIPTS)/iq.zsh
	rm -f $(SCRIPTS)/geo.zsh
