###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS source tree.
#
# Author:       S. Hardman
#
# Date:         11/18/94
#
###############################################################################

include makedefs


all:	glib app

glib:	glib_ext

app:	glib_int access_apps disp_apps fa_apps fts_apps media_apps migrate_apps op_apps util_apps v0_apps examples_apps


glib_ext:
	cd elib; $(MAKE) all
	cd eolib; $(MAKE) all
	cd plib; $(MAKE) glib_ext
	cd v0/llib; $(MAKE) all

glib_int:
	cd glib; $(MAKE) all
	cd plib; $(MAKE) glib_int

access_apps:
	cd access; $(MAKE) all

disp_apps:
	cd disp; $(MAKE) all

examples_apps:
	cd examples; $(MAKE) all

fa_apps:
	cd fa; $(MAKE) all

fts_apps:
	cd fts; $(MAKE) all

media_apps:
	cd media; $(MAKE) all

migrate_apps:
	cd migrate; $(MAKE) all

op_apps:
	cd op; $(MAKE) all

util_apps:
	cd util; $(MAKE) all

v0_apps:
	cd v0; $(MAKE) appl

clean:
	cd elib; $(MAKE) clean
	cd eolib; $(MAKE) clean
	cd plib; $(MAKE) clean
	cd glib; $(MAKE) clean
	cd access; $(MAKE) clean
	cd disp; $(MAKE) clean
	cd examples; $(MAKE) clean
	cd fa; $(MAKE) clean
	cd fts; $(MAKE) clean
	cd media; $(MAKE) clean
	cd migrate; $(MAKE) clean
	cd op; $(MAKE) clean
	cd v0; $(MAKE) clean
	cd util; $(MAKE) clean

dirs:
	mkdir -p ../bin/$(MACH)
	mkdir -p ../lib/$(MACH)/local
	mkdir -p ../lib/$(MACH)/global
	mkdir -p ../include/local
	cd ../include; ln -s ../src/eincl global

glib_inst:
	cd elib; $(MAKE) install
	cd eolib; $(MAKE) install
	cd plib; $(MAKE) ext_inst 
	cd v0/llib; $(MAKE) install

bin_inst:
	cd glib; $(MAKE) install
	cd plib; $(MAKE) int_inst
	cd access; $(MAKE) install
	cd disp; $(MAKE) install
	cd examples; $(MAKE) install
	cd fa; $(MAKE) install
	cd fts; $(MAKE) install
	cd media; $(MAKE) install
	cd migrate; $(MAKE) install
	cd op; $(MAKE) install
	cd v0; $(MAKE) install
	cd util; $(MAKE) install
	@echo
	@echo "Installing Man Pages"
	@rm -f $(ASFMAN)/man1/ims_*.1
	@rm -f $(ASFMAN)/man1/IMS_*.1
	cp -p ../etc/man/man1/*.1 $(ASFMAN)/man1
	@rm -f $(ASFMAN)/man3/ims_*.3
	cp -p ../etc/man/man3/*.3 $(ASFMAN)/man3
	@rm -f $(ASFMAN)/man7/*.7
	cp -p ../etc/man/man7/*.7 $(ASFMAN)/man7
	@echo
	@echo "Installing Scripts"
	cp -p ../etc/sh_scripts/IMS_* $(ASFBIN)
	@rm -f $(ASFRDD)/imsdads/*
	cp -p ../etc/docs/rdd/ims_*.mif $(ASFRDD)/imsdads
	@rm -rf $(ASFINST)/imsdads/*
	cp -rp ../etc/db/* $(ASFINST)/imsdads

loc_inst:
	@echo
	@echo "Installing LOCAL support files"
	mkdir -p $(LOCAL)/imsdads/config
	mkdir -p $(LOCAL)/imsdads/app-defaults/pixmaps
	mkdir -p $(LOCAL)/logs
	cp -p  ../etc/install/config/* $(LOCAL)/imsdads/config
	cp -rp ../etc/install/app-defaults/* $(LOCAL)/imsdads/app-defaults

port_glib:
	cd elib; $(MAKE) all
	cd eolib; $(MAKE) all
	cd plib; $(MAKE) glib_ext

port_app:
	cd glib; $(MAKE) all
	cd plib; $(MAKE) glib_int
	cd access; $(MAKE) all
	cd fts/clnt_app; $(MAKE) ../../../bin/$(MACH)/ims_askClnt
	cd fts/clnt_app; $(MAKE) ../../../bin/$(MACH)/ims_ftsClnt

port_clean:
	cd elib; $(MAKE) clean
	cd eolib; $(MAKE) clean
	cd plib; $(MAKE) clean
	cd glib; $(MAKE) clean
	cd access; $(MAKE) clean
	cd fts; $(MAKE) clean

port_glib_inst:
	cd elib; $(MAKE) install
	cd eolib; $(MAKE) install
	cd plib; $(MAKE) ext_inst

port_bin_inst:
	cd glib; $(MAKE) install
	cd plib; $(MAKE) int_inst
	cd access; $(MAKE) install
	@echo
	@echo "Installing Executables and Scripts"
	cp -p ../etc/sh_scripts/IMS_access.sh $(ASFBIN)
	cp -p ../bin/$(MACH)/ims_askClnt $(ASFBIN)
	cp -p ../bin/$(MACH)/ims_ftsClnt $(ASFBIN)
	@echo
	@echo "Installing Man Pages"
	@rm -f $(ASFMAN)/man1/ims_*.1
	cp -p ../etc/man/man1/*.1 $(ASFMAN)/man1
	@rm -f $(ASFMAN)/man3/ims_*.3
	cp -p ../etc/man/man3/*.3 $(ASFMAN)/man3

port_loc_inst:
	@echo
	@echo "Installing LOCAL support files"
	mkdir -p $(LOCAL)/imsdads/config
	mkdir -p $(LOCAL)/imsdads/app-defaults/pixmaps
	mkdir -p $(LOCAL)/logs
	cp -p  ../etc/install/config/* $(LOCAL)/imsdads/config
	cp -rp ../etc/install/app-defaults/* $(LOCAL)/imsdads/app-defaults

