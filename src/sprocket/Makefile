# Makefile for Sprocket
# Order things are compiled in MATTERS

include ../../make_support/system_rules


PROGRAM=$(LIBDIR)/libUtil.a \
	$(BINDIR)/java_util \
	$(BINDIR)/convert_standard \
	$(BINDIR)/converter_vexcel_complex \
	$(BINDIR)/image2pgm \
	$(BINDIR)/image_mask \
	$(BINDIR)/xy_plotter \
	$(BINDIR)/java_gui \
	$(BINDIR)/locater \
	$(BINDIR)/measure_geo \
	$(BINDIR)/point_target_selection \
	$(BINDIR)/pt_anal \
	$(BINDIR)/pvs_db \
	$(BINDIR)/scripts


all: create_dirs $(PROGRAM)

create_dirs:
	mkdir ../../java

clean: clean_message clean_libUtil clean_java_util clean_convert_standard \
	clean_converter_vexcel_complex clean_image2pgm clean_image_mask \
	clean_xy_plotter clean_java_gui clean_locater clean_measure_geo \
	clean_point_target_selection clean_pt_anal clean_pvs_db \
	clean_scripts

clean_message:
	@ echo ""
	@ echo XXXXXXXX   REMOVING ENTIRE SPROCKET PROGRAM   XXXXXXXX

##################################################
### Utility source that MUST be compiled first ###
##################################################

####################### Library libUtil.a  ######################
$(LIBDIR)/libUtil.a:
	@ echo ""
	@ echo "XXXXXXXX  Compiling Sprocket library libUtil.a... XXXXXXXX"
	cd util; $(MAKE)

clean_libUtil:
	- rm -f $(LIBDIR)/libUtil.a

####################### Classes in java_util ######################
$(BINDIR)/java_util:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket classes in java_util... XXXXXXXX"
	cd java_util; $(MAKE)

clean_java_util:
	- rm -f $(JAVADIR)/*.class


##################################################
####### C source which depends on libUtil ########
##################################################

####################### Program convert_standard ######################
$(BINDIR)/convert_standard:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket program convert_standard... XXXXXXXX"
	cd convert_standard; $(MAKE)

clean_convert_standard:
	- rm -f $(BINDIR)/convert_standard

####################### Program converter_vexcel_complex ######################
$(BINDIR)/converter_vexcel_complex:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket converter_vexcel_complex programs... XXXXXXXX"
	cd converter_vexcel_complex; $(MAKE)

clean_converter_vexcel_complex:
	- rm -f $(BINDIR)/vexcel_complex_metadata
	- rm -f $(BINDIR)/vexcel_complex_data_planes

####################### Program image2pgm ######################
$(BINDIR)/image2pgm:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket program image2pgm... XXXXXXXX"
	cd image2pgm; $(MAKE)

clean_image2pgm:
	- rm -f $(BINDIR)/image2pgm

####################### Program image_mask ######################
$(BINDIR)/image_mask:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket program image_mask... XXXXXXXX"
	cd image_mask; $(MAKE)

clean_image_mask:
	- rm -f $(BINDIR)/image_mask

####################### Program xy_plotter ######################
$(BINDIR)/xy_plotter:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket program xy_plotter... XXXXXXXX"
	cd xy_plotter; $(MAKE)

clean_xy_plotter:
	- rm -f $(BINDIR)/xy_plotter


##################################################
#### Java source which depends on java_util ######
##################################################

####################### Classes in java_gui ######################
$(BINDIR)/java_gui:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket classes in java_gui... XXXXXXXX"
	cd java_gui; $(MAKE)

clean_java_gui:
	- rm -f $(JAVADIR)/*.class $(JAVADIR)/pvs.pref

####################### Classes in locater ######################
$(BINDIR)/locater:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket classes in locater... XXXXXXXX"
	cd locater; $(MAKE)

clean_locater:
	- rm -f $(JAVADIR)/Locater.class $(JAVADIR)/geotransform.jar

####################### Classes in measure_geo ######################
$(BINDIR)/measure_geo:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket classes in measure_geo... XXXXXXXX"
	cd measure_geo; $(MAKE)

clean_measure_geo:
	- rm -f $(JAVADIR)/MeasureGeoLocationErrors.class

####################### Classes in point_target_selection ######################
$(BINDIR)/point_target_selection:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket classes in point_target_selection... XXXXXXXX"
	cd point_target_selection; $(MAKE)

clean_point_target_selection:
	- rm -f $(JAVADIR)/*.class

####################### Classes in pt_anal ######################
$(BINDIR)/pt_anal:
	@ echo ""
	@ echo "XXXXXXXX Compiling Sprocket classes in pt_anal... XXXXXXXX"
	cd pt_anal; $(MAKE)

clean_pt_anal:
	- rm -f $(JAVADIR)/*.class


##################################################
###### SQL Database stuff that uses .sqsh ########
##################################################

####################### Database pvs_db ######################
$(BINDIR)/pvs_db:
	@ echo ""
	@ echo "XXXXXXXX Sprocket database pvs_db... XXXXXXXX"
	cd pvs_db; $(MAKE)

clean_pvs_db:
	- rm -f $(BINDIR)/list_ground_targets.sql
	- rm -f $(BINDIR)/list_sites.sql


##################################################
#### Scripts that run programs in proper env #####
##################################################

####################### Scripts ######################
$(BINDIR)/scripts:
	@ echo ""
	@ echo "XXXXXXXX Sprocket scripts... XXXXXXXX"
	cd scripts; $(MAKE)

clean_scripts:
	- rm -f $(BINDIR)/standard
	- rm -f $(BINDIR)/complex
	- rm -f $(BINDIR)/sprocket
	- rm -f $(BINDIR)/vexcel_complex
	- rm -f $(BINDIR)/target_selector
	- rm -f $(BINDIR)/iq
	- rm -f $(BINDIR)/geo



#### OLD STUFF #################################################################
SRC_DIRS=\
	image_masker \
	image2pgm \
	xy_plotter \
	convert_standard \
	converter_vexcel_complex \
	util

doc:
#
# Create the cross reference files
#
	@for dir in $(SRC_DIRS) ; do \
	   cd $$dir ; \
	   echo Cross referencing $$dir ; \
	   cxref -xref -O../../doc *.c -I../../include ; \
	   cd .. ;\
	done
#
# Create the source files using cxref
#
	@for dir in $(SRC_DIRS) ; do \
	   cd $$dir; \
	   echo Documenting $$dir ; \
	   cxref -warn-xref -xref -O../../doc -html32-src *.c -I../../include ; \
	   cd .. ;\
	done
#
# Create the index using cxref
#
	@echo Indexing ; \
	cxref -index-all -O../doc -html32
