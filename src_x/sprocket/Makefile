# Makefile for ASF Converter
include mk.common

build: clean pass1 pass2

pass1:  bin include java lib scripts
	cd  util; make all;

pass2:
	cd image2pgm; make all;
#	cd converter_standard; make all;
	cd scripts; make all;
	cd xy_plotter; make all;
	cd converter_vexcel_complex; make all;
	cd image_masker; make all
	cd java_util; make all;
	cd java_gui; make all;
	cd point_target_selection; make all;
	cd locater; make all;
	cd pt_anal; make all;
	cd pvs_db; make all;

clean:
	cd util; make clean;
	cd converter_standard; make clean;
	cd scripts; make clean;
	cd image2pgm ; make clean;
	cd xy_plotter; make clean;
	cd converter_vexcel_complex ; make clean;
	cd image_masker; make clean
	cd java_gui; make clean;
	cd java_util; make clean;
	cd point_target_selection; make clean;
	cd locater; make clean;
	cd pt_anal; make clean;
	cd pvs_db; make clean;

TOP =..

SRC_DIRS = 				\
	image_masker			\
	image2pgm			\
	xy_plotter			\
	converter_vexcel_complex	\
	util

#	converter_standard		\

########

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


########
#	Create bin directories
##
bin: $(TOP)/bin $(TOP)/bin/Linux
$(TOP)/bin:
	mkdir $(TOP)/bin
$(TOP)/bin/Linux:
	mkdir $(TOP)/bin/Linux

##
# 	Create include directories 
##
include: $(TOP)/include
$(TOP)/include:
	mkdir $(TOP)/include


##
#       Create java directories
##
java: $(TOP)/java $(TOP)/java
$(TOP)/java:
	mkdir $(TOP)/java

##
#       Create lib directories
##
lib: $(TOP)/lib $(TOP)/lib/Linux
$(TOP)/lib:
	mkdir $(TOP)/lib
$(TOP)/lib/Linux:
	mkdir $(TOP)/lib/Linux

##
#       Create scripts directories
##
scripts: $(TOP)/scripts $(TOP)/scripts/Linux
$(TOP)/scripts:
	mkdir $(TOP)/scripts
$(TOP)/scripts/Linux:
	mkdir $(TOP)/scripts/Linux
