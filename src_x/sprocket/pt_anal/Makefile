# Makefile for pt_anal java classes 

include ../Make_rules

SOURCE= data_cuts.java \
	image_data.java \
	point_target_analysis_results.java \
	lobe.java \
	location.java \
	resolution.java \
	pt_anal.java \
	islr.java \
	pslr.java \
	clutter.java \
	side_lobe.java \
	DEBUG.java

all: pt_anal clean

pt_anal:
	$(JAVAC) $(SOURCE)
	cp *.class $(JAVADIR)

clean:
	rm -f *.class
