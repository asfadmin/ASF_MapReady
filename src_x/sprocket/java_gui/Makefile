# Makefile for java_gui


include  ../Make_rules

JAVA_SOURCE = BlurFilter.java \
	aboutDialog.java \
	commDialog.java \
	contDialog.java \
	helpFrame.java \
	imageCanvas.java \
	importImage.java \
	logger.java \
	mask.java \
	pointtargets.java \
	prefDialog.java \
	progressMeter.java \
	pvs.java \
	smask.java \
	targetFrame.java \
	textFrame.java \
	zoomCanvas.java \
	zoomFrame.java


all: java_gui clean

java_gui: 
	$(JAVAC) $(JAVA_SOURCE)
	cp *.class $(JAVADIR)
	cp pvs.pref $(JAVADIR)

clean:
	rm -f *.class
