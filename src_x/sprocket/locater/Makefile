include ../mk.common
all:
	cp geotransform.jar $(JAVA)
	$(JAVAC) Locater.java
	cp *.class $(JAVA) 

clean:
	rm -f *.class $(JAVA)/Locater.class $(JAVA)/geotransform.jar

test:
	java Locater /scratch2/test_images/vexcel/new_version/1675/out
