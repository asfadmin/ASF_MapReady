# Makefile for: Image++, the X-Windows C++ LAS image display program.

SYS=solaris
CPP=CC
XHOME=/usr/openwin
INCL=-I../asf_inc -I./incl -I$(XHOME)/include
CFLAGS=$(INCL) $(FLAGS) -O
LIBS=-L$(XHOME)/lib -lX11 ../../lib/$(SYS)/asf_las.a -lm

OBJS=o/viewer3D.o\
	o/dem3D.o\
	o/demos3D.o\
	o/object3D.o\
	o/camera3D.o\
	o/vector.o\
	o/matrix3D.o\
	o/asfimage.o\
	o/buffUtils.o\
	o/lines.o\
	o/buffer2D.o\
	o/demo.o\
	o/image.o\
	o/imageDispatch.o\
	o/imageResp.o\
	o/lasimage.o\
	o/menu.o\
	o/main.o\
	o/myResponder.o\
	o/object.o\
	o/responder.o\
	o/utilities.o\
	o/winResp.o\
	o/window.o

image: main.cxx $(OBJS)
	$(CPP) $(CFLAGS) $(OBJS) $(LIBS) -o image
#	mv image ../../bin/$(SYS)


o/viewer3D.o: 3D/viewer3D.cxx
	$(CPP) $(CFLAGS) -c 3D/viewer3D.cxx -o o/viewer3D.o

o/dem3D.o: 3D/dem3D.cxx
	$(CPP) $(CFLAGS) -c 3D/dem3D.cxx -o o/dem3D.o

o/demos3D.o: 3D/demos3D.cxx
	$(CPP) $(CFLAGS) -c 3D/demos3D.cxx -o o/demos3D.o

o/object3D.o: 3D/object3D.cxx
	$(CPP) $(CFLAGS) -c 3D/object3D.cxx -o o/object3D.o

o/camera3D.o: 3D/camera3D.cxx
	$(CPP) $(CFLAGS) -c 3D/camera3D.cxx -o o/camera3D.o

o/vector.o: 3D/vector.cxx
	$(CPP) $(CFLAGS) -c 3D/vector.cxx -o o/vector.o

o/matrix3D.o: 3D/matrix3D.cxx
	$(CPP) $(CFLAGS) -c 3D/matrix3D.cxx -o o/matrix3D.o

o/asfimage.o: asfimage.cxx
	$(CPP) $(CFLAGS) -c asfimage.cxx -o o/asfimage.o

o/lines.o: 2D/lines.cxx
	$(CPP) $(CFLAGS) -c 2D/lines.cxx -o o/lines.o

o/buffUtils.o: 2D/buffUtils.cxx
	$(CPP) $(CFLAGS) -c 2D/buffUtils.cxx -o o/buffUtils.o

o/buffer2D.o: 2D/buffer2D.cxx
	$(CPP) $(CFLAGS) -c 2D/buffer2D.cxx -o o/buffer2D.o

o/demo.o: demo.cxx
	$(CPP) $(CFLAGS) -c demo.cxx -o o/demo.o

o/image.o: image.cxx
	$(CPP) $(CFLAGS) -c image.cxx -o o/image.o

o/imageDispatch.o: imageDispatch.cxx
	$(CPP) $(CFLAGS) -c imageDispatch.cxx -o o/imageDispatch.o

o/imageResp.o: imageResp.cxx
	$(CPP) $(CFLAGS) -c imageResp.cxx -o o/imageResp.o

o/lasimage.o: lasimage.cxx
	$(CPP) $(CFLAGS) -c lasimage.cxx -o o/lasimage.o

o/main.o: main.cxx
	$(CPP) $(CFLAGS) -c main.cxx -o o/main.o

o/menu.o: menu.cxx
	$(CPP) $(CFLAGS) -c menu.cxx -o o/menu.o

o/myResponder.o: myResponder.cxx
	$(CPP) $(CFLAGS) -c myResponder.cxx -o o/myResponder.o

o/object.o: object.cxx
	$(CPP) $(CFLAGS) -c object.cxx -o o/object.o

o/responder.o: responder.cxx
	$(CPP) $(CFLAGS) -c responder.cxx -o o/responder.o

o/utilities.o: utilities.cxx
	$(CPP) $(CFLAGS) -c utilities.cxx -o o/utilities.o

o/winResp.o: winResp.cxx
	$(CPP) $(CFLAGS) -c winResp.cxx -o o/winResp.o

o/window.o: window.cxx
	$(CPP) $(CFLAGS) -c window.cxx -o o/window.o

clean:
	rm -f image++ core $(OBJS) o/*.o
