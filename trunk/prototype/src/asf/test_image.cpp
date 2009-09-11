/**
Test routine for image.cpp routines.

Orion Sky Lawlor, olawlor@acm.org, 2005/11/8.
*/
#include "asf/image.h"
#include <stdlib.h>

using namespace asf;

/* Abort if these two vectors are substantially different */
void assert_equal(const image_location &a,const image_location &b) {
	double lenSqr=a.distSqr(b);
	double lenTol=1.0e-10;
	if (lenSqr>lenTol*lenTol) {
		printf("  (%f,%f) vs (%f,%f)\n",
			a.x,a.y, b.x,b.y);
		die("Vectors are too different!");
	}
}

/** Return random number on the range [0,1) */
double rand_float(void) {
	return (rand()&0xffff)*(1.0/0x10000);
}

/* Generate a random vector on the unit square */
image_location rand_vec(void) {
	return image_location(rand_float(),rand_float());
}

/* Check out this mapping for consistency */
void check_function(location_function *f,const char *fn) {
	int i,ntest=10000;
	/* Make sure the function does what it promises */
	linear2d_function DfmS=f->linearize(rand_vec());
	print(DfmS);
	linear2d_function SfmD=DfmS.inverse();
	print(SfmD);
	for (i=0;i<ntest;i++) {
		image_location s=rand_vec();
		image_location d=f->apply(s);
		switch (f->getRelationship()) {
		case location_function::identity: 
			assert_equal(s,d); /* source and dest equal ... */
		case location_function::translation: 
			assert_equal(DfmS.o+s,d);  /* equal plus offset ... */
			assert_equal(s,SfmD.o+d);
		case location_function::scaling:
		case location_function::linear:
			assert_equal(DfmS.apply(s),d);  /* up to linearization */
			assert_equal(s,SfmD.apply(d));  /* up to linearization */
			break;
		default: /* no assertion is possible for nonlinear functions*/
			break;
		}
	}
	
	/* Make sure the bounding rectangle actually contains the image */
	pixel_rectangle src(100,1000,200,2000);
	pixel_rectangle dest(f->apply_rectangle(src));
	// print(dest);
	for (i=0;i<ntest;i++) {
		image_location s=rand_vec();
		s=rect_frac(src,s.x,s.y);
		image_location d=f->apply(s);
		if (!dest.inbounds((int)d.x,(int)d.y))
			die("location_function boundary containment lost");
	}
	
	/* Make sure the inverse works */
	location_function *inv=f->make_inverse();
	for (i=0;i<ntest;i++) {
		image_location x=rand_vec();
		assert_equal(x,inv->apply(f->apply(x)));
		assert_equal(x,f->apply(inv->apply(x)));
	}
	
	printf("Test passed: %s\n",fn);
}

ASF_PLUGIN_EXPORT int main() {
	srand(1);
	location_function_identity id;
	check_function(&id,"identity");
	location_function_translation tr(pixel_location(10,20));
	check_function(&tr,"translation");
	for (int i=0;i<5;i++) {
		linear2d_function m(
			200*rand_vec(), // offset
			rand_vec(), // X axis
			4*rand_vec() // Y axis
		);
		print(m);
		location_function_linear li(m);
		check_function(&li,"linear");
	}
}

