/**
Trivial PUP demo

by Orion Sky Lawlor, olawlor@acm.org, 2005/08/26 (Public Domain)
*/
#include "pup.h"
#include <stdlib.h>

/* On g++ 4, this freaks out the compiler with the default templated operator|: */
enum {en1=1, en2=2};
int en_or=en1|en2;

class foo {
 public:
  bool isBar;
  int x;
  char y;
  unsigned long z;
  double q[3];
  
  void pup(PUP::er &p) {
    PUPn(isBar);
    PUPn(x);PUPn(y);PUPn(z);
    PUPv(q,3);
  }
};

int main() {
	foo f;
	f.isBar=false;
	f.x=12;
	f.y='k';
	f.z=4567;
	f.q[0]=f.q[1]=1.23456;
	f.q[2]=2.3456;
	PUP::toTextFile pt(fopen("test_foo.txt","w"),true);
	f.pup(pt);
	PUP::toDisk pd(fopen("test_foo.bin","wb"));
	f.pup(pd);
	return 0;
}

extern "C" void CmiAbort(const char *why) {
	printf("Fatal: %s\n",why);
	exit(1);
}
