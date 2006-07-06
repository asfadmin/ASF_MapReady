/**
Trivial PUP::able registration demo

by Orion Sky Lawlor, olawlor@acm.org, 2005/08/26 (Public Domain)
*/
#include "pup.h"
#include <stdlib.h>

class A : public PUP::able {
public:
	A(PupMigrateMessage *) {}
	PUPable_decl(A);
};
class B : public PUP::able {
public:
	B(PupMigrateMessage *) {}
	PUPable_decl(B);
};
class C : public PUP::able {
public:
	C(PupMigrateMessage *) {}
	PUPable_decl(C);
};
class D : public PUP::able {
public:
	D(PupMigrateMessage *) {}
	PUPable_decl(D);
};

PUPable_def(A);
PUPable_def(B);
PUPable_def(C);
PUPable_def(D);

int main() {
	PUPable_reg(A);
	PUPable_reg(B);
	PUPable_reg(C);
	PUPable_reg(D);
	
	A a(0);
	A *p=(A *)a.clone();
	delete p;
	
	return 0;
}

extern "C" void CmiAbort(const char *why) {
	printf("Fatal: %s\n",why);
	exit(1);
}
