#include "units.h"

int bar(void);
int foo(void) {
	return (int)constants::c;
}

int main() {
	foo();
	bar();
}
