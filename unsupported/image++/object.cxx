#include "object.h"

void object::init0(void)
{
	status=0x0070BABE;
}
void object::die(void)
{
	status=0xDEADBEEF;
	delete this;
}
