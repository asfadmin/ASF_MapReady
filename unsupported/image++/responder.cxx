#include "main.h"
#include "responder.h"

void responder::init2(int argc,char **argv)
{
	object::init0();
}
void responder::mouseClick(Window w,int button,int x,int y) {}
void responder::mouseMove(Window w,int state,int x,int y) {}
void responder::mouseRelease(Window w,int button,int x,int y) {}
void responder::doIdle(void) {}
void responder::processMenuItem(int menu,int item)
{
	printf("You chose menu #%i, item #%i.  This wasn't trapped, however.\n",menu,item);
}
void responder::die(void)
{
	object::die();
}
