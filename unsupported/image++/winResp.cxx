#include "main.h"
#include "winResp.h"

void singleWinResponder::init4(int argc,char **argv,int maxX,int maxY)
{
	responder::init2(argc,argv);
	win=createWindow(getWindowName(),maxX,maxY);
	menus=getMenus();
}
char *singleWinResponder::getWindowName(void)
{
	return "Default Window Name";
}
menuBlock * singleWinResponder::getMenus(void)
{
	menuBlock *block=new menuBlock;
	menuBlock *b2=new menuBlock;
	menuBlock *b3=new menuBlock;
	b3->init12(300,
		new menuItem("Not much this deep.",NULL)
	);
	b2->init12(200,
		new menuItem("Heirarchical, level 1", NULL),
		new menuItem("Heirarchical >",b3)
	);
	block->init12(100,
		new menuItem("About this app",NULL),
		new menuItem("About some other app",NULL),
		new menuItem("Heirarchy! >",b2),
		new menuItem("Dorky, eh?",NULL),
		new menuItem("Quit",NULL)
	);
	return block;
}
void singleWinResponder::mouseClick(Window w,int button,int x,int y)
{
	if (button==3)
	{
		menus->show(win->w,x,y);
	} else responder::mouseClick(w,button,x,y);
}
void singleWinResponder::mouseMove(Window w,int state,int x,int y)
{
	if (state&Button3Mask)
	{
		menus->mouseMove(x,y);
	} else responder::mouseClick(w,state,x,y);
}
void singleWinResponder::mouseRelease(Window w,int button,int x,int y)
{
	if (button==3)
	{
		menus->hide(this);
	} else responder::mouseRelease(w,button,x,y);
}
void singleWinResponder::drawWindow(Window w)
{
	menus->draw();
}
void singleWinResponder::resizeWindow(Window w,int newX,int newY)
{
	
}
void singleWinResponder::die(void)
{
	menus->die();
	win->die();
	responder::die();
}

