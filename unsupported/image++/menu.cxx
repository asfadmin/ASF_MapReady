#include <string.h>
#include "main.h"
#include "responder.h"
#include "menu.h"

const int itemHeight=25;
const int textStartY=20;
const int textStartX=5;

void menuBlock::init12(int NmenuNo,
		menuItem *item1,
		menuItem *item2,
		menuItem *item3,
		menuItem *item4,
		menuItem *item5,
		menuItem *item6,
		menuItem *item7,
		menuItem *item8,
		menuItem *item9,
		menuItem *item10,
		menuItem *item11,
		menuItem *item12)
{
	int itemNo;
	object::init0();
	menuNo=NmenuNo;
	item[0]=item1;
	item[1]=item2;
	item[2]=item3;
	item[3]=item4;
	item[4]=item5;
	item[5]=item6;
	item[6]=item7;
	item[7]=item8;
	item[8]=item9;
	item[9]=item10;
	item[10]=item11;
	item[11]=item12;
	numItems=12;
	for (itemNo=0;itemNo<12;itemNo++)
		if (NULL==item[itemNo])
		{
			numItems=itemNo;
			break;
		}
	frame.top=frame.left=0;
	wid=200;
	height=numItems*itemHeight+2;
	top=left=0;
	menuW=0;
	selectedItem=-1;
}
void menuBlock::show(Window w,int x,int y)
{
	XSetWindowAttributes attr;
	if (menuW)
		return;
	left=x;
	top=y;
	attr.save_under=True;
	menuW=XCreateWindow(display,w,left,top,
			wid, height,0,screenBits,
			InputOutput,visual,CWSaveUnder,&attr);
 	XMapWindow(display,menuW);
	draw();
}
Bool menuBlock::mouseMove(int x, int y)
{
        if (menuW)
	  {
	    int i;
	    x-=left;
	    y-=top;
	    for (i=0;i<numItems;i++)
	      if (item[i]->inSubMenu(x,y))
		return True;
	    if ((x<=0)||(x>=wid)||(y<1)||(y>=height-1))
	      {
		menuActivate(-1);
		return False;
	      }
	    else
	      {
		menuActivate((y-1)/itemHeight);
		return True;
	      }
	  } else return False;
}
void menuBlock::menuActivate(int newSelected)
{
	if (newSelected!=selectedItem)
	{
    	        if (selectedItem!=-1)
			item[selectedItem]->setHilite(menuW,False,selectedItem,wid);
		if (newSelected!=-1)
			item[newSelected]->setHilite(menuW,True,newSelected,wid);
		selectedItem=newSelected;
	}
}
void menuBlock::draw(void)
{
	if (menuW)
	{
		int i;
	/*Erase Background.*/
		XSetForeground(display,gc,WhitePixel);
		XFillRectangle(display,menuW,gc,0,0,wid,height);
		XSetForeground(display,gc,BlackPixel);
		XDrawRectangle(display,menuW,gc,0,0,wid-1,height-1);
	/*Draw Items.*/
		for (i=0;i<numItems;i++)
			item[i]->draw(menuW,i,wid);
	}
}
void menuBlock::hide(responder *destination)
{
	if (menuW)
	{
		if (selectedItem!=-1)
		{
			if (item[selectedItem]->block==NULL)
			{
				int i;
				for (i=0;i<2;i++)
				{
					item[selectedItem]->setHilite(menuW,False,selectedItem,wid);
					XSync(display,False);
					item[selectedItem]->setHilite(menuW,True,selectedItem,wid);
					XSync(display,False);
				}
				if (destination)
					destination->processMenuItem(menuNo,selectedItem+1);
			} else
				item[selectedItem]->block->hide(destination);
			item[selectedItem]->setHilite(menuW,False,selectedItem,wid);
			selectedItem=-1;
		}
		XDestroyWindow(display,menuW);
		menuW=0;
	}
}
void menuBlock::die(void)
{
	int itemNo;
	for (itemNo=0;itemNo<12;itemNo++)
		if (item[itemNo])
			item[itemNo]->die();
	if (menuW)
		hide(NULL);
	object::die();
}



menuItem::menuItem(const char *Nname,menuBlock *Nblock)
{
	block=Nblock;
	setName(Nname);
}
void menuItem::setName(const char *Nname)
{
	name=Nname;
	nameLen=strlen(name);
}
void menuItem::setHilite(Window w,Bool newHilite,int itemNumber,int menuWid)
{
	hilited=newHilite;
	if (hilited)
	{
		if (block)
			block->show(w,menuWid-100,itemNumber*itemHeight+10);
	} else if (block)
		block->hide(NULL);
	draw(w,itemNumber,menuWid);
}
void menuItem::draw(Window w,int itemNumber, int menuWid)
{
	if (hilited)
		XSetForeground(display,gc,BlackPixel);
	else 
		XSetForeground(display,gc,WhitePixel);
	XFillRectangle(display,w,gc,1,itemNumber*itemHeight+1,menuWid-2,itemHeight);
	if (hilited)
		XSetForeground(display,gc,WhitePixel);
	else 
		XSetForeground(display,gc,BlackPixel);
	XDrawString(display,w,gc,textStartX,itemNumber*itemHeight+textStartY,
		name,nameLen);
	XSetForeground(display,gc,BlackPixel);
	if (hilited&&block)
		block->draw();
}
Bool menuItem::inSubMenu(int x, int y)
{
	if (!block)
		return False;
	return block->mouseMove(x,y);
}
void menuItem::die(void)
{
	if (block!=NULL)
		block->die();
	object::die();
}
