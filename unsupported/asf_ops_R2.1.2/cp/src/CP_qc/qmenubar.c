/* Setting up the menu bar for the quality analysis app */

#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qmenubar_c[] = "@(#)qmenubar.c	1.18 96/03/11 13:23:38";

/* This sets up the layout and adds callbacks for the menubar */

Widget
set_up_menubar(parent_form)
   Widget parent_form;
{

   Widget   menubar_widget, 
            local_generic_widget ;
 
   XmString file, help, quit, prodInfo, overview;
 
   /* Set up the menu bar: */
   file = XmStringCreateLocalized("File");
   help = XmStringCreateLocalized("Help");
   overview = XmStringCreateLocalized("Overview");
   prodInfo = XmStringCreateLocalized("Product Information");
 
   menubar_widget = XmVaCreateSimpleMenuBar(parent_form, "menubar",
 				   XmVaCASCADEBUTTON,    file, 'F',
 				   XmVaCASCADEBUTTON,    help, 'H',
 				   XmNleftAttachment,    XmATTACH_FORM,
 				   XmNrightAttachment,   XmATTACH_FORM,
 				   NULL);
   XmStringFree(file);
 
   /* Tell the menubar which button is the help menu  */
   if (local_generic_widget = XtNameToWidget(menubar_widget, "button_1"))
      XtVaSetValues(menubar_widget, XmNmenuHelpWidget, 
 		   local_generic_widget, NULL);
 
   /* First menu is the File menu -- callback is file_cb() */
   quit = XmStringCreateLocalized("Exit");
   XmVaCreateSimplePulldownMenu(menubar_widget, "file_menu", 0, file_cb,
 			XmVaPUSHBUTTON, quit, 'x', NULL, NULL, NULL);
   XmStringFree(quit);
 
   /* Second menu is the help menu -- callback is help_cb() */
   XmVaCreateSimplePulldownMenu(menubar_widget, "help_menu", 1, help_cb,
 				XmVaPUSHBUTTON, overview, 'O', NULL, NULL, 
 				XmVaPUSHBUTTON, prodInfo, 'P', NULL, NULL, NULL);
   XmStringFree(help); 
   XmStringFree(overview);
   XmStringFree(prodInfo);
 
   return menubar_widget;
}
