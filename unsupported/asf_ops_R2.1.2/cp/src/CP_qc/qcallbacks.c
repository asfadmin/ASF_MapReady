/* the callbacks for the quality app */

static char sccsid_qcallbacks_c[] = "@(#)qcallbacks.c	1.21 96/12/30 15:24:56";

#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */
#include "qversion.h"

/* This is the callback for the file option on the menubar */

void
file_cb(widget, client_data, call_data)
   Widget widget;     	   /* menu item that was selected */
   XtPointer client_data;  /* the index into the menu */
   XtPointer call_data;	   /* unused */
{
   static   Widget dialog; /* make it static for reuse */
   extern   void load_pixmap();
   int      item_no = (int) client_data;

   /* the "quit" item */
   if (item_no == 0) {
      char fred[333];
      sprintf(fred, "Assigned status HOLD to image %s", _filename);
      make_announcement(fred, INFO);
      exit(QC_HOLD);
   }
    
#if 0
    /* "Open" was selected.  Create a Motif FileSelectionDialog w/callback */
    if (!dialog) {
       dialog = XmCreateFileSelectionDialog(_toplevel, "file_sel", NULL, 0);
       XtAddCallback(dialog, XmNokCallback, load_pixmap, NULL);
       XtAddCallback(dialog, XmNcancelCallback, XtUnmanageChild, NULL);
    }
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
#endif
}

#define OVERVIEW \
"                   QC Overview\n\
\n\
This application contains two major parts. The first\n\
one is the image file which located at the left part\n\
of the display.\n\
\n\
The loaded image is either scaled down \(for images\n\
whose number of lines per channel is greater than or \n\
equal to 1024 lines\), or displayed as it is in the \n\
CEOS file.\n\
\n\
If the image has been scaled down, click on the 'zoom in'\n\
button \(in the lower right corner\) to increase the\n\
maginfication for a subset of, or all of, the image.\n\
Then click on the 'zoom out' button to restore the image.\n\
\n\
If the full size of the image in the y dimension is\n\
exactly 1024, then the zooming will happen automagically.\n\
Otherwise, move the pointer into the image itself, and a\n\
white outline of a box should appear, representing a\n\
1024x1024 subset of the image.  Click the left mouse\n\
button in any part of the image to zoom into that subset.\n\
\n\
The second part is the leader file which located at the\n\
right part of the display. On the top it summarized the\n\
parameters of the image from the leader file. Then it shows\n\
the processed data histogram and its associated data values \n\
represent either the pixel values contained within\n\
the clip window \(the visible portion of image, beneath\n\
the scroll bars\) for full-resolution image, or for\n\
the entire image, for complex image, or low-resolution image.\n\
\n\
The high strench buttons and low stretch buttons are \n\
following to do the large scale or small scale adjustment.\n\
The 'Zoom in', 'Zoom out' button follows by 'Accept',\n\
'Hold', and 'Reject' bottons.\n\
\n\
If the leader file contains the signal data histogram \n\
record, then the real part (I) and imaginary part (Q) of \n\
the signal data record will be displayed with their \n\
histograms and associated data in the bottom.\n\
"


#define PROD_INFO \
"        qc version CP_QC_VERSION        \n\
\n\
"

#define AUTHOR \
"            About the Author:\n\
\n\
Dan Fineman \(\"Daniel\", but only his mother calls him\n\
that\) wrote this app between 6/94 and 12/94 at the\n\
Jet Propultion Lab for his first co-op tour of duty.\n\
\n\
During that time, his 20th birthday was\n\
celebrated, he owned his first car \(\"Hahahah!  Nobody\n\
*walks* in LA\!\" -- Steve Martin\), he applied to Reed\n\
College in Portland, and he planned a long trip to Europe,\n\
with plans to search for the holy grail.\n\
\n\
Educationally speaking, he was a CS undergrad at the\n\
University of Washington, up in Seattle, Washington.  \n\
If you'd like to e-mail your questions, comments, \n\
or praises, or if you'd like to inquire about\n\
contributing to the latte fund, try sending mail to:\n\
\n\
          fineman@u.washington.edu\n\
\n\
That address should be valid until roughly 6/97.\n\
\n\
                               Dan\n\
"

/* 
 * The help button in the help menu from the menubar was selected.
 * Display help information defined above for how to use the program.
 * This is done by creating a Motif information dialog box.  Again,
 * make the dialog static so we can reuse it.
 */
void
help_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   int item_no = (int) client_data;
   Widget dialog;
   Arg args[5];
   int n = 0;
      
  
   if (item_no == 0)        /* the "help overview" item */ {
      XmString msg = XmStringCreateLtoR(OVERVIEW, XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNmessageString, msg); n++;
      
      dialog = XmCreateInformationDialog(_toplevel, "help_dialog", args, n);
   }
   else if (item_no == 1) { /* the "product information" item */
      char newstring[222];
      XmString msg;

      sprintf (newstring, "        CP_qc version %s        \n", CP_QC_VERSION);
      msg = XmStringCreateLtoR(newstring, XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNmessageString, msg); n++;
      dialog = XmCreateInformationDialog(_toplevel, "message_dialog", args, n);
      XtVaSetValues(dialog, XmNtitle, "self-embellishment", NULL);
   }
  
   XtManageChild(dialog);
   XtPopup(XtParent(dialog), XtGrabNone);
  
   XSetWindowColormap(_display_pointer, XtWindow(XtParent(dialog)),
		      XDefaultColormap(_display_pointer, _screen_number));
}

/* The accept button's callback */
/* Exit with a status defined in qdefines.h */

void
accept_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   char  fred[333];

   sprintf(fred, "Assigned status ACCEPT to image %s", _filename);
   make_announcement(fred, INFO);
   exit(QC_ACCEPT);
}


/* The reject button's callback */
/* Exit with a status defined in qdefines.h */
void
reject_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   char  fred[333];

   sprintf(fred, "Assigned status REJECT to image %s", _filename);
   make_announcement(fred, INFO);
   exit(QC_REJECT);
}


/* The hold button's callback */
/* Exit with a status defined in qdefines.h */
void
hold_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   char  fred[333];

   sprintf(fred, "Assigned status HOLD to image %s", _filename);
   make_announcement(fred, INFO);
   exit(QC_HOLD);
}
