! UIMX ascii 2.9 key: 3708                                                      

*APSFileSelection.class: fileSelectionBox
*APSFileSelection.classinc:
*APSFileSelection.classspec:
*APSFileSelection.classmembers:
*APSFileSelection.classconstructor:
*APSFileSelection.classdestructor:
*APSFileSelection.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
U.S. Government Sponsorship acknowledged.\
#endif\
 \
/*==============================================================================\
Filename:\
 \
Description:\
 \
External Functions Defined:\
 \
File Scope Functions:\
 \
External Variables Defined:\
 \
File Scope Variables:\
 \
Notes:\
 \
==============================================================================*/\
#pragma ident   "@(#)APSFileSelection.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.APSFileSelection.i"\

*APSFileSelection.ispecdecl:
*APSFileSelection.funcdecl: swidget create_APSFileSelection(swidget UxParent)
*APSFileSelection.funcname: create_APSFileSelection
*APSFileSelection.funcdef: "swidget", "<create_APSFileSelection>(%)"
*APSFileSelection.argdecl: swidget UxParent;
*APSFileSelection.arglist: UxParent
*APSFileSelection.arglist.UxParent: "swidget", "%UxParent%"
*APSFileSelection.icode:
*APSFileSelection.fcode: XtSetSensitive(\
XmFileSelectionBoxGetChild(rtrn, XmDIALOG_HELP_BUTTON), False) ;\
return(rtrn) ;\

*APSFileSelection.auxdecl:
*APSFileSelection.static: true
*APSFileSelection.name: APSFileSelection
*APSFileSelection.parent: NO_PARENT
*APSFileSelection.parentExpression: UxParent
*APSFileSelection.defaultShell: topLevelShell
*APSFileSelection.width: 405
*APSFileSelection.height: 395
*APSFileSelection.isCompound: "true"
*APSFileSelection.compoundIcon: "filebox.xpm"
*APSFileSelection.compoundName: "file_SelBox"
*APSFileSelection.x: 660
*APSFileSelection.y: 460
*APSFileSelection.unitType: "pixels"
*APSFileSelection.createCallback: {\
    XtSetSensitive (\
        XmFileSelectionBoxGetChild (UxWidget, XmDIALOG_HELP_BUTTON), False) ;\
\
}
*APSFileSelection.cancelCallback: {\
XtPopdown(XtParent(UxWidget)) ;\
\
}
*APSFileSelection.textString: "/ua/aps/rgreen/aps"

