! UIMX ascii 2.9 key: 5876                                                      

*FileViewer.class: form
*FileViewer.classinc:
*FileViewer.classspec:
*FileViewer.classmembers:
*FileViewer.classconstructor:
*FileViewer.classdestructor:
*FileViewer.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)FileViewer.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.FileViewer.i"\

*FileViewer.ispecdecl:
*FileViewer.funcdecl: swidget create_FileViewer(UxParent)\
swidget UxParent;
*FileViewer.funcname: create_FileViewer
*FileViewer.funcdef: "swidget", "<create_FileViewer>(%)"
*FileViewer.argdecl: swidget UxParent;
*FileViewer.arglist: UxParent
*FileViewer.arglist.UxParent: "swidget", "%UxParent%"
*FileViewer.icode:
*FileViewer.fcode: return(rtrn);\

*FileViewer.auxdecl:
*FileViewer.static: true
*FileViewer.name: FileViewer
*FileViewer.parent: NO_PARENT
*FileViewer.parentExpression: UxParent
*FileViewer.defaultShell: topLevelShell
*FileViewer.width: 570
*FileViewer.height: 280
*FileViewer.resizePolicy: "resize_none"
*FileViewer.isCompound: "true"
*FileViewer.compoundIcon: "form.xpm"
*FileViewer.compoundName: "form_"
*FileViewer.x: 320
*FileViewer.y: 170
*FileViewer.unitType: "pixels"

*scrolledWindowFileText.class: scrolledWindow
*scrolledWindowFileText.static: true
*scrolledWindowFileText.name: scrolledWindowFileText
*scrolledWindowFileText.parent: FileViewer
*scrolledWindowFileText.scrollingPolicy: "application_defined"
*scrolledWindowFileText.visualPolicy: "variable"
*scrolledWindowFileText.scrollBarDisplayPolicy: "static"
*scrolledWindowFileText.isCompound: "true"
*scrolledWindowFileText.compoundIcon: "scrltext.xpm"
*scrolledWindowFileText.compoundName: "scrolled_Text"
*scrolledWindowFileText.x: 10
*scrolledWindowFileText.y: 20
*scrolledWindowFileText.height: 200
*scrolledWindowFileText.width: 550

*scrolledText_messages.class: scrolledText
*scrolledText_messages.static: true
*scrolledText_messages.name: scrolledText_messages
*scrolledText_messages.parent: scrolledWindowFileText
*scrolledText_messages.width: 530
*scrolledText_messages.height: 230
*scrolledText_messages.editMode: "multi_line_edit"
*scrolledText_messages.editable: "false"
*scrolledText_messages.wordWrap: "true"

*pushButton30.class: pushButton
*pushButton30.static: true
*pushButton30.name: pushButton30
*pushButton30.parent: FileViewer
*pushButton30.x: 223
*pushButton30.y: 234
*pushButton30.width: 115
*pushButton30.height: 30
*pushButton30.labelString: "QUIT"
*pushButton30.fontList: "h12bluci"
*pushButton30.activateCallback: {\
UxPopdownInterface( UxThisWidget );\
}

