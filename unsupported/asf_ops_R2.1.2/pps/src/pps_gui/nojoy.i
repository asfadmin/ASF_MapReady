! UIMX ascii 2.9 key: 2253                                                      

*nojoy.class: warningDialog
*nojoy.classinc:
*nojoy.classspec:
*nojoy.classmembers:
*nojoy.classconstructor:
*nojoy.classdestructor:
*nojoy.gbldecl: #include <stdio.h>\
#include "pps_common.h"\
#include "pps_util.h"\
char nojoy_labelPixmapString[MAXSMALLBUF];\
extern char rootPath[];\
static char SccsFileId[] = "@(#)nojoy.i	1.1    11/21/96";
*nojoy.ispecdecl:
*nojoy.funcdecl: swidget create_nojoy(swidget UxParent)
*nojoy.funcname: create_nojoy
*nojoy.funcdef: "swidget", "<create_nojoy>(%)"
*nojoy.argdecl: swidget UxParent;
*nojoy.arglist: UxParent
*nojoy.arglist.UxParent: "swidget", "%UxParent%"
*nojoy.icode: (void)sprintf(nojoy_labelPixmapString, "%s/%s/tigerfrown.xpm",\
rootPath, PPS_PIXMAP_SUBPATH);
*nojoy.fcode: return(rtrn);\

*nojoy.auxdecl:
*nojoy.name.source: public
*nojoy.static: false
*nojoy.name: nojoy
*nojoy.parent: NO_PARENT
*nojoy.parentExpression: UxParent
*nojoy.defaultShell: transientShell
*nojoy.msgDialogType: "dialog_error"
*nojoy.width: 324
*nojoy.height: 172
*nojoy.isCompound: "true"
*nojoy.compoundIcon: "warningD.xpm"
*nojoy.compoundName: "warning_Dialog"
*nojoy.x: 388
*nojoy.y: 360
*nojoy.unitType: "pixels"
*nojoy.messageString: "No joy here!\n\nTry something else."
*nojoy.cancelCallback: UxPopdownInterface(nojoy);
*nojoy.helpCallback: UxPopdownInterface(nojoy);
*nojoy.okCallback: UxPopdownInterface(nojoy);
*nojoy.symbolPixmap: nojoy_labelPixmapString
*nojoy.dialogStyle: "dialog_full_application_modal"

