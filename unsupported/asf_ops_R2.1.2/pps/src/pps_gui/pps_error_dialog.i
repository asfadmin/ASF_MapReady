! UIMX ascii 2.9 key: 5539                                                      

*pps_error_dialog.class: errorDialog
*pps_error_dialog.classinc:
*pps_error_dialog.classspec:
*pps_error_dialog.classmembers:
*pps_error_dialog.classconstructor:
*pps_error_dialog.classdestructor:
*pps_error_dialog.gbldecl: #include <stdio.h>\
#include "pps_common.h"\
#include "pps_util.h"\
char error_labelPixmapString[MAXSMALLBUF];\
extern char rootPath[];\
static char SccsFileId[] = "@(#)pps_error_dialog.i	1.1    11/21/96";
*pps_error_dialog.ispecdecl:
*pps_error_dialog.funcdecl: swidget create_pps_error_dialog(swidget UxParent)
*pps_error_dialog.funcname: create_pps_error_dialog
*pps_error_dialog.funcdef: "swidget", "<create_pps_error_dialog>(%)"
*pps_error_dialog.argdecl: swidget UxParent;
*pps_error_dialog.arglist: UxParent
*pps_error_dialog.arglist.UxParent: "swidget", "%UxParent%"
*pps_error_dialog.icode: (void)sprintf(error_labelPixmapString,\
"%s/%s/bearwait2.xpm", rootPath, PPS_PIXMAP_SUBPATH);
*pps_error_dialog.fcode: return(rtrn);\

*pps_error_dialog.auxdecl:
*pps_error_dialog.name.source: public
*pps_error_dialog.static: false
*pps_error_dialog.name: pps_error_dialog
*pps_error_dialog.parent: NO_PARENT
*pps_error_dialog.parentExpression: UxParent
*pps_error_dialog.defaultShell: topLevelShell
*pps_error_dialog.msgDialogType: "dialog_error"
*pps_error_dialog.width: 483
*pps_error_dialog.height: 211
*pps_error_dialog.isCompound: "true"
*pps_error_dialog.compoundIcon: "errorD.xpm"
*pps_error_dialog.compoundName: "error_Dialog"
*pps_error_dialog.x: 427
*pps_error_dialog.y: 311
*pps_error_dialog.unitType: "pixels"
*pps_error_dialog.marginHeight: 10
*pps_error_dialog.messageAlignment: "alignment_center"
*pps_error_dialog.symbolPixmap: error_labelPixmapString
*pps_error_dialog.dialogStyle: "dialog_full_application_modal"
*pps_error_dialog.allowOverlap: "false"

