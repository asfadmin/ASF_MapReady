! UIMX ascii 2.9 key: 754                                                       

*CPquestionBox.class: questionDialog
*CPquestionBox.classinc:
*CPquestionBox.classspec:
*CPquestionBox.classmembers:
*CPquestionBox.classconstructor:
*CPquestionBox.classdestructor:
*CPquestionBox.gbldecl: #include <stdio.h>\
static char sccsid_CPquestionBox_i[] = "@(#)CPquestionBox.i	2.7 96/04/10 20:02:32"; \
\
#ifndef DESIGN_TIME\
#include "que_xwp.h"\
#else\
typedef struct {\
  int action;\
  char *namePtr;\
  Widget buttonWid;\
  int pos;\
  int rev;\
  char platform[30];\
} dialogCBdata;\
#endif\
extern dialogCBdata *doCreateQuestionDlg(Widget wid, dialogCBdata *cbData);\

*CPquestionBox.ispecdecl:
*CPquestionBox.funcdecl: swidget create_CPquestionBox(UxParent, QuestionCBdata)\
swidget UxParent;\
dialogCBdata *QuestionCBdata;\

*CPquestionBox.funcname: create_CPquestionBox
*CPquestionBox.funcdef: "swidget", "<create_CPquestionBox>(%)"
*CPquestionBox.argdecl: swidget UxParent;\
dialogCBdata *QuestionCBdata;
*CPquestionBox.arglist: UxParent, QuestionCBdata
*CPquestionBox.arglist.UxParent: "swidget", "%UxParent%"
*CPquestionBox.arglist.QuestionCBdata: "dialogCBdata", "*%QuestionCBdata%"
*CPquestionBox.icode: dialogCBdata *questionCBdata;\

*CPquestionBox.fcode: #ifndef DESIGN_TIME\
QuestionCBdata = doCreateQuestionDlg(rtrn, QuestionCBdata);\
#endif\
return(rtrn);\

*CPquestionBox.auxdecl:
*CPquestionBox.name.source: public
*CPquestionBox.static: false
*CPquestionBox.name: CPquestionBox
*CPquestionBox.parent: NO_PARENT
*CPquestionBox.parentExpression: UxParent
*CPquestionBox.defaultShell: topLevelShell
*CPquestionBox.msgDialogType: "dialog_question"
*CPquestionBox.height: 162
*CPquestionBox.isCompound: "true"
*CPquestionBox.compoundIcon: "questionD.xpm"
*CPquestionBox.compoundName: "question_Box"
*CPquestionBox.x: 125
*CPquestionBox.y: 368
*CPquestionBox.unitType: "pixels"
*CPquestionBox.dialogTitle: "Confirmation"
*CPquestionBox.messageString: "default message string"
*CPquestionBox.defaultPosition: "false"

