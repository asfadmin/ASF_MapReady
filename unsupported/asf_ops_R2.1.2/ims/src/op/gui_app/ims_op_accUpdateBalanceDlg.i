! UIMX ascii 2.9 key: 4751                                                      

*update_balance_dlg.class: promptDialog
*update_balance_dlg.classinc:
*update_balance_dlg.classspec:
*update_balance_dlg.classmembers:
*update_balance_dlg.classconstructor:
*update_balance_dlg.classdestructor:
*update_balance_dlg.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>
*update_balance_dlg.ispecdecl:
*update_balance_dlg.funcdecl: /* account_param : account_id; begin_param:begin_balance; */\
/* current_param : current_balance ; hold_param : on_hold */\
swidget create_update_balance_dlg(swidget UxParent, Widget account_param,\
                                   Widget begin_param, Widget current_param,\
                                   Widget hold_param )
*update_balance_dlg.funcname: create_update_balance_dlg
*update_balance_dlg.funcdef: "swidget", "<create_update_balance_dlg>(%)"
*update_balance_dlg.argdecl: swidget UxParent;\
Widget account_param;\
Widget begin_param;\
Widget current_param;\
Widget hold_param;
*update_balance_dlg.arglist: UxParent, account_param, begin_param, current_param, hold_param
*update_balance_dlg.arglist.UxParent: "swidget", "%UxParent%"
*update_balance_dlg.arglist.account_param: "Widget", "%account_param%"
*update_balance_dlg.arglist.begin_param: "Widget", "%begin_param%"
*update_balance_dlg.arglist.current_param: "Widget", "%current_param%"
*update_balance_dlg.arglist.hold_param: "Widget", "%hold_param%"
*update_balance_dlg.icode:
*update_balance_dlg.fcode: return(rtrn);\

*update_balance_dlg.auxdecl:
*update_balance_dlg.name.source: public
*update_balance_dlg.static: false
*update_balance_dlg.name: update_balance_dlg
*update_balance_dlg.parent: NO_PARENT
*update_balance_dlg.parentExpression: UxParent
*update_balance_dlg.defaultShell: transientShell
*update_balance_dlg.dialogType: "dialog_prompt"
*update_balance_dlg.isCompound: "true"
*update_balance_dlg.compoundIcon: "promptD.xpm"
*update_balance_dlg.compoundName: "prompt_Dialog"
*update_balance_dlg.x: 331
*update_balance_dlg.y: 345
*update_balance_dlg.unitType: "pixels"
*update_balance_dlg.background: "#9ac0cd"
*update_balance_dlg.applyLabelString: "Apply"
*update_balance_dlg.buttonFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*update_balance_dlg.dialogStyle: "dialog_full_application_modal"
*update_balance_dlg.labelFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*update_balance_dlg.resizePolicy: "resize_none"
*update_balance_dlg.selectionLabelString: "Enter the amount to be added to\nBegin Balance and Current Balance:"
*update_balance_dlg.dialogTitle: "Update Balance"
*update_balance_dlg.textColumns: 20
*update_balance_dlg.textFontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*update_balance_dlg.topShadowColor: "#d8e7ec"
*update_balance_dlg.okCallback.source: public
*update_balance_dlg.okCallback: update_balance_dlg_okCb
*update_balance_dlg.cancelCallback.source: public
*update_balance_dlg.cancelCallback: update_balance_dlg_okCb
*update_balance_dlg.traversalOn: "true"
*update_balance_dlg.navigationType: "tab_group"
*update_balance_dlg.defaultPosition: "true"
*update_balance_dlg.sensitive: "true"
*update_balance_dlg.createCallback.source: public
*update_balance_dlg.createCallback: update_balance_dlg_okCb
*update_balance_dlg.createCallbackClientData: (XtPointer) 1
*update_balance_dlg.okCallbackClientData: (XtPointer) 0
*update_balance_dlg.autoUnmanage: "false"
*update_balance_dlg.cancelCallbackClientData: (XtPointer) 2

