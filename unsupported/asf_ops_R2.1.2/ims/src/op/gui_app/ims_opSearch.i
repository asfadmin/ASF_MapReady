! UIMX ascii 2.9 key: 5830                                                      

*search.class: form
*search.classinc:
*search.classspec:
*search.classmembers:
*search.classconstructor:
*search.classdestructor:
*search.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*search.ispecdecl:
*search.funcdecl: swidget create_search(swidget UxParent)
*search.funcname: create_search
*search.funcdef: "swidget", "<create_search>(%)"
*search.argdecl: swidget UxParent;
*search.arglist: UxParent
*search.arglist.UxParent: "swidget", "%UxParent%"
*search.icode:
*search.fcode: return(rtrn);\

*search.auxdecl:
*search.static: true
*search.name: search
*search.parent: NO_PARENT
*search.parentExpression: UxParent
*search.defaultShell: transientShell
*search.width: 1080
*search.height: 772
*search.resizePolicy: "resize_none"
*search.isCompound: "true"
*search.compoundIcon: "form.xpm"
*search.compoundName: "form_"
*search.x: 28
*search.y: 56
*search.unitType: "pixels"
*search.allowShellResize: "false"
*search.background: "#9ac0cd"
*search.dialogTitle: "Order Search Screen"
*search.noResize: "true"

*srchOrderSearchLB.class: label
*srchOrderSearchLB.static: true
*srchOrderSearchLB.name: srchOrderSearchLB
*srchOrderSearchLB.parent: search
*srchOrderSearchLB.isCompound: "true"
*srchOrderSearchLB.compoundIcon: "label.xpm"
*srchOrderSearchLB.compoundName: "label_"
*srchOrderSearchLB.x: 415
*srchOrderSearchLB.y: 37
*srchOrderSearchLB.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*srchOrderSearchLB.labelString: "Order     Search     Screen"
*srchOrderSearchLB.background: "#9ac0cd"
*srchOrderSearchLB.height: 28

*srchUserIdLB.class: label
*srchUserIdLB.static: true
*srchUserIdLB.name: srchUserIdLB
*srchUserIdLB.parent: search
*srchUserIdLB.isCompound: "true"
*srchUserIdLB.compoundIcon: "label.xpm"
*srchUserIdLB.compoundName: "label_"
*srchUserIdLB.x: 729
*srchUserIdLB.y: 97
*srchUserIdLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchUserIdLB.labelString: "User ID:"
*srchUserIdLB.background: "#9ac0cd"

*srchUserNameLB.class: label
*srchUserNameLB.static: true
*srchUserNameLB.name: srchUserNameLB
*srchUserNameLB.parent: search
*srchUserNameLB.isCompound: "true"
*srchUserNameLB.compoundIcon: "label.xpm"
*srchUserNameLB.compoundName: "label_"
*srchUserNameLB.x: 22
*srchUserNameLB.y: 514
*srchUserNameLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchUserNameLB.labelString: "User Name:"
*srchUserNameLB.background: "#9ac0cd"

*srchOrderReceivedFrame.class: frame
*srchOrderReceivedFrame.static: true
*srchOrderReceivedFrame.name: srchOrderReceivedFrame
*srchOrderReceivedFrame.parent: search
*srchOrderReceivedFrame.width: 515
*srchOrderReceivedFrame.height: 46
*srchOrderReceivedFrame.isCompound: "true"
*srchOrderReceivedFrame.compoundIcon: "frame.xpm"
*srchOrderReceivedFrame.compoundName: "frame_"
*srchOrderReceivedFrame.x: 172
*srchOrderReceivedFrame.y: 600
*srchOrderReceivedFrame.shadowType: "shadow_in"
*srchOrderReceivedFrame.shadowThickness: 2

*srchOrderReceivedForm.class: form
*srchOrderReceivedForm.static: true
*srchOrderReceivedForm.name: srchOrderReceivedForm
*srchOrderReceivedForm.parent: srchOrderReceivedFrame
*srchOrderReceivedForm.width: 700
*srchOrderReceivedForm.height: 42
*srchOrderReceivedForm.resizePolicy: "resize_none"
*srchOrderReceivedForm.isCompound: "true"
*srchOrderReceivedForm.compoundIcon: "form.xpm"
*srchOrderReceivedForm.compoundName: "form_"
*srchOrderReceivedForm.x: 2
*srchOrderReceivedForm.y: 2
*srchOrderReceivedForm.background: "#9ac0cd"

*srchOrderReceivedStartDateLB.class: label
*srchOrderReceivedStartDateLB.static: true
*srchOrderReceivedStartDateLB.name: srchOrderReceivedStartDateLB
*srchOrderReceivedStartDateLB.parent: srchOrderReceivedForm
*srchOrderReceivedStartDateLB.isCompound: "true"
*srchOrderReceivedStartDateLB.compoundIcon: "label.xpm"
*srchOrderReceivedStartDateLB.compoundName: "label_"
*srchOrderReceivedStartDateLB.x: 8
*srchOrderReceivedStartDateLB.y: 12
*srchOrderReceivedStartDateLB.labelString: "Start Date:"
*srchOrderReceivedStartDateLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderReceivedStartDateLB.width: 100
*srchOrderReceivedStartDateLB.background: "#9ac0cd"

*srchOrderReceivedEndDateLB.class: label
*srchOrderReceivedEndDateLB.static: true
*srchOrderReceivedEndDateLB.name: srchOrderReceivedEndDateLB
*srchOrderReceivedEndDateLB.parent: srchOrderReceivedForm
*srchOrderReceivedEndDateLB.isCompound: "true"
*srchOrderReceivedEndDateLB.compoundIcon: "label.xpm"
*srchOrderReceivedEndDateLB.compoundName: "label_"
*srchOrderReceivedEndDateLB.x: 256
*srchOrderReceivedEndDateLB.y: 12
*srchOrderReceivedEndDateLB.labelString: "End Date:"
*srchOrderReceivedEndDateLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderReceivedEndDateLB.width: 100
*srchOrderReceivedEndDateLB.background: "#9ac0cd"

*ordrRecStartDateText.class: text
*ordrRecStartDateText.static: true
*ordrRecStartDateText.name: ordrRecStartDateText
*ordrRecStartDateText.parent: srchOrderReceivedForm
*ordrRecStartDateText.width: 140
*ordrRecStartDateText.isCompound: "true"
*ordrRecStartDateText.compoundIcon: "text.xpm"
*ordrRecStartDateText.compoundName: "text_"
*ordrRecStartDateText.x: 112
*ordrRecStartDateText.y: 4
*ordrRecStartDateText.background: "LightSkyBlue3"
*ordrRecStartDateText.height: 36
*ordrRecStartDateText.modifyVerifyCallback.source: public
*ordrRecStartDateText.modifyVerifyCallback: search_check_date
*ordrRecStartDateText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ordrRecStartDateText.motionVerifyCallback.source: public
*ordrRecStartDateText.motionVerifyCallback: search_check_date
*ordrRecStartDateText.losingFocusCallback.source: public
*ordrRecStartDateText.losingFocusCallback: search_date_loseFocusCb

*ordrRecEndDateText.class: text
*ordrRecEndDateText.static: true
*ordrRecEndDateText.name: ordrRecEndDateText
*ordrRecEndDateText.parent: srchOrderReceivedForm
*ordrRecEndDateText.width: 140
*ordrRecEndDateText.isCompound: "true"
*ordrRecEndDateText.compoundIcon: "text.xpm"
*ordrRecEndDateText.compoundName: "text_"
*ordrRecEndDateText.x: 360
*ordrRecEndDateText.y: 4
*ordrRecEndDateText.background: "LightSkyBlue3"
*ordrRecEndDateText.height: 36
*ordrRecEndDateText.modifyVerifyCallback.source: public
*ordrRecEndDateText.modifyVerifyCallback: search_check_date
*ordrRecEndDateText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ordrRecEndDateText.motionVerifyCallback.source: public
*ordrRecEndDateText.motionVerifyCallback: search_check_date
*ordrRecEndDateText.losingFocusCallback.source: public
*ordrRecEndDateText.losingFocusCallback: search_date_loseFocusCb

*srchOrderCompletedLB.class: label
*srchOrderCompletedLB.static: true
*srchOrderCompletedLB.name: srchOrderCompletedLB
*srchOrderCompletedLB.parent: search
*srchOrderCompletedLB.isCompound: "true"
*srchOrderCompletedLB.compoundIcon: "label.xpm"
*srchOrderCompletedLB.compoundName: "label_"
*srchOrderCompletedLB.x: 22
*srchOrderCompletedLB.y: 674
*srchOrderCompletedLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderCompletedLB.labelString: "Order Completed:"
*srchOrderCompletedLB.background: "#9ac0cd"

*srchOrderCompletedFrame.class: frame
*srchOrderCompletedFrame.static: true
*srchOrderCompletedFrame.name: srchOrderCompletedFrame
*srchOrderCompletedFrame.parent: search
*srchOrderCompletedFrame.width: 515
*srchOrderCompletedFrame.height: 46
*srchOrderCompletedFrame.isCompound: "true"
*srchOrderCompletedFrame.compoundIcon: "frame.xpm"
*srchOrderCompletedFrame.compoundName: "frame_"
*srchOrderCompletedFrame.x: 172
*srchOrderCompletedFrame.y: 660
*srchOrderCompletedFrame.shadowType: "shadow_in"

*srchOrderCompletedForm.class: form
*srchOrderCompletedForm.static: true
*srchOrderCompletedForm.name: srchOrderCompletedForm
*srchOrderCompletedForm.parent: srchOrderCompletedFrame
*srchOrderCompletedForm.width: 510
*srchOrderCompletedForm.height: 42
*srchOrderCompletedForm.resizePolicy: "resize_none"
*srchOrderCompletedForm.isCompound: "true"
*srchOrderCompletedForm.compoundIcon: "form.xpm"
*srchOrderCompletedForm.compoundName: "form_"
*srchOrderCompletedForm.x: -12
*srchOrderCompletedForm.y: 2
*srchOrderCompletedForm.background: "#9ac0cd"

*srchOrderCompletedStartDateLB.class: label
*srchOrderCompletedStartDateLB.static: true
*srchOrderCompletedStartDateLB.name: srchOrderCompletedStartDateLB
*srchOrderCompletedStartDateLB.parent: srchOrderCompletedForm
*srchOrderCompletedStartDateLB.isCompound: "true"
*srchOrderCompletedStartDateLB.compoundIcon: "label.xpm"
*srchOrderCompletedStartDateLB.compoundName: "label_"
*srchOrderCompletedStartDateLB.x: 8
*srchOrderCompletedStartDateLB.y: 12
*srchOrderCompletedStartDateLB.labelString: "Start Date:"
*srchOrderCompletedStartDateLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderCompletedStartDateLB.width: 100
*srchOrderCompletedStartDateLB.background: "#9ac0cd"

*srchOrderCompletedEndDateLB.class: label
*srchOrderCompletedEndDateLB.static: true
*srchOrderCompletedEndDateLB.name: srchOrderCompletedEndDateLB
*srchOrderCompletedEndDateLB.parent: srchOrderCompletedForm
*srchOrderCompletedEndDateLB.isCompound: "true"
*srchOrderCompletedEndDateLB.compoundIcon: "label.xpm"
*srchOrderCompletedEndDateLB.compoundName: "label_"
*srchOrderCompletedEndDateLB.x: 256
*srchOrderCompletedEndDateLB.y: 12
*srchOrderCompletedEndDateLB.labelString: "End Date:"
*srchOrderCompletedEndDateLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderCompletedEndDateLB.width: 100
*srchOrderCompletedEndDateLB.background: "#9ac0cd"

*ordrComEndDateText.class: text
*ordrComEndDateText.static: true
*ordrComEndDateText.name: ordrComEndDateText
*ordrComEndDateText.parent: srchOrderCompletedForm
*ordrComEndDateText.width: 140
*ordrComEndDateText.isCompound: "true"
*ordrComEndDateText.compoundIcon: "text.xpm"
*ordrComEndDateText.compoundName: "text_"
*ordrComEndDateText.x: 360
*ordrComEndDateText.y: 4
*ordrComEndDateText.background: "LightSkyBlue3"
*ordrComEndDateText.height: 36
*ordrComEndDateText.modifyVerifyCallback.source: public
*ordrComEndDateText.modifyVerifyCallback: search_check_date
*ordrComEndDateText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ordrComEndDateText.motionVerifyCallback.source: public
*ordrComEndDateText.motionVerifyCallback: search_check_date
*ordrComEndDateText.losingFocusCallback.source: public
*ordrComEndDateText.losingFocusCallback: search_date_loseFocusCb

*ordrComStartDateText.class: text
*ordrComStartDateText.static: true
*ordrComStartDateText.name: ordrComStartDateText
*ordrComStartDateText.parent: srchOrderCompletedForm
*ordrComStartDateText.width: 140
*ordrComStartDateText.isCompound: "true"
*ordrComStartDateText.compoundIcon: "text.xpm"
*ordrComStartDateText.compoundName: "text_"
*ordrComStartDateText.x: 112
*ordrComStartDateText.y: 4
*ordrComStartDateText.background: "LightSkyBlue3"
*ordrComStartDateText.height: 36
*ordrComStartDateText.modifyVerifyCallback.source: public
*ordrComStartDateText.modifyVerifyCallback: search_check_date
*ordrComStartDateText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ordrComStartDateText.motionVerifyCallback.source: public
*ordrComStartDateText.motionVerifyCallback: search_check_date
*ordrComStartDateText.losingFocusCallback.source: public
*ordrComStartDateText.losingFocusCallback: search_date_loseFocusCb

*srchUserNameFrame.class: frame
*srchUserNameFrame.static: true
*srchUserNameFrame.name: srchUserNameFrame
*srchUserNameFrame.parent: search
*srchUserNameFrame.width: 515
*srchUserNameFrame.height: 46
*srchUserNameFrame.isCompound: "true"
*srchUserNameFrame.compoundIcon: "frame.xpm"
*srchUserNameFrame.compoundName: "frame_"
*srchUserNameFrame.x: 172
*srchUserNameFrame.y: 500
*srchUserNameFrame.shadowType: "shadow_in"
*srchUserNameFrame.shadowThickness: 2

*srchUserNameForm.class: form
*srchUserNameForm.static: true
*srchUserNameForm.name: srchUserNameForm
*srchUserNameForm.parent: srchUserNameFrame
*srchUserNameForm.width: 518
*srchUserNameForm.height: 42
*srchUserNameForm.resizePolicy: "resize_none"
*srchUserNameForm.isCompound: "true"
*srchUserNameForm.compoundIcon: "form.xpm"
*srchUserNameForm.compoundName: "form_"
*srchUserNameForm.x: 2
*srchUserNameForm.y: 2
*srchUserNameForm.background: "#9ac0cd"

*srchLastNameLB.class: label
*srchLastNameLB.static: true
*srchLastNameLB.name: srchLastNameLB
*srchLastNameLB.parent: srchUserNameForm
*srchLastNameLB.isCompound: "true"
*srchLastNameLB.compoundIcon: "label.xpm"
*srchLastNameLB.compoundName: "label_"
*srchLastNameLB.x: 0
*srchLastNameLB.y: 12
*srchLastNameLB.labelString: "Last:"
*srchLastNameLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchLastNameLB.width: 44
*srchLastNameLB.background: "#9ac0cd"

*srchLastNameTF.class: textField
*srchLastNameTF.static: true
*srchLastNameTF.name: srchLastNameTF
*srchLastNameTF.parent: srchUserNameForm
*srchLastNameTF.width: 160
*srchLastNameTF.isCompound: "true"
*srchLastNameTF.compoundIcon: "textfield.xpm"
*srchLastNameTF.compoundName: "text_Field"
*srchLastNameTF.x: 44
*srchLastNameTF.y: 4
*srchLastNameTF.background: "LightSkyBlue3"
*srchLastNameTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchLastNameTF.text: ""
*srchLastNameTF.height: 36

*srchFirstNameLB.class: label
*srchFirstNameLB.static: true
*srchFirstNameLB.name: srchFirstNameLB
*srchFirstNameLB.parent: srchUserNameForm
*srchFirstNameLB.isCompound: "true"
*srchFirstNameLB.compoundIcon: "label.xpm"
*srchFirstNameLB.compoundName: "label_"
*srchFirstNameLB.x: 204
*srchFirstNameLB.y: 12
*srchFirstNameLB.labelString: "First:"
*srchFirstNameLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchFirstNameLB.width: 48
*srchFirstNameLB.background: "#9ac0cd"

*srchFirstNameTF.class: textField
*srchFirstNameTF.static: true
*srchFirstNameTF.name: srchFirstNameTF
*srchFirstNameTF.parent: srchUserNameForm
*srchFirstNameTF.width: 160
*srchFirstNameTF.isCompound: "true"
*srchFirstNameTF.compoundIcon: "textfield.xpm"
*srchFirstNameTF.compoundName: "text_Field"
*srchFirstNameTF.x: 252
*srchFirstNameTF.y: 4
*srchFirstNameTF.background: "LightSkyBlue3"
*srchFirstNameTF.height: 36
*srchFirstNameTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchFirstNameTF.text: ""

*srchFirstNameLB1.class: label
*srchFirstNameLB1.static: true
*srchFirstNameLB1.name: srchFirstNameLB1
*srchFirstNameLB1.parent: srchUserNameForm
*srchFirstNameLB1.isCompound: "true"
*srchFirstNameLB1.compoundIcon: "label.xpm"
*srchFirstNameLB1.compoundName: "label_"
*srchFirstNameLB1.x: 412
*srchFirstNameLB1.y: 12
*srchFirstNameLB1.labelString: "M.I."
*srchFirstNameLB1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchFirstNameLB1.width: 42
*srchFirstNameLB1.background: "#9ac0cd"

*middleNameTF.class: textField
*middleNameTF.static: true
*middleNameTF.name: middleNameTF
*middleNameTF.parent: srchUserNameForm
*middleNameTF.width: 44
*middleNameTF.isCompound: "true"
*middleNameTF.compoundIcon: "textfield.xpm"
*middleNameTF.compoundName: "text_Field"
*middleNameTF.x: 456
*middleNameTF.y: 4
*middleNameTF.background: "LightSkyBlue3"
*middleNameTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*srchItemStatusLB.class: label
*srchItemStatusLB.static: true
*srchItemStatusLB.name: srchItemStatusLB
*srchItemStatusLB.parent: search
*srchItemStatusLB.isCompound: "true"
*srchItemStatusLB.compoundIcon: "label.xpm"
*srchItemStatusLB.compoundName: "label_"
*srchItemStatusLB.x: 188
*srchItemStatusLB.y: 160
*srchItemStatusLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchItemStatusLB.labelString: "Item Status"
*srchItemStatusLB.background: "#9ac0cd"

*srchMediaTypeLB.class: label
*srchMediaTypeLB.static: true
*srchMediaTypeLB.name: srchMediaTypeLB
*srchMediaTypeLB.parent: search
*srchMediaTypeLB.isCompound: "true"
*srchMediaTypeLB.compoundIcon: "label.xpm"
*srchMediaTypeLB.compoundName: "label_"
*srchMediaTypeLB.x: 895
*srchMediaTypeLB.y: 160
*srchMediaTypeLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchMediaTypeLB.labelString: "Media Type"
*srchMediaTypeLB.background: "#9ac0cd"

*srchSeparator2.class: separator
*srchSeparator2.static: true
*srchSeparator2.name: srchSeparator2
*srchSeparator2.parent: search
*srchSeparator2.width: 1083
*srchSeparator2.height: 8
*srchSeparator2.isCompound: "true"
*srchSeparator2.compoundIcon: "sep.xpm"
*srchSeparator2.compoundName: "separator_"
*srchSeparator2.x: 0
*srchSeparator2.y: 432
*srchSeparator2.background: "#9ac0cd"
*srchSeparator2.separatorType: "shadow_etched_in"
*srchSeparator2.shadowThickness: 3

*srchProductionStatusLB.class: label
*srchProductionStatusLB.static: true
*srchProductionStatusLB.name: srchProductionStatusLB
*srchProductionStatusLB.parent: search
*srchProductionStatusLB.isCompound: "true"
*srchProductionStatusLB.compoundIcon: "label.xpm"
*srchProductionStatusLB.compoundName: "label_"
*srchProductionStatusLB.x: 501
*srchProductionStatusLB.y: 160
*srchProductionStatusLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchProductionStatusLB.labelString: "PPF Status"
*srchProductionStatusLB.background: "#9ac0cd"

*srchSeparator3.class: separator
*srchSeparator3.static: true
*srchSeparator3.name: srchSeparator3
*srchSeparator3.parent: search
*srchSeparator3.width: 1083
*srchSeparator3.height: 8
*srchSeparator3.isCompound: "true"
*srchSeparator3.compoundIcon: "sep.xpm"
*srchSeparator3.compoundName: "separator_"
*srchSeparator3.x: 0
*srchSeparator3.y: 716
*srchSeparator3.background: "#9ac0cd"
*srchSeparator3.separatorType: "shadow_etched_in"
*srchSeparator3.shadowThickness: 3

*srchExecuteSearchPB.class: pushButton
*srchExecuteSearchPB.static: true
*srchExecuteSearchPB.name: srchExecuteSearchPB
*srchExecuteSearchPB.parent: search
*srchExecuteSearchPB.isCompound: "true"
*srchExecuteSearchPB.compoundIcon: "push.xpm"
*srchExecuteSearchPB.compoundName: "push_Button"
*srchExecuteSearchPB.x: 44
*srchExecuteSearchPB.y: 728
*srchExecuteSearchPB.width: 192
*srchExecuteSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*srchExecuteSearchPB.labelString: " EXECUTE   SEARCH "
*srchExecuteSearchPB.height: 32
*srchExecuteSearchPB.background: "CadetBlue"
*srchExecuteSearchPB.shadowThickness: 4
*srchExecuteSearchPB.activateCallback.source: public
*srchExecuteSearchPB.activateCallback: search_executeCb

*menuBar1.class: rowColumn
*menuBar1.static: true
*menuBar1.name: menuBar1
*menuBar1.parent: search
*menuBar1.rowColumnType: "menu_bar"
*menuBar1.isCompound: "true"
*menuBar1.compoundIcon: "pulldownM.xpm"
*menuBar1.compoundName: "menu_Bar"
*menuBar1.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*menuBar1.x: 916
*menuBar1.y: 0
*menuBar1.background: "CadetBlue"
*menuBar1.height: 36
*menuBar1.rightAttachment: "attach_form"
*menuBar1.leftAttachment: "attach_form"
*menuBar1.menuAccelerator: "<KeyUp>F10"
*menuBar1.menuHelpWidget: "menuBar1_top_b2"

*menuBar_p1.class: rowColumn
*menuBar_p1.static: true
*menuBar_p1.name: menuBar_p1
*menuBar_p1.parent: menuBar1
*menuBar_p1.rowColumnType: "menu_pulldown"

*srchWelcomeMPB.class: pushButton
*srchWelcomeMPB.static: true
*srchWelcomeMPB.name: srchWelcomeMPB
*srchWelcomeMPB.parent: menuBar_p1
*srchWelcomeMPB.labelString: "Welcome Screen"
*srchWelcomeMPB.background: "CadetBlue"
*srchWelcomeMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchWelcomeMPB.mnemonic: "W"
*srchWelcomeMPB.activateCallback.source: public
*srchWelcomeMPB.activateCallback: search_goto_welcomeCb

*menuBar_p1_b2.class: separator
*menuBar_p1_b2.static: true
*menuBar_p1_b2.name: menuBar_p1_b2
*menuBar_p1_b2.parent: menuBar_p1

*gotoOrderMPB.class: pushButton
*gotoOrderMPB.static: true
*gotoOrderMPB.name: gotoOrderMPB
*gotoOrderMPB.parent: menuBar_p1
*gotoOrderMPB.labelString: "Order Production Screen"
*gotoOrderMPB.background: "cadetBlue"
*gotoOrderMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*gotoOrderMPB.mnemonic: "O"
*gotoOrderMPB.activateCallback.source: public
*gotoOrderMPB.activateCallback: search_goto_orderCb

*menuBar_p1_b4.class: separator
*menuBar_p1_b4.static: true
*menuBar_p1_b4.name: menuBar_p1_b4
*menuBar_p1_b4.parent: menuBar_p1

*menuBar_p1_b11.class: pushButton
*menuBar_p1_b11.static: true
*menuBar_p1_b11.name: menuBar_p1_b11
*menuBar_p1_b11.parent: menuBar_p1
*menuBar_p1_b11.labelString: "Close  Screen"
*menuBar_p1_b11.mnemonic: "C"
*menuBar_p1_b11.background: "cadetBlue"
*menuBar_p1_b11.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_p1_b11.activateCallback.source: public
*menuBar_p1_b11.activateCallback: search_closeCb

*menuBar1_p2.class: rowColumn
*menuBar1_p2.static: true
*menuBar1_p2.name: menuBar1_p2
*menuBar1_p2.parent: menuBar1
*menuBar1_p2.rowColumnType: "menu_pulldown"

*srchExecuteSearchMPB.class: pushButton
*srchExecuteSearchMPB.static: true
*srchExecuteSearchMPB.name: srchExecuteSearchMPB
*srchExecuteSearchMPB.parent: menuBar1_p2
*srchExecuteSearchMPB.labelString: "Execute Search"
*srchExecuteSearchMPB.mnemonic: "E"
*srchExecuteSearchMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchExecuteSearchMPB.background: "CadetBlue"
*srchExecuteSearchMPB.activateCallback.source: public
*srchExecuteSearchMPB.activateCallback: search_executeCb

*menuBar1_p2_b7.class: separator
*menuBar1_p2_b7.static: true
*menuBar1_p2_b7.name: menuBar1_p2_b7
*menuBar1_p2_b7.parent: menuBar1_p2
*menuBar1_p2_b7.background: "#7e88ab"

*srchClearSearchMPB.class: pushButton
*srchClearSearchMPB.static: true
*srchClearSearchMPB.name: srchClearSearchMPB
*srchClearSearchMPB.parent: menuBar1_p2
*srchClearSearchMPB.labelString: "Clear  Search"
*srchClearSearchMPB.mnemonic: "l"
*srchClearSearchMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchClearSearchMPB.background: "CadetBlue"
*srchClearSearchMPB.activateCallback.source: public
*srchClearSearchMPB.activateCallback: search_clearCb

*menuBar1_p2_b8.class: separator
*menuBar1_p2_b8.static: true
*menuBar1_p2_b8.name: menuBar1_p2_b8
*menuBar1_p2_b8.parent: menuBar1_p2

*srchPrintScreenMPB.class: pushButton
*srchPrintScreenMPB.static: true
*srchPrintScreenMPB.name: srchPrintScreenMPB
*srchPrintScreenMPB.parent: menuBar1_p2
*srchPrintScreenMPB.labelString: "Print  Screen"
*srchPrintScreenMPB.background: "cadetBlue"
*srchPrintScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchPrintScreenMPB.mnemonic: "P"
*srchPrintScreenMPB.activateCallback.source: public
*srchPrintScreenMPB.activateCallback: search_printScreenCb

*menuBar1_p3.class: rowColumn
*menuBar1_p3.static: true
*menuBar1_p3.name: menuBar1_p3
*menuBar1_p3.parent: menuBar1
*menuBar1_p3.rowColumnType: "menu_pulldown"

*menuBar1_p3_b1.class: pushButton
*menuBar1_p3_b1.static: true
*menuBar1_p3_b1.name: menuBar1_p3_b1
*menuBar1_p3_b1.parent: menuBar1_p3
*menuBar1_p3_b1.labelString: "No Help Available"
*menuBar1_p3_b1.background: "CadetBlue"

*menuBar_top_b1.class: cascadeButton
*menuBar_top_b1.static: true
*menuBar_top_b1.name: menuBar_top_b1
*menuBar_top_b1.parent: menuBar1
*menuBar_top_b1.labelString: "Go To"
*menuBar_top_b1.subMenuId: "menuBar_p1"
*menuBar_top_b1.mnemonic: "G"
*menuBar_top_b1.background: "CadetBlue"
*menuBar_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_top_b1.marginWidth: 10

*menuBar1_top_b1.class: cascadeButtonGadget
*menuBar1_top_b1.static: true
*menuBar1_top_b1.name: menuBar1_top_b1
*menuBar1_top_b1.parent: menuBar1
*menuBar1_top_b1.labelString: "Screen Functions"
*menuBar1_top_b1.mnemonic: "S"
*menuBar1_top_b1.subMenuId: "menuBar1_p2"
*menuBar1_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b1.marginWidth: 20

*menuBar1_top_b2.class: cascadeButton
*menuBar1_top_b2.static: true
*menuBar1_top_b2.name: menuBar1_top_b2
*menuBar1_top_b2.parent: menuBar1
*menuBar1_top_b2.labelString: "Help"
*menuBar1_top_b2.mnemonic: "H"
*menuBar1_top_b2.subMenuId: "menuBar1_p3"
*menuBar1_top_b2.background: "CadetBlue"
*menuBar1_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*srchOrderIdLB.class: label
*srchOrderIdLB.static: true
*srchOrderIdLB.name: srchOrderIdLB
*srchOrderIdLB.parent: search
*srchOrderIdLB.isCompound: "true"
*srchOrderIdLB.compoundIcon: "label.xpm"
*srchOrderIdLB.compoundName: "label_"
*srchOrderIdLB.x: 34
*srchOrderIdLB.y: 95
*srchOrderIdLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderIdLB.labelString: "Order ID:"
*srchOrderIdLB.background: "#9ac0cd"
*srchOrderIdLB.height: 24

*srchProcessingOptionLB.class: label
*srchProcessingOptionLB.static: true
*srchProcessingOptionLB.name: srchProcessingOptionLB
*srchProcessingOptionLB.parent: search
*srchProcessingOptionLB.isCompound: "true"
*srchProcessingOptionLB.compoundIcon: "label.xpm"
*srchProcessingOptionLB.compoundName: "label_"
*srchProcessingOptionLB.x: 675
*srchProcessingOptionLB.y: 160
*srchProcessingOptionLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchProcessingOptionLB.labelString: "Processing Option"
*srchProcessingOptionLB.background: "#9ac0cd"

*srchClearSearchPB.class: pushButton
*srchClearSearchPB.static: true
*srchClearSearchPB.name: srchClearSearchPB
*srchClearSearchPB.parent: search
*srchClearSearchPB.isCompound: "true"
*srchClearSearchPB.compoundIcon: "push.xpm"
*srchClearSearchPB.compoundName: "push_Button"
*srchClearSearchPB.x: 308
*srchClearSearchPB.y: 728
*srchClearSearchPB.width: 192
*srchClearSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*srchClearSearchPB.labelString: "CLEAR    SEARCH"
*srchClearSearchPB.height: 32
*srchClearSearchPB.background: "CadetBlue"
*srchClearSearchPB.shadowThickness: 4
*srchClearSearchPB.activateCallback.source: public
*srchClearSearchPB.activateCallback: search_clearCb

*srchCloseSearchPB.class: pushButton
*srchCloseSearchPB.static: true
*srchCloseSearchPB.name: srchCloseSearchPB
*srchCloseSearchPB.parent: search
*srchCloseSearchPB.isCompound: "true"
*srchCloseSearchPB.compoundIcon: "push.xpm"
*srchCloseSearchPB.compoundName: "push_Button"
*srchCloseSearchPB.x: 837
*srchCloseSearchPB.y: 728
*srchCloseSearchPB.width: 192
*srchCloseSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*srchCloseSearchPB.labelString: "CLOSE    SCREEN"
*srchCloseSearchPB.height: 32
*srchCloseSearchPB.background: "CadetBlue"
*srchCloseSearchPB.shadowThickness: 4
*srchCloseSearchPB.activateCallback.source: public
*srchCloseSearchPB.activateCallback: search_closeCb

*srchOrderStatusLB.class: label
*srchOrderStatusLB.static: true
*srchOrderStatusLB.name: srchOrderStatusLB
*srchOrderStatusLB.parent: search
*srchOrderStatusLB.isCompound: "true"
*srchOrderStatusLB.compoundIcon: "label.xpm"
*srchOrderStatusLB.compoundName: "label_"
*srchOrderStatusLB.x: 16
*srchOrderStatusLB.y: 158
*srchOrderStatusLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderStatusLB.labelString: "Order Status"
*srchOrderStatusLB.background: "#9ac0cd"
*srchOrderStatusLB.height: 24

*srchSeparator1.class: separator
*srchSeparator1.static: true
*srchSeparator1.name: srchSeparator1
*srchSeparator1.parent: search
*srchSeparator1.width: 1083
*srchSeparator1.height: 8
*srchSeparator1.isCompound: "true"
*srchSeparator1.compoundIcon: "sep.xpm"
*srchSeparator1.compoundName: "separator_"
*srchSeparator1.x: 0
*srchSeparator1.y: 140
*srchSeparator1.background: "#9ac0cd"
*srchSeparator1.separatorType: "shadow_etched_in"
*srchSeparator1.shadowThickness: 3

*srchOrderReceivedLB.class: label
*srchOrderReceivedLB.static: true
*srchOrderReceivedLB.name: srchOrderReceivedLB
*srchOrderReceivedLB.parent: search
*srchOrderReceivedLB.isCompound: "true"
*srchOrderReceivedLB.compoundIcon: "label.xpm"
*srchOrderReceivedLB.compoundName: "label_"
*srchOrderReceivedLB.x: 22
*srchOrderReceivedLB.y: 614
*srchOrderReceivedLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderReceivedLB.labelString: "Order Received:"
*srchOrderReceivedLB.background: "#9ac0cd"

*srchAccountIdLB.class: label
*srchAccountIdLB.static: true
*srchAccountIdLB.name: srchAccountIdLB
*srchAccountIdLB.parent: search
*srchAccountIdLB.isCompound: "true"
*srchAccountIdLB.compoundIcon: "label.xpm"
*srchAccountIdLB.compoundName: "label_"
*srchAccountIdLB.x: 335
*srchAccountIdLB.y: 97
*srchAccountIdLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchAccountIdLB.labelString: "Account ID:"
*srchAccountIdLB.background: "#9ac0cd"

*srchSeparator4.class: separator
*srchSeparator4.static: true
*srchSeparator4.name: srchSeparator4
*srchSeparator4.parent: search
*srchSeparator4.width: 16
*srchSeparator4.height: 278
*srchSeparator4.isCompound: "true"
*srchSeparator4.compoundIcon: "sep.xpm"
*srchSeparator4.compoundName: "separator_"
*srchSeparator4.x: 713
*srchSeparator4.y: 440
*srchSeparator4.background: "#9ac0cd"
*srchSeparator4.orientation: "vertical"
*srchSeparator4.separatorType: "shadow_etched_in"
*srchSeparator4.shadowThickness: 3

*srchOrderItemLB.class: label
*srchOrderItemLB.static: true
*srchOrderItemLB.name: srchOrderItemLB
*srchOrderItemLB.parent: search
*srchOrderItemLB.isCompound: "true"
*srchOrderItemLB.compoundIcon: "label.xpm"
*srchOrderItemLB.compoundName: "label_"
*srchOrderItemLB.x: 895
*srchOrderItemLB.y: 458
*srchOrderItemLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderItemLB.labelString: "Order Item"
*srchOrderItemLB.background: "#9ac0cd"

*srchValidatedLB.class: label
*srchValidatedLB.static: true
*srchValidatedLB.name: srchValidatedLB
*srchValidatedLB.parent: search
*srchValidatedLB.isCompound: "true"
*srchValidatedLB.compoundIcon: "label.xpm"
*srchValidatedLB.compoundName: "label_"
*srchValidatedLB.x: 744
*srchValidatedLB.y: 498
*srchValidatedLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchValidatedLB.labelString: "Validated:"
*srchValidatedLB.background: "#9ac0cd"

*srchDebitedLB.class: label
*srchDebitedLB.static: true
*srchDebitedLB.name: srchDebitedLB
*srchDebitedLB.parent: search
*srchDebitedLB.isCompound: "true"
*srchDebitedLB.compoundIcon: "label.xpm"
*srchDebitedLB.compoundName: "label_"
*srchDebitedLB.x: 744
*srchDebitedLB.y: 613
*srchDebitedLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchDebitedLB.labelString: "Debited:"
*srchDebitedLB.background: "#9ac0cd"

*srchShippedLB.class: label
*srchShippedLB.static: true
*srchShippedLB.name: srchShippedLB
*srchShippedLB.parent: search
*srchShippedLB.isCompound: "true"
*srchShippedLB.compoundIcon: "label.xpm"
*srchShippedLB.compoundName: "label_"
*srchShippedLB.x: 744
*srchShippedLB.y: 555
*srchShippedLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchShippedLB.labelString: "Shipped:"
*srchShippedLB.background: "#9ac0cd"

*srchBilledLB.class: label
*srchBilledLB.static: true
*srchBilledLB.name: srchBilledLB
*srchBilledLB.parent: search
*srchBilledLB.isCompound: "true"
*srchBilledLB.compoundIcon: "label.xpm"
*srchBilledLB.compoundName: "label_"
*srchBilledLB.x: 744
*srchBilledLB.y: 669
*srchBilledLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchBilledLB.labelString: "Billed:"
*srchBilledLB.background: "#9ac0cd"

*srchValidatedFrame.class: frame
*srchValidatedFrame.static: true
*srchValidatedFrame.name: srchValidatedFrame
*srchValidatedFrame.parent: search
*srchValidatedFrame.width: 225
*srchValidatedFrame.height: 44
*srchValidatedFrame.isCompound: "true"
*srchValidatedFrame.compoundIcon: "frame.xpm"
*srchValidatedFrame.compoundName: "frame_"
*srchValidatedFrame.x: 831
*srchValidatedFrame.y: 486
*srchValidatedFrame.shadowType: "shadow_in"

*srchValidatedRC.class: rowColumn
*srchValidatedRC.static: true
*srchValidatedRC.name: srchValidatedRC
*srchValidatedRC.parent: srchValidatedFrame
*srchValidatedRC.width: 200
*srchValidatedRC.height: 38
*srchValidatedRC.isCompound: "true"
*srchValidatedRC.compoundIcon: "row.xpm"
*srchValidatedRC.compoundName: "row_Column"
*srchValidatedRC.x: 804
*srchValidatedRC.y: 2
*srchValidatedRC.background: "LightSkyBlue3"
*srchValidatedRC.orientation: "horizontal"
*srchValidatedRC.radioBehavior: "true"
*srchValidatedRC.spacing: 0
*srchValidatedRC.whichButton: 1
*srchValidatedRC.userData: (XtPointer) 1

*srchValidatedYesTB.class: toggleButton
*srchValidatedYesTB.static: true
*srchValidatedYesTB.name: srchValidatedYesTB
*srchValidatedYesTB.parent: srchValidatedRC
*srchValidatedYesTB.isCompound: "true"
*srchValidatedYesTB.compoundIcon: "toggle.xpm"
*srchValidatedYesTB.compoundName: "toggle_Button"
*srchValidatedYesTB.x: 804
*srchValidatedYesTB.y: 3
*srchValidatedYesTB.background: "LightSkyBlue3"
*srchValidatedYesTB.indicatorSize: 25
*srchValidatedYesTB.labelString: "Yes"
*srchValidatedYesTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchValidatedYesTB.selectColor: "SlateGray4"
*srchValidatedYesTB.userData: (XtPointer) 0
*srchValidatedYesTB.valueChangedCallback.source: public
*srchValidatedYesTB.valueChangedCallback: search_radiobox_toggledCb
*srchValidatedYesTB.valueChangedCallbackClientData: (XtPointer) 0
*srchValidatedYesTB.marginHeight: 2
*srchValidatedYesTB.marginTop: 6
*srchValidatedYesTB.marginRight: 8
*srchValidatedYesTB.height: 32

*srchValidatedNoTB.class: toggleButton
*srchValidatedNoTB.static: true
*srchValidatedNoTB.name: srchValidatedNoTB
*srchValidatedNoTB.parent: srchValidatedRC
*srchValidatedNoTB.isCompound: "true"
*srchValidatedNoTB.compoundIcon: "toggle.xpm"
*srchValidatedNoTB.compoundName: "toggle_Button"
*srchValidatedNoTB.x: 804
*srchValidatedNoTB.y: 3
*srchValidatedNoTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchValidatedNoTB.indicatorSize: 25
*srchValidatedNoTB.labelString: "No"
*srchValidatedNoTB.background: "LightSkyBlue3"
*srchValidatedNoTB.width: 67
*srchValidatedNoTB.marginWidth: 2
*srchValidatedNoTB.selectColor: "SlateGray4"
*srchValidatedNoTB.userData: (XtPointer) 1
*srchValidatedNoTB.valueChangedCallback.source: public
*srchValidatedNoTB.valueChangedCallback: search_radiobox_toggledCb
*srchValidatedNoTB.valueChangedCallbackClientData: (XtPointer) 1
*srchValidatedNoTB.height: 32
*srchValidatedNoTB.marginTop: 6

*srchValidatedBothTB.class: toggleButton
*srchValidatedBothTB.static: true
*srchValidatedBothTB.name: srchValidatedBothTB
*srchValidatedBothTB.parent: srchValidatedRC
*srchValidatedBothTB.isCompound: "true"
*srchValidatedBothTB.compoundIcon: "toggle.xpm"
*srchValidatedBothTB.compoundName: "toggle_Button"
*srchValidatedBothTB.x: 136
*srchValidatedBothTB.y: 4
*srchValidatedBothTB.background: "LightSkyBlue3"
*srchValidatedBothTB.indicatorSize: 25
*srchValidatedBothTB.labelString: "Any"
*srchValidatedBothTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchValidatedBothTB.set: "true"
*srchValidatedBothTB.selectColor: "SlateGray4"
*srchValidatedBothTB.userData: (XtPointer) 2
*srchValidatedBothTB.valueChangedCallback.source: public
*srchValidatedBothTB.valueChangedCallback: search_radiobox_toggledCb
*srchValidatedBothTB.valueChangedCallbackClientData: (XtPointer) 2
*srchValidatedBothTB.height: 32

*srchShippedFrame.class: frame
*srchShippedFrame.static: true
*srchShippedFrame.name: srchShippedFrame
*srchShippedFrame.parent: search
*srchShippedFrame.width: 225
*srchShippedFrame.height: 44
*srchShippedFrame.isCompound: "true"
*srchShippedFrame.compoundIcon: "frame.xpm"
*srchShippedFrame.compoundName: "frame_"
*srchShippedFrame.x: 831
*srchShippedFrame.y: 543
*srchShippedFrame.shadowType: "shadow_in"

*srchShippedRC.class: rowColumn
*srchShippedRC.static: true
*srchShippedRC.name: srchShippedRC
*srchShippedRC.parent: srchShippedFrame
*srchShippedRC.width: 200
*srchShippedRC.height: 36
*srchShippedRC.isCompound: "true"
*srchShippedRC.compoundIcon: "row.xpm"
*srchShippedRC.compoundName: "row_Column"
*srchShippedRC.x: 804
*srchShippedRC.y: 2
*srchShippedRC.background: "LightSkyBlue3"
*srchShippedRC.orientation: "horizontal"
*srchShippedRC.radioBehavior: "true"
*srchShippedRC.spacing: 0
*srchShippedRC.whichButton: 1
*srchShippedRC.userData: (XtPointer) 2

*srchShippedYesTB.class: toggleButton
*srchShippedYesTB.static: true
*srchShippedYesTB.name: srchShippedYesTB
*srchShippedYesTB.parent: srchShippedRC
*srchShippedYesTB.isCompound: "true"
*srchShippedYesTB.compoundIcon: "toggle.xpm"
*srchShippedYesTB.compoundName: "toggle_Button"
*srchShippedYesTB.x: 804
*srchShippedYesTB.y: 3
*srchShippedYesTB.background: "LightSkyBlue3"
*srchShippedYesTB.indicatorSize: 25
*srchShippedYesTB.labelString: "Yes"
*srchShippedYesTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchShippedYesTB.selectColor: "SlateGray4"
*srchShippedYesTB.userData: (XtPointer) 0
*srchShippedYesTB.valueChangedCallback.source: public
*srchShippedYesTB.valueChangedCallback: search_radiobox_toggledCb
*srchShippedYesTB.valueChangedCallbackClientData: (XtPointer) 0
*srchShippedYesTB.marginRight: 8
*srchShippedYesTB.height: 32

*srchShippedNoTB.class: toggleButton
*srchShippedNoTB.static: true
*srchShippedNoTB.name: srchShippedNoTB
*srchShippedNoTB.parent: srchShippedRC
*srchShippedNoTB.isCompound: "true"
*srchShippedNoTB.compoundIcon: "toggle.xpm"
*srchShippedNoTB.compoundName: "toggle_Button"
*srchShippedNoTB.x: 804
*srchShippedNoTB.y: 3
*srchShippedNoTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchShippedNoTB.indicatorSize: 25
*srchShippedNoTB.labelString: "No"
*srchShippedNoTB.background: "LightSkyBlue3"
*srchShippedNoTB.selectColor: "SlateGray4"
*srchShippedNoTB.userData: (XtPointer) 1
*srchShippedNoTB.valueChangedCallback.source: public
*srchShippedNoTB.valueChangedCallback: search_radiobox_toggledCb
*srchShippedNoTB.valueChangedCallbackClientData: (XtPointer) 1
*srchShippedNoTB.height: 32

*srchShippedBothTB.class: toggleButton
*srchShippedBothTB.static: true
*srchShippedBothTB.name: srchShippedBothTB
*srchShippedBothTB.parent: srchShippedRC
*srchShippedBothTB.isCompound: "true"
*srchShippedBothTB.compoundIcon: "toggle.xpm"
*srchShippedBothTB.compoundName: "toggle_Button"
*srchShippedBothTB.x: 75
*srchShippedBothTB.y: 11
*srchShippedBothTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchShippedBothTB.indicatorSize: 25
*srchShippedBothTB.labelString: "Any"
*srchShippedBothTB.background: "LightSkyBlue3"
*srchShippedBothTB.set: "true"
*srchShippedBothTB.selectColor: "SlateGray4"
*srchShippedBothTB.userData: (XtPointer) 2
*srchShippedBothTB.valueChangedCallback.source: public
*srchShippedBothTB.valueChangedCallback: search_radiobox_toggledCb
*srchShippedBothTB.valueChangedCallbackClientData: (XtPointer) 2
*srchShippedBothTB.height: 32

*srchDebitedFrame.class: frame
*srchDebitedFrame.static: true
*srchDebitedFrame.name: srchDebitedFrame
*srchDebitedFrame.parent: search
*srchDebitedFrame.width: 225
*srchDebitedFrame.height: 44
*srchDebitedFrame.isCompound: "true"
*srchDebitedFrame.compoundIcon: "frame.xpm"
*srchDebitedFrame.compoundName: "frame_"
*srchDebitedFrame.x: 831
*srchDebitedFrame.y: 601
*srchDebitedFrame.shadowType: "shadow_in"

*srchDebitedRC.class: rowColumn
*srchDebitedRC.static: true
*srchDebitedRC.name: srchDebitedRC
*srchDebitedRC.parent: srchDebitedFrame
*srchDebitedRC.width: 200
*srchDebitedRC.height: 36
*srchDebitedRC.isCompound: "true"
*srchDebitedRC.compoundIcon: "row.xpm"
*srchDebitedRC.compoundName: "row_Column"
*srchDebitedRC.x: 804
*srchDebitedRC.y: 2
*srchDebitedRC.background: "LightSkyBlue3"
*srchDebitedRC.orientation: "horizontal"
*srchDebitedRC.radioBehavior: "true"
*srchDebitedRC.spacing: 0
*srchDebitedRC.whichButton: 1
*srchDebitedRC.userData: (XtPointer) 3

*srchDebitedYesTB.class: toggleButton
*srchDebitedYesTB.static: true
*srchDebitedYesTB.name: srchDebitedYesTB
*srchDebitedYesTB.parent: srchDebitedRC
*srchDebitedYesTB.isCompound: "true"
*srchDebitedYesTB.compoundIcon: "toggle.xpm"
*srchDebitedYesTB.compoundName: "toggle_Button"
*srchDebitedYesTB.x: 804
*srchDebitedYesTB.y: 3
*srchDebitedYesTB.background: "LightSkyBlue3"
*srchDebitedYesTB.indicatorSize: 25
*srchDebitedYesTB.labelString: "Yes"
*srchDebitedYesTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchDebitedYesTB.selectColor: "SlateGray4"
*srchDebitedYesTB.userData: (XtPointer) 0
*srchDebitedYesTB.valueChangedCallback.source: public
*srchDebitedYesTB.valueChangedCallback: search_radiobox_toggledCb
*srchDebitedYesTB.valueChangedCallbackClientData: (XtPointer) 0
*srchDebitedYesTB.marginRight: 8
*srchDebitedYesTB.height: 32

*srchDebitedNoTB.class: toggleButton
*srchDebitedNoTB.static: true
*srchDebitedNoTB.name: srchDebitedNoTB
*srchDebitedNoTB.parent: srchDebitedRC
*srchDebitedNoTB.isCompound: "true"
*srchDebitedNoTB.compoundIcon: "toggle.xpm"
*srchDebitedNoTB.compoundName: "toggle_Button"
*srchDebitedNoTB.x: 804
*srchDebitedNoTB.y: 3
*srchDebitedNoTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchDebitedNoTB.indicatorSize: 25
*srchDebitedNoTB.labelString: "No"
*srchDebitedNoTB.background: "LightSkyBlue3"
*srchDebitedNoTB.selectColor: "SlateGray4"
*srchDebitedNoTB.userData: (XtPointer) 1
*srchDebitedNoTB.valueChangedCallback.source: public
*srchDebitedNoTB.valueChangedCallback: search_radiobox_toggledCb
*srchDebitedNoTB.valueChangedCallbackClientData: (XtPointer) 1
*srchDebitedNoTB.height: 32

*srchDebitedBothTB.class: toggleButton
*srchDebitedBothTB.static: true
*srchDebitedBothTB.name: srchDebitedBothTB
*srchDebitedBothTB.parent: srchDebitedRC
*srchDebitedBothTB.isCompound: "true"
*srchDebitedBothTB.compoundIcon: "toggle.xpm"
*srchDebitedBothTB.compoundName: "toggle_Button"
*srchDebitedBothTB.x: 75
*srchDebitedBothTB.y: 11
*srchDebitedBothTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchDebitedBothTB.indicatorSize: 25
*srchDebitedBothTB.labelString: "Any"
*srchDebitedBothTB.background: "LightSkyBlue3"
*srchDebitedBothTB.selectColor: "SlateGray4"
*srchDebitedBothTB.set: "true"
*srchDebitedBothTB.userData: (XtPointer) 2
*srchDebitedBothTB.valueChangedCallback.source: public
*srchDebitedBothTB.valueChangedCallback: search_radiobox_toggledCb
*srchDebitedBothTB.valueChangedCallbackClientData: (XtPointer) 2
*srchDebitedBothTB.height: 32

*srchBilledFrame.class: frame
*srchBilledFrame.static: true
*srchBilledFrame.name: srchBilledFrame
*srchBilledFrame.parent: search
*srchBilledFrame.width: 225
*srchBilledFrame.height: 44
*srchBilledFrame.isCompound: "true"
*srchBilledFrame.compoundIcon: "frame.xpm"
*srchBilledFrame.compoundName: "frame_"
*srchBilledFrame.x: 831
*srchBilledFrame.y: 657
*srchBilledFrame.shadowType: "shadow_in"

*srchBilledRC.class: rowColumn
*srchBilledRC.static: true
*srchBilledRC.name: srchBilledRC
*srchBilledRC.parent: srchBilledFrame
*srchBilledRC.width: 200
*srchBilledRC.height: 36
*srchBilledRC.isCompound: "true"
*srchBilledRC.compoundIcon: "row.xpm"
*srchBilledRC.compoundName: "row_Column"
*srchBilledRC.x: 804
*srchBilledRC.y: 2
*srchBilledRC.background: "LightSkyBlue3"
*srchBilledRC.orientation: "horizontal"
*srchBilledRC.radioBehavior: "true"
*srchBilledRC.spacing: 0
*srchBilledRC.whichButton: 1
*srchBilledRC.userData: (XtPointer) 4

*srchBilledYesTB.class: toggleButton
*srchBilledYesTB.static: true
*srchBilledYesTB.name: srchBilledYesTB
*srchBilledYesTB.parent: srchBilledRC
*srchBilledYesTB.isCompound: "true"
*srchBilledYesTB.compoundIcon: "toggle.xpm"
*srchBilledYesTB.compoundName: "toggle_Button"
*srchBilledYesTB.x: 804
*srchBilledYesTB.y: 3
*srchBilledYesTB.background: "LightSkyBlue3"
*srchBilledYesTB.indicatorSize: 25
*srchBilledYesTB.labelString: "Yes"
*srchBilledYesTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchBilledYesTB.selectColor: "SlateGray4"
*srchBilledYesTB.userData: (XtPointer) 0
*srchBilledYesTB.valueChangedCallback.source: public
*srchBilledYesTB.valueChangedCallback: search_radiobox_toggledCb
*srchBilledYesTB.valueChangedCallbackClientData: (XtPointer) 0
*srchBilledYesTB.marginRight: 8
*srchBilledYesTB.height: 32

*srchBilledNoTB.class: toggleButton
*srchBilledNoTB.static: true
*srchBilledNoTB.name: srchBilledNoTB
*srchBilledNoTB.parent: srchBilledRC
*srchBilledNoTB.isCompound: "true"
*srchBilledNoTB.compoundIcon: "toggle.xpm"
*srchBilledNoTB.compoundName: "toggle_Button"
*srchBilledNoTB.x: 804
*srchBilledNoTB.y: 3
*srchBilledNoTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchBilledNoTB.indicatorSize: 25
*srchBilledNoTB.labelString: "No"
*srchBilledNoTB.background: "LightSkyBlue3"
*srchBilledNoTB.selectColor: "SlateGray4"
*srchBilledNoTB.userData: (XtPointer) 1
*srchBilledNoTB.valueChangedCallback.source: public
*srchBilledNoTB.valueChangedCallback: search_radiobox_toggledCb
*srchBilledNoTB.valueChangedCallbackClientData: (XtPointer) 1
*srchBilledNoTB.height: 32

*srchBilledBothTB.class: toggleButton
*srchBilledBothTB.static: true
*srchBilledBothTB.name: srchBilledBothTB
*srchBilledBothTB.parent: srchBilledRC
*srchBilledBothTB.isCompound: "true"
*srchBilledBothTB.compoundIcon: "toggle.xpm"
*srchBilledBothTB.compoundName: "toggle_Button"
*srchBilledBothTB.x: 75
*srchBilledBothTB.y: 11
*srchBilledBothTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchBilledBothTB.indicatorSize: 25
*srchBilledBothTB.labelString: "Any"
*srchBilledBothTB.background: "LightSkyBlue3"
*srchBilledBothTB.selectColor: "SlateGray4"
*srchBilledBothTB.set: "true"
*srchBilledBothTB.userData: (XtPointer) 2
*srchBilledBothTB.valueChangedCallback.source: public
*srchBilledBothTB.valueChangedCallback: search_radiobox_toggledCb
*srchBilledBothTB.valueChangedCallbackClientData: (XtPointer) 2
*srchBilledBothTB.height: 32

*mediaTypeSW.class: scrolledWindow
*mediaTypeSW.static: true
*mediaTypeSW.name: mediaTypeSW
*mediaTypeSW.parent: search
*mediaTypeSW.scrollingPolicy: "automatic"
*mediaTypeSW.width: 180
*mediaTypeSW.height: 230
*mediaTypeSW.isCompound: "true"
*mediaTypeSW.compoundIcon: "scrlwnd.xpm"
*mediaTypeSW.compoundName: "scrolled_Window"
*mediaTypeSW.x: 891
*mediaTypeSW.y: 188
*mediaTypeSW.background: "LightSkyBlue3"

*mediaTypeRC.class: rowColumn
*mediaTypeRC.static: true
*mediaTypeRC.name: mediaTypeRC
*mediaTypeRC.parent: mediaTypeSW
*mediaTypeRC.isCompound: "true"
*mediaTypeRC.compoundIcon: "row.xpm"
*mediaTypeRC.compoundName: "row_Column"
*mediaTypeRC.x: 2
*mediaTypeRC.y: 2
*mediaTypeRC.width: 149
*mediaTypeRC.height: 222
*mediaTypeRC.resizeWidth: "false"
*mediaTypeRC.background: "LightSkyBlue3"
*mediaTypeRC.spacing: 0

*processStatusSW.class: scrolledWindow
*processStatusSW.static: true
*processStatusSW.name: processStatusSW
*processStatusSW.parent: search
*processStatusSW.scrollingPolicy: "automatic"
*processStatusSW.width: 165
*processStatusSW.height: 230
*processStatusSW.isCompound: "true"
*processStatusSW.compoundIcon: "scrlwnd.xpm"
*processStatusSW.compoundName: "scrolled_Window"
*processStatusSW.x: 476
*processStatusSW.y: 188
*processStatusSW.background: "LightSkyBlue3"

*processStatusRC.class: rowColumn
*processStatusRC.static: true
*processStatusRC.name: processStatusRC
*processStatusRC.parent: processStatusSW
*processStatusRC.isCompound: "true"
*processStatusRC.compoundIcon: "row.xpm"
*processStatusRC.compoundName: "row_Column"
*processStatusRC.x: 2
*processStatusRC.y: 2
*processStatusRC.width: 134
*processStatusRC.height: 226
*processStatusRC.background: "lightSkyBlue3"
*processStatusRC.resizeHeight: "true"
*processStatusRC.resizeWidth: "false"
*processStatusRC.spacing: 0
*processStatusRC.marginHeight: 5

*srchItemStatusSW.class: scrolledWindow
*srchItemStatusSW.static: true
*srchItemStatusSW.name: srchItemStatusSW
*srchItemStatusSW.parent: search
*srchItemStatusSW.scrollingPolicy: "automatic"
*srchItemStatusSW.width: 181
*srchItemStatusSW.height: 230
*srchItemStatusSW.isCompound: "true"
*srchItemStatusSW.compoundIcon: "scrlwnd.xpm"
*srchItemStatusSW.compoundName: "scrolled_Window"
*srchItemStatusSW.x: 176
*srchItemStatusSW.y: 188
*srchItemStatusSW.background: "LightSkyBlue3"

*itemStatusRC.class: rowColumn
*itemStatusRC.static: true
*itemStatusRC.name: itemStatusRC
*itemStatusRC.parent: srchItemStatusSW
*itemStatusRC.isCompound: "true"
*itemStatusRC.compoundIcon: "row.xpm"
*itemStatusRC.compoundName: "row_Column"
*itemStatusRC.x: 2
*itemStatusRC.y: 2
*itemStatusRC.width: 150
*itemStatusRC.height: 226
*itemStatusRC.background: "LightSkyBlue3"
*itemStatusRC.resizeHeight: "true"
*itemStatusRC.resizeWidth: "false"
*itemStatusRC.spacing: 0
*itemStatusRC.marginHeight: 5

*srchOrderStatusSW.class: scrolledWindow
*srchOrderStatusSW.static: true
*srchOrderStatusSW.name: srchOrderStatusSW
*srchOrderStatusSW.parent: search
*srchOrderStatusSW.scrollingPolicy: "automatic"
*srchOrderStatusSW.width: 158
*srchOrderStatusSW.height: 230
*srchOrderStatusSW.isCompound: "true"
*srchOrderStatusSW.compoundIcon: "scrlwnd.xpm"
*srchOrderStatusSW.compoundName: "scrolled_Window"
*srchOrderStatusSW.x: 12
*srchOrderStatusSW.y: 188
*srchOrderStatusSW.background: "LightSkyBlue3"
*srchOrderStatusSW.scrollBarDisplayPolicy: "as_needed"

*orderStatusRC.class: rowColumn
*orderStatusRC.static: true
*orderStatusRC.name: orderStatusRC
*orderStatusRC.parent: srchOrderStatusSW
*orderStatusRC.isCompound: "true"
*orderStatusRC.compoundIcon: "row.xpm"
*orderStatusRC.compoundName: "row_Column"
*orderStatusRC.x: 2
*orderStatusRC.y: 2
*orderStatusRC.width: 127
*orderStatusRC.height: 226
*orderStatusRC.background: "LightSkyBlue3"
*orderStatusRC.resizeHeight: "true"
*orderStatusRC.resizeWidth: "false"
*orderStatusRC.spacing: 0

*dummyTB.class: toggleButton
*dummyTB.static: true
*dummyTB.name: dummyTB
*dummyTB.parent: orderStatusRC
*dummyTB.isCompound: "true"
*dummyTB.compoundIcon: "toggle.xpm"
*dummyTB.compoundName: "toggle_Button"
*dummyTB.x: -14
*dummyTB.y: 0
*dummyTB.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dummyTB.indicatorSize: 25
*dummyTB.labelString: "Dummy"
*dummyTB.background: "LightSkyBlue3"
*dummyTB.width: 130
*dummyTB.selectColor: "SlateGray4"

*processOptionSW.class: scrolledWindow
*processOptionSW.static: true
*processOptionSW.name: processOptionSW
*processOptionSW.parent: search
*processOptionSW.scrollingPolicy: "automatic"
*processOptionSW.height: 230
*processOptionSW.isCompound: "true"
*processOptionSW.compoundIcon: "scrlwnd.xpm"
*processOptionSW.compoundName: "scrolled_Window"
*processOptionSW.x: 648
*processOptionSW.y: 188
*processOptionSW.background: "LightSkyBlue3"
*processOptionSW.width: 237

*processOptionRC.class: rowColumn
*processOptionRC.static: true
*processOptionRC.name: processOptionRC
*processOptionRC.parent: processOptionSW
*processOptionRC.isCompound: "true"
*processOptionRC.compoundIcon: "row.xpm"
*processOptionRC.compoundName: "row_Column"
*processOptionRC.x: 2
*processOptionRC.y: 4
*processOptionRC.width: 206
*processOptionRC.height: 226
*processOptionRC.background: "LightSkyBLue3"
*processOptionRC.resizeWidth: "false"
*processOptionRC.marginWidth: 5
*processOptionRC.spacing: 0

*srchOrderIdTF.class: textField
*srchOrderIdTF.static: true
*srchOrderIdTF.name: srchOrderIdTF
*srchOrderIdTF.parent: search
*srchOrderIdTF.width: 150
*srchOrderIdTF.isCompound: "true"
*srchOrderIdTF.compoundIcon: "textfield.xpm"
*srchOrderIdTF.compoundName: "text_Field"
*srchOrderIdTF.x: 134
*srchOrderIdTF.y: 87
*srchOrderIdTF.background: "LightSkyBlue3"
*srchOrderIdTF.height: 40
*srchOrderIdTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*accountIdTF.class: textField
*accountIdTF.static: true
*accountIdTF.name: accountIdTF
*accountIdTF.parent: search
*accountIdTF.width: 160
*accountIdTF.isCompound: "true"
*accountIdTF.compoundIcon: "textfield.xpm"
*accountIdTF.compoundName: "text_Field"
*accountIdTF.x: 453
*accountIdTF.y: 87
*accountIdTF.background: "LightSkyBlue3"
*accountIdTF.height: 40
*accountIdTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*accountIdTF.cursorPositionVisible: "true"

*userIdTF.class: textField
*userIdTF.static: true
*userIdTF.name: userIdTF
*userIdTF.parent: search
*userIdTF.width: 160
*userIdTF.isCompound: "true"
*userIdTF.compoundIcon: "textfield.xpm"
*userIdTF.compoundName: "text_Field"
*userIdTF.x: 821
*userIdTF.y: 87
*userIdTF.background: "LightSkyBlue3"
*userIdTF.height: 40
*userIdTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*userIdPB.class: pushButton
*userIdPB.static: true
*userIdPB.name: userIdPB
*userIdPB.parent: search
*userIdPB.isCompound: "true"
*userIdPB.compoundIcon: "push.xpm"
*userIdPB.compoundName: "push_Button"
*userIdPB.x: 981
*userIdPB.y: 87
*userIdPB.width: 60
*userIdPB.background: "LightSkyBlue3"
*userIdPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*userIdPB.labelString: "List..."
*userIdPB.height: 40
*userIdPB.shadowThickness: 3
*userIdPB.activateCallback.source: public
*userIdPB.activateCallback: search_userId_validsCb

*accountIdPB.class: pushButton
*accountIdPB.static: true
*accountIdPB.name: accountIdPB
*accountIdPB.parent: search
*accountIdPB.isCompound: "true"
*accountIdPB.compoundIcon: "push.xpm"
*accountIdPB.compoundName: "push_Button"
*accountIdPB.x: 613
*accountIdPB.y: 87
*accountIdPB.width: 60
*accountIdPB.background: "LightSkyBlue3"
*accountIdPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*accountIdPB.labelString: "List..."
*accountIdPB.height: 40
*accountIdPB.shadowThickness: 3
*accountIdPB.activateCallback.source: public
*accountIdPB.activateCallback: search_accountId_validsCb

*processingTypeOM.class: rowColumn
*processingTypeOM.static: true
*processingTypeOM.name: processingTypeOM
*processingTypeOM.parent: search
*processingTypeOM.rowColumnType: "menu_option"
*processingTypeOM.subMenuId: "optionMenu_p1"
*processingTypeOM.isCompound: "true"
*processingTypeOM.compoundIcon: "optionM.xpm"
*processingTypeOM.compoundName: "option_Menu"
*processingTypeOM.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*processingTypeOM.x: 158
*processingTypeOM.y: 450
*processingTypeOM.background: "#9ac0cd"
*processingTypeOM.width: 200
*processingTypeOM.labelString: " "

*optionMenu_p1.class: rowColumn
*optionMenu_p1.static: true
*optionMenu_p1.name: optionMenu_p1
*optionMenu_p1.parent: processingTypeOM
*optionMenu_p1.rowColumnType: "menu_pulldown"
*optionMenu_p1.background: "#9ac0cd"
*optionMenu_p1.width: 150
*optionMenu_p1.userData: (XtPointer) 1

*processTypeAllPB.class: pushButton
*processTypeAllPB.static: true
*processTypeAllPB.name: processTypeAllPB
*processTypeAllPB.parent: optionMenu_p1
*processTypeAllPB.labelString: "ALL"
*processTypeAllPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*processTypeAllPB.activateCallback.source: public
*processTypeAllPB.activateCallback: search_optionmenu_toggledCb
*processTypeAllPB.activateCallbackClientData: (XtPointer) 0
*processTypeAllPB.background: "LightSkyBlue3"

*optionMenu_p1_b4.class: separator
*optionMenu_p1_b4.static: true
*optionMenu_p1_b4.name: optionMenu_p1_b4
*optionMenu_p1_b4.parent: optionMenu_p1

*optionMenu_p_b1.class: pushButton
*optionMenu_p_b1.static: true
*optionMenu_p_b1.name: optionMenu_p_b1
*optionMenu_p_b1.parent: optionMenu_p1
*optionMenu_p_b1.labelString: "STANDARD"
*optionMenu_p_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*optionMenu_p_b1.background: "LightSkyBlue3"
*optionMenu_p_b1.width: 200
*optionMenu_p_b1.marginWidth: 20
*optionMenu_p_b1.userData: (XtPointer) 0x0
*optionMenu_p_b1.activateCallback.source: public
*optionMenu_p_b1.activateCallback: search_optionmenu_toggledCb
*optionMenu_p_b1.activateCallbackClientData: (XtPointer) 1

*optionMenu_p1_b3.class: separator
*optionMenu_p1_b3.static: true
*optionMenu_p1_b3.name: optionMenu_p1_b3
*optionMenu_p1_b3.parent: optionMenu_p1

*optionMenu_p1_b2.class: pushButton
*optionMenu_p1_b2.static: true
*optionMenu_p1_b2.name: optionMenu_p1_b2
*optionMenu_p1_b2.parent: optionMenu_p1
*optionMenu_p1_b2.labelString: "QUICK-LOOK"
*optionMenu_p1_b2.background: "LightSkyBlue3"
*optionMenu_p1_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*optionMenu_p1_b2.userData: (XtPointer) 0x0
*optionMenu_p1_b2.activateCallback.source: public
*optionMenu_p1_b2.activateCallback: search_optionmenu_toggledCb
*optionMenu_p1_b2.activateCallbackClientData: (XtPointer) 2

*srchPrintSearchPB.class: pushButton
*srchPrintSearchPB.static: true
*srchPrintSearchPB.name: srchPrintSearchPB
*srchPrintSearchPB.parent: search
*srchPrintSearchPB.isCompound: "true"
*srchPrintSearchPB.compoundIcon: "push.xpm"
*srchPrintSearchPB.compoundName: "push_Button"
*srchPrintSearchPB.x: 573
*srchPrintSearchPB.y: 728
*srchPrintSearchPB.width: 192
*srchPrintSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*srchPrintSearchPB.labelString: "PRINT     SCREEN"
*srchPrintSearchPB.height: 32
*srchPrintSearchPB.background: "CadetBlue"
*srchPrintSearchPB.shadowThickness: 4
*srchPrintSearchPB.activateCallback.source: public
*srchPrintSearchPB.activateCallback: search_printScreenCb

*srchOrderItemLB1.class: label
*srchOrderItemLB1.static: true
*srchOrderItemLB1.name: srchOrderItemLB1
*srchOrderItemLB1.parent: search
*srchOrderItemLB1.isCompound: "true"
*srchOrderItemLB1.compoundIcon: "label.xpm"
*srchOrderItemLB1.compoundName: "label_"
*srchOrderItemLB1.x: 382
*srchOrderItemLB1.y: 459
*srchOrderItemLB1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderItemLB1.labelString: "Order Priority:"
*srchOrderItemLB1.background: "#9ac0cd"

*srchOrderItemLB2.class: label
*srchOrderItemLB2.static: true
*srchOrderItemLB2.name: srchOrderItemLB2
*srchOrderItemLB2.parent: search
*srchOrderItemLB2.isCompound: "true"
*srchOrderItemLB2.compoundIcon: "label.xpm"
*srchOrderItemLB2.compoundName: "label_"
*srchOrderItemLB2.x: 22
*srchOrderItemLB2.y: 459
*srchOrderItemLB2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchOrderItemLB2.labelString: "Processing Type:"
*srchOrderItemLB2.background: "#9ac0cd"

*orderPriorityOM.class: rowColumn
*orderPriorityOM.static: true
*orderPriorityOM.name: orderPriorityOM
*orderPriorityOM.parent: search
*orderPriorityOM.rowColumnType: "menu_option"
*orderPriorityOM.subMenuId: "priorityOM_pane"
*orderPriorityOM.isCompound: "true"
*orderPriorityOM.compoundIcon: "optionM.xpm"
*orderPriorityOM.compoundName: "option_Menu"
*orderPriorityOM.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*orderPriorityOM.x: 502
*orderPriorityOM.y: 450
*orderPriorityOM.background: "#9ac0cd"
*orderPriorityOM.width: 200
*orderPriorityOM.marginHeight: 3
*orderPriorityOM.marginWidth: 3
*orderPriorityOM.labelString: " "

*priorityOM_pane.class: rowColumn
*priorityOM_pane.static: true
*priorityOM_pane.name: priorityOM_pane
*priorityOM_pane.parent: orderPriorityOM
*priorityOM_pane.rowColumnType: "menu_pulldown"
*priorityOM_pane.background: "#9ac0cd"
*priorityOM_pane.width: 150
*priorityOM_pane.userData: (XtPointer) 2

*orderPriorityAllPB.class: pushButton
*orderPriorityAllPB.static: true
*orderPriorityAllPB.name: orderPriorityAllPB
*orderPriorityAllPB.parent: priorityOM_pane
*orderPriorityAllPB.labelString: "ALL"
*orderPriorityAllPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderPriorityAllPB.background: "LightSkyBlue3"
*orderPriorityAllPB.width: 200
*orderPriorityAllPB.marginWidth: 20
*orderPriorityAllPB.marginLeft: 10
*orderPriorityAllPB.marginRight: 10
*orderPriorityAllPB.activateCallback.source: public
*orderPriorityAllPB.activateCallback: search_optionmenu_toggledCb
*orderPriorityAllPB.activateCallbackClientData: (XtPointer) 0

*frame1.class: frame
*frame1.static: true
*frame1.name: frame1
*frame1.parent: search
*frame1.width: 516
*frame1.height: 30
*frame1.isCompound: "true"
*frame1.compoundIcon: "frame.xpm"
*frame1.compoundName: "frame_"
*frame1.x: 172
*frame1.y: 558
*frame1.background: "#9ac0cd"
*frame1.shadowType: "shadow_etched_in"
*frame1.shadowThickness: 3

*label40.class: label
*label40.static: true
*label40.name: label40
*label40.parent: frame1
*label40.isCompound: "true"
*label40.compoundIcon: "label.xpm"
*label40.compoundName: "label_"
*label40.x: 144
*label40.y: 4
*label40.background: "#9ac0cd"
*label40.labelString: "Date Format:   YYYY-MM-DD"
*label40.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*srchItemStatusLB1.class: label
*srchItemStatusLB1.static: true
*srchItemStatusLB1.name: srchItemStatusLB1
*srchItemStatusLB1.parent: search
*srchItemStatusLB1.isCompound: "true"
*srchItemStatusLB1.compoundIcon: "label.xpm"
*srchItemStatusLB1.compoundName: "label_"
*srchItemStatusLB1.x: 366
*srchItemStatusLB1.y: 160
*srchItemStatusLB1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchItemStatusLB1.labelString: "Item Type"
*srchItemStatusLB1.background: "#9ac0cd"

*srchItemTypeSW.class: scrolledWindow
*srchItemTypeSW.static: true
*srchItemTypeSW.name: srchItemTypeSW
*srchItemTypeSW.parent: search
*srchItemTypeSW.scrollingPolicy: "automatic"
*srchItemTypeSW.width: 104
*srchItemTypeSW.height: 230
*srchItemTypeSW.isCompound: "true"
*srchItemTypeSW.compoundIcon: "scrlwnd.xpm"
*srchItemTypeSW.compoundName: "scrolled_Window"
*srchItemTypeSW.x: 364
*srchItemTypeSW.y: 188
*srchItemTypeSW.background: "LightSkyBlue3"

*itemTypeRC.class: rowColumn
*itemTypeRC.static: true
*itemTypeRC.name: itemTypeRC
*itemTypeRC.parent: srchItemTypeSW
*itemTypeRC.isCompound: "true"
*itemTypeRC.compoundIcon: "row.xpm"
*itemTypeRC.compoundName: "row_Column"
*itemTypeRC.y: 0
*itemTypeRC.width: 73
*itemTypeRC.height: 226
*itemTypeRC.background: "LightSkyBlue3"
*itemTypeRC.resizeHeight: "true"
*itemTypeRC.resizeWidth: "false"
*itemTypeRC.spacing: 0
*itemTypeRC.marginHeight: 5

