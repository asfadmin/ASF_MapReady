! UIMX ascii 2.9 key: 8411                                                      

*photoJob.class: form
*photoJob.classinc:
*photoJob.classspec:
*photoJob.classmembers:
*photoJob.classconstructor:
*photoJob.classdestructor:
*photoJob.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*photoJob.ispecdecl:
*photoJob.funcdecl: swidget create_photoJob(swidget UxParent)
*photoJob.funcname: create_photoJob
*photoJob.funcdef: "swidget", "<create_photoJob>(%)"
*photoJob.argdecl: swidget UxParent;
*photoJob.arglist: UxParent
*photoJob.arglist.UxParent: "swidget", "%UxParent%"
*photoJob.icode:
*photoJob.fcode: return(rtrn);\

*photoJob.auxdecl:
*photoJob.static: true
*photoJob.name: photoJob
*photoJob.parent: NO_PARENT
*photoJob.parentExpression: UxParent
*photoJob.defaultShell: transientShell
*photoJob.width: 1047
*photoJob.height: 790
*photoJob.resizePolicy: "resize_none"
*photoJob.isCompound: "true"
*photoJob.compoundIcon: "form.xpm"
*photoJob.compoundName: "form_"
*photoJob.x: 60
*photoJob.y: 50
*photoJob.unitType: "pixels"
*photoJob.allowShellResize: "false"
*photoJob.background: "#9ac0cd"

*menuBar3.class: rowColumn
*menuBar3.static: true
*menuBar3.name: menuBar3
*menuBar3.parent: photoJob
*menuBar3.rowColumnType: "menu_bar"
*menuBar3.isCompound: "true"
*menuBar3.compoundIcon: "pulldownM.xpm"
*menuBar3.compoundName: "menu_Bar"
*menuBar3.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*menuBar3.x: 0
*menuBar3.y: 0
*menuBar3.background: "CadetBlue"
*menuBar3.height: 36
*menuBar3.menuAccelerator: "<KeyUp>F10"
*menuBar3.menuHelpWidget: "menuBar1_top_b6"
*menuBar3.rightAttachment: "attach_form"
*menuBar3.leftAttachment: "attach_form"

*menuBar_p3.class: rowColumn
*menuBar_p3.static: true
*menuBar_p3.name: menuBar_p3
*menuBar_p3.parent: menuBar3
*menuBar_p3.rowColumnType: "menu_pulldown"

*welcomeMPB.class: pushButton
*welcomeMPB.static: true
*welcomeMPB.name: welcomeMPB
*welcomeMPB.parent: menuBar_p3
*welcomeMPB.labelString: "Welcome Screen"
*welcomeMPB.background: "CadetBlue"
*welcomeMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*welcomeMPB.mnemonic: "W"
*welcomeMPB.activateCallback.source: public
*welcomeMPB.activateCallback: goto_welcomeScreen

*menuBar_p1_b19.class: separator
*menuBar_p1_b19.static: true
*menuBar_p1_b19.name: menuBar_p1_b19
*menuBar_p1_b19.parent: menuBar_p3

*gotoPhotoOrderMPB.class: pushButton
*gotoPhotoOrderMPB.static: true
*gotoPhotoOrderMPB.name: gotoPhotoOrderMPB
*gotoPhotoOrderMPB.parent: menuBar_p3
*gotoPhotoOrderMPB.labelString: "Create Photo Jobs Screen"
*gotoPhotoOrderMPB.background: "cadetBlue"
*gotoPhotoOrderMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*gotoPhotoOrderMPB.mnemonic: "P"
*gotoPhotoOrderMPB.activateCallback.source: public
*gotoPhotoOrderMPB.activateCallback: goto_photoOrderScreen

*menuBar_p1_b22.class: separator
*menuBar_p1_b22.static: true
*menuBar_p1_b22.name: menuBar_p1_b22
*menuBar_p1_b22.parent: menuBar_p3

*menuBar_p1_b27.class: pushButton
*menuBar_p1_b27.static: true
*menuBar_p1_b27.name: menuBar_p1_b27
*menuBar_p1_b27.parent: menuBar_p3
*menuBar_p1_b27.labelString: "Close  Screen"
*menuBar_p1_b27.mnemonic: "C"
*menuBar_p1_b27.background: "cadetBlue"
*menuBar_p1_b27.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_p1_b27.activateCallback.source: public
*menuBar_p1_b27.activateCallback: photoJob_closeCb

*menuBar1_p5.class: rowColumn
*menuBar1_p5.static: true
*menuBar1_p5.name: menuBar1_p5
*menuBar1_p5.parent: menuBar3
*menuBar1_p5.rowColumnType: "menu_pulldown"

*photoJobsPrintMPB.class: pushButton
*photoJobsPrintMPB.static: true
*photoJobsPrintMPB.name: photoJobsPrintMPB
*photoJobsPrintMPB.parent: menuBar1_p5
*photoJobsPrintMPB.labelString: "Print  Screen"
*photoJobsPrintMPB.mnemonic: "P"
*photoJobsPrintMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*photoJobsPrintMPB.background: "CadetBlue"
*photoJobsPrintMPB.activateCallback.source: public
*photoJobsPrintMPB.activateCallback: photoJob_printCb

*menuBar1_p6.class: rowColumn
*menuBar1_p6.static: true
*menuBar1_p6.name: menuBar1_p6
*menuBar1_p6.parent: menuBar3
*menuBar1_p6.rowColumnType: "menu_pulldown"

*menuBar1_p3_b3.class: pushButton
*menuBar1_p3_b3.static: true
*menuBar1_p3_b3.name: menuBar1_p3_b3
*menuBar1_p3_b3.parent: menuBar1_p6
*menuBar1_p3_b3.labelString: "No Help Available"
*menuBar1_p3_b3.activateCallback.source: public
*menuBar1_p3_b3.activateCallback: 
*menuBar1_p3_b3.background: "CadetBlue"

*menuBar_top_b3.class: cascadeButton
*menuBar_top_b3.static: true
*menuBar_top_b3.name: menuBar_top_b3
*menuBar_top_b3.parent: menuBar3
*menuBar_top_b3.labelString: "Go To"
*menuBar_top_b3.subMenuId: "menuBar_p3"
*menuBar_top_b3.mnemonic: "G"
*menuBar_top_b3.background: "CadetBlue"
*menuBar_top_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_top_b3.marginWidth: 10
*menuBar_top_b3.x: 0
*menuBar_top_b3.y: 0

*menuBar1_top_b5.class: cascadeButtonGadget
*menuBar1_top_b5.static: true
*menuBar1_top_b5.name: menuBar1_top_b5
*menuBar1_top_b5.parent: menuBar3
*menuBar1_top_b5.labelString: "Screen Functions"
*menuBar1_top_b5.mnemonic: "S"
*menuBar1_top_b5.subMenuId: "menuBar1_p5"
*menuBar1_top_b5.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b5.marginWidth: 20
*menuBar1_top_b5.x: 0
*menuBar1_top_b5.y: 0

*menuBar1_top_b6.class: cascadeButton
*menuBar1_top_b6.static: true
*menuBar1_top_b6.name: menuBar1_top_b6
*menuBar1_top_b6.parent: menuBar3
*menuBar1_top_b6.labelString: "Help"
*menuBar1_top_b6.mnemonic: "H"
*menuBar1_top_b6.subMenuId: "menuBar1_p6"
*menuBar1_top_b6.background: "CadetBlue"
*menuBar1_top_b6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b6.x: 0
*menuBar1_top_b6.y: 0

*label48.class: label
*label48.static: true
*label48.name: label48
*label48.parent: photoJob
*label48.isCompound: "true"
*label48.compoundIcon: "label.xpm"
*label48.compoundName: "label_"
*label48.x: 375
*label48.y: 36
*label48.background: "#9ac0cd"
*label48.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label48.labelString: "Complete   Photo   Jobs   Screen"
*label48.height: 32

*separator7.class: separator
*separator7.static: true
*separator7.name: separator7
*separator7.parent: photoJob
*separator7.width: 1048
*separator7.height: 4
*separator7.isCompound: "true"
*separator7.compoundIcon: "sep.xpm"
*separator7.compoundName: "separator_"
*separator7.x: 0
*separator7.y: 732
*separator7.background: "#9ac0cd"

*frame4.class: frame
*frame4.static: true
*frame4.name: frame4
*frame4.parent: photoJob
*frame4.width: 416
*frame4.height: 652
*frame4.isCompound: "true"
*frame4.compoundIcon: "frame.xpm"
*frame4.compoundName: "frame_"
*frame4.x: 8
*frame4.y: 72
*frame4.background: "#9ac0cd"
*frame4.shadowThickness: 4
*frame4.shadowType: "shadow_etched_out"

*form6.class: form
*form6.static: true
*form6.name: form6
*form6.parent: frame4
*form6.width: 200
*form6.height: 200
*form6.resizePolicy: "resize_none"
*form6.isCompound: "true"
*form6.compoundIcon: "form.xpm"
*form6.compoundName: "form_"
*form6.x: 0
*form6.y: -44
*form6.background: "#9ac0cd"

*jobIdSW1.class: scrolledWindow
*jobIdSW1.static: true
*jobIdSW1.name: jobIdSW1
*jobIdSW1.parent: form6
*jobIdSW1.scrollingPolicy: "automatic"
*jobIdSW1.visualPolicy: "constant"
*jobIdSW1.scrollBarDisplayPolicy: "as_needed"
*jobIdSW1.shadowThickness: 0
*jobIdSW1.isCompound: "true"
*jobIdSW1.compoundIcon: "scrllist.xpm"
*jobIdSW1.compoundName: "scrolled_List"
*jobIdSW1.x: 16
*jobIdSW1.y: 256
*jobIdSW1.background: "LightSkyBlue3"
*jobIdSW1.height: 336
*jobIdSW1.width: 80

*jobIdSL1.class: scrolledList
*jobIdSL1.static: true
*jobIdSL1.name: jobIdSL1
*jobIdSL1.parent: jobIdSW1
*jobIdSL1.width: 80
*jobIdSL1.height: 336
*jobIdSL1.background: "LightSkyBlue3"
*jobIdSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*jobIdSL1.listSizePolicy: "constant"
*jobIdSL1.selectionPolicy: "browse_select"
*jobIdSL1.x: 0
*jobIdSL1.y: 93
*jobIdSL1.listSpacing: 0
*jobIdSL1.browseSelectionCallback.source: public
*jobIdSL1.browseSelectionCallback: photoJob_jobLists_selectionCb
*jobIdSL1.browseSelectionCallbackClientData: (XtPointer) 1
*jobIdSL1.defaultActionCallback.source: public
*jobIdSL1.defaultActionCallback: photoJob_viewCb

*label51.class: label
*label51.static: true
*label51.name: label51
*label51.parent: form6
*label51.isCompound: "true"
*label51.compoundIcon: "label.xpm"
*label51.compoundName: "label_"
*label51.x: 176
*label51.y: 232
*label51.background: "#9ac0cd"
*label51.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label51.labelString: "Start Date"

*startDateSW1.class: scrolledWindow
*startDateSW1.static: true
*startDateSW1.name: startDateSW1
*startDateSW1.parent: form6
*startDateSW1.scrollingPolicy: "automatic"
*startDateSW1.visualPolicy: "constant"
*startDateSW1.scrollBarDisplayPolicy: "as_needed"
*startDateSW1.shadowThickness: 0
*startDateSW1.isCompound: "true"
*startDateSW1.compoundIcon: "scrllist.xpm"
*startDateSW1.compoundName: "scrolled_List"
*startDateSW1.x: 176
*startDateSW1.y: 256
*startDateSW1.background: "LightSkyBlue3"
*startDateSW1.height: 336
*startDateSW1.width: 100

*startDateSL1.class: scrolledList
*startDateSL1.static: true
*startDateSL1.name: startDateSL1
*startDateSL1.parent: startDateSW1
*startDateSL1.width: 100
*startDateSL1.height: 336
*startDateSL1.background: "LightSkyBlue3"
*startDateSL1.listSizePolicy: "constant"
*startDateSL1.selectionPolicy: "browse_select"
*startDateSL1.x: 0
*startDateSL1.y: 93
*startDateSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*startDateSL1.itemCount: 0
*startDateSL1.browseSelectionCallback.source: public
*startDateSL1.browseSelectionCallback: photoJob_jobLists_selectionCb
*startDateSL1.browseSelectionCallbackClientData: (XtPointer) 3
*startDateSL1.defaultActionCallback.source: public
*startDateSL1.defaultActionCallback: photoJob_viewCb

*photoJobDummySW1.class: scrolledWindow
*photoJobDummySW1.static: true
*photoJobDummySW1.name: photoJobDummySW1
*photoJobDummySW1.parent: form6
*photoJobDummySW1.scrollingPolicy: "application_defined"
*photoJobDummySW1.visualPolicy: "variable"
*photoJobDummySW1.scrollBarDisplayPolicy: "static"
*photoJobDummySW1.shadowThickness: 0
*photoJobDummySW1.isCompound: "true"
*photoJobDummySW1.compoundIcon: "scrllist.xpm"
*photoJobDummySW1.compoundName: "scrolled_List"
*photoJobDummySW1.x: 376
*photoJobDummySW1.y: 256
*photoJobDummySW1.width: 15
*photoJobDummySW1.background: "LightSkyBlue3"
*photoJobDummySW1.height: 332

*photoJobDummySL1.class: scrolledList
*photoJobDummySL1.static: true
*photoJobDummySL1.name: photoJobDummySL1
*photoJobDummySL1.parent: photoJobDummySW1
*photoJobDummySL1.width: 2
*photoJobDummySL1.height: 320
*photoJobDummySL1.background: "LightSkyBlue3"
*photoJobDummySL1.listSizePolicy: "variable"
*photoJobDummySL1.scrollBarDisplayPolicy: "static"
*photoJobDummySL1.selectionPolicy: "browse_select"
*photoJobDummySL1.x: 0
*photoJobDummySL1.y: 93
*photoJobDummySL1.visibleItemCount: 19

*label53.class: label
*label53.static: true
*label53.name: label53
*label53.parent: form6
*label53.isCompound: "true"
*label53.compoundIcon: "label.xpm"
*label53.compoundName: "label_"
*label53.x: 16
*label53.y: 16
*label53.background: "#9ac0cd"
*label53.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label53.labelString: "Photo Job ID:"
*label53.height: 24

*jobIdTF1.class: textField
*jobIdTF1.static: true
*jobIdTF1.name: jobIdTF1
*jobIdTF1.parent: form6
*jobIdTF1.width: 141
*jobIdTF1.isCompound: "true"
*jobIdTF1.compoundIcon: "textfield.xpm"
*jobIdTF1.compoundName: "text_Field"
*jobIdTF1.x: 188
*jobIdTF1.y: 8
*jobIdTF1.background: "LightSkyBlue3"
*jobIdTF1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*jobIdListPB.class: pushButton
*jobIdListPB.static: true
*jobIdListPB.name: jobIdListPB
*jobIdListPB.parent: form6
*jobIdListPB.isCompound: "true"
*jobIdListPB.compoundIcon: "push.xpm"
*jobIdListPB.compoundName: "push_Button"
*jobIdListPB.x: 332
*jobIdListPB.y: 8
*jobIdListPB.background: "LightSkyBlue3"
*jobIdListPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*jobIdListPB.labelString: "List..."
*jobIdListPB.height: 36
*jobIdListPB.shadowThickness: 3
*jobIdListPB.width: 60
*jobIdListPB.activateCallback.source: public
*jobIdListPB.activateCallback: photoJob_jobId_validsCb

*searchPB.class: pushButton
*searchPB.static: true
*searchPB.name: searchPB
*searchPB.parent: form6
*searchPB.isCompound: "true"
*searchPB.compoundIcon: "push.xpm"
*searchPB.compoundName: "push_Button"
*searchPB.x: 40
*searchPB.y: 602
*searchPB.background: "CadetBlue"
*searchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*searchPB.height: 36
*searchPB.shadowThickness: 4
*searchPB.labelString: "Search"
*searchPB.width: 108
*searchPB.activateCallback.source: public
*searchPB.activateCallback: photoJob_searchCb

*separator9.class: separator
*separator9.static: true
*separator9.name: separator9
*separator9.parent: form6
*separator9.width: 412
*separator9.height: 8
*separator9.isCompound: "true"
*separator9.compoundIcon: "sep.xpm"
*separator9.compoundName: "separator_"
*separator9.x: 0
*separator9.y: 172
*separator9.background: "#9ac0cd"

*label50.class: label
*label50.static: true
*label50.name: label50
*label50.parent: form6
*label50.isCompound: "true"
*label50.compoundIcon: "label.xpm"
*label50.compoundName: "label_"
*label50.x: 16
*label50.y: 52
*label50.background: "#9ac0cd"
*label50.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label50.labelString: "Processing Type:"
*label50.height: 24

*photoTypeTF.class: textField
*photoTypeTF.static: true
*photoTypeTF.name: photoTypeTF
*photoTypeTF.parent: form6
*photoTypeTF.width: 141
*photoTypeTF.isCompound: "true"
*photoTypeTF.compoundIcon: "textfield.xpm"
*photoTypeTF.compoundName: "text_Field"
*photoTypeTF.x: 188
*photoTypeTF.y: 48
*photoTypeTF.background: "LightSkyBlue3"
*photoTypeTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*photoTypeListPB.class: pushButton
*photoTypeListPB.static: true
*photoTypeListPB.name: photoTypeListPB
*photoTypeListPB.parent: form6
*photoTypeListPB.isCompound: "true"
*photoTypeListPB.compoundIcon: "push.xpm"
*photoTypeListPB.compoundName: "push_Button"
*photoTypeListPB.x: 332
*photoTypeListPB.y: 48
*photoTypeListPB.background: "LightSkyBlue3"
*photoTypeListPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*photoTypeListPB.labelString: "List..."
*photoTypeListPB.height: 36
*photoTypeListPB.shadowThickness: 3
*photoTypeListPB.width: 60
*photoTypeListPB.activateCallback.source: public
*photoTypeListPB.activateCallback: photoJob_photoType_validsCb

*label67.class: label
*label67.static: true
*label67.name: label67
*label67.parent: form6
*label67.isCompound: "true"
*label67.compoundIcon: "label.xpm"
*label67.compoundName: "label_"
*label67.x: 140
*label67.y: 138
*label67.background: "#9ac0cd"
*label67.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label67.labelString: "From"
*label67.height: 24

*label68.class: label
*label68.static: true
*label68.name: label68
*label68.parent: form6
*label68.isCompound: "true"
*label68.compoundIcon: "label.xpm"
*label68.compoundName: "label_"
*label68.x: 280
*label68.y: 138
*label68.background: "#9ac0cd"
*label68.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label68.labelString: "To"
*label68.height: 24

*photoTypeSW1.class: scrolledWindow
*photoTypeSW1.static: true
*photoTypeSW1.name: photoTypeSW1
*photoTypeSW1.parent: form6
*photoTypeSW1.scrollingPolicy: "automatic"
*photoTypeSW1.visualPolicy: "constant"
*photoTypeSW1.scrollBarDisplayPolicy: "as_needed"
*photoTypeSW1.shadowThickness: 0
*photoTypeSW1.isCompound: "true"
*photoTypeSW1.compoundIcon: "scrllist.xpm"
*photoTypeSW1.compoundName: "scrolled_List"
*photoTypeSW1.x: 100
*photoTypeSW1.y: 256
*photoTypeSW1.background: "LightSkyBlue3"
*photoTypeSW1.height: 336
*photoTypeSW1.width: 70

*photoTypeSL1.class: scrolledList
*photoTypeSL1.static: true
*photoTypeSL1.name: photoTypeSL1
*photoTypeSL1.parent: photoTypeSW1
*photoTypeSL1.width: 70
*photoTypeSL1.height: 336
*photoTypeSL1.background: "LightSkyBlue3"
*photoTypeSL1.listSizePolicy: "constant"
*photoTypeSL1.selectionPolicy: "browse_select"
*photoTypeSL1.x: 0
*photoTypeSL1.y: 93
*photoTypeSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*photoTypeSL1.browseSelectionCallback.source: public
*photoTypeSL1.browseSelectionCallback: photoJob_jobLists_selectionCb
*photoTypeSL1.browseSelectionCallbackClientData: (XtPointer) 2
*photoTypeSL1.defaultActionCallback.source: public
*photoTypeSL1.defaultActionCallback: photoJob_viewCb

*statusSW1.class: scrolledWindow
*statusSW1.static: true
*statusSW1.name: statusSW1
*statusSW1.parent: form6
*statusSW1.scrollingPolicy: "automatic"
*statusSW1.visualPolicy: "constant"
*statusSW1.scrollBarDisplayPolicy: "as_needed"
*statusSW1.shadowThickness: 0
*statusSW1.isCompound: "true"
*statusSW1.compoundIcon: "scrllist.xpm"
*statusSW1.compoundName: "scrolled_List"
*statusSW1.x: 280
*statusSW1.y: 256
*statusSW1.background: "LightSkyBlue3"
*statusSW1.height: 336
*statusSW1.width: 96

*statusSL1.class: scrolledList
*statusSL1.static: true
*statusSL1.name: statusSL1
*statusSL1.parent: statusSW1
*statusSL1.width: 96
*statusSL1.height: 336
*statusSL1.background: "LightSkyBlue3"
*statusSL1.listSizePolicy: "constant"
*statusSL1.selectionPolicy: "browse_select"
*statusSL1.x: 0
*statusSL1.y: 93
*statusSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*statusSL1.itemCount: 0
*statusSL1.browseSelectionCallback.source: public
*statusSL1.browseSelectionCallback: photoJob_jobLists_selectionCb
*statusSL1.browseSelectionCallbackClientData: (XtPointer) 4
*statusSL1.defaultActionCallback.source: public
*statusSL1.defaultActionCallback: photoJob_viewCb

*label69.class: label
*label69.static: true
*label69.name: label69
*label69.parent: form6
*label69.isCompound: "true"
*label69.compoundIcon: "label.xpm"
*label69.compoundName: "label_"
*label69.x: 296
*label69.y: 232
*label69.background: "#9ac0cd"
*label69.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label69.labelString: "Status"

*label70.class: label
*label70.static: true
*label70.name: label70
*label70.parent: form6
*label70.isCompound: "true"
*label70.compoundIcon: "label.xpm"
*label70.compoundName: "label_"
*label70.x: 108
*label70.y: 232
*label70.background: "#9ac0cd"
*label70.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label70.labelString: "Type"

*fromDateText.class: text
*fromDateText.static: true
*fromDateText.name: fromDateText
*fromDateText.parent: form6
*fromDateText.width: 92
*fromDateText.isCompound: "true"
*fromDateText.compoundIcon: "text.xpm"
*fromDateText.compoundName: "text_"
*fromDateText.x: 188
*fromDateText.y: 132
*fromDateText.background: "LightSkyBlue3"
*fromDateText.height: 36
*fromDateText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fromDateText.text: ""
*fromDateText.focusCallback.source: public
*fromDateText.focusCallback: photoJob_date_looseFocusCb
*fromDateText.modifyVerifyCallback.source: public
*fromDateText.modifyVerifyCallback: photoJob_check_date
*fromDateText.motionVerifyCallback.source: public
*fromDateText.motionVerifyCallback: photoJob_check_date
*fromDateText.losingFocusCallback.source: public
*fromDateText.losingFocusCallback: photoJob_date_looseFocusCb
*fromDateText.marginWidth: 1

*toDateText.class: text
*toDateText.static: true
*toDateText.name: toDateText
*toDateText.parent: form6
*toDateText.width: 92
*toDateText.isCompound: "true"
*toDateText.compoundIcon: "text.xpm"
*toDateText.compoundName: "text_"
*toDateText.x: 304
*toDateText.y: 132
*toDateText.background: "LightSkyBlue3"
*toDateText.height: 36
*toDateText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*toDateText.losingFocusCallback.source: public
*toDateText.losingFocusCallback: photoJob_date_looseFocusCb
*toDateText.modifyVerifyCallback.source: public
*toDateText.modifyVerifyCallback: photoJob_check_date
*toDateText.motionVerifyCallback.source: public
*toDateText.motionVerifyCallback: photoJob_check_date
*toDateText.marginWidth: 1

*label71.class: label
*label71.static: true
*label71.name: label71
*label71.parent: form6
*label71.isCompound: "true"
*label71.compoundIcon: "label.xpm"
*label71.compoundName: "label_"
*label71.x: 16
*label71.y: 92
*label71.background: "#9ac0cd"
*label71.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label71.labelString: "Photo Job Status:"
*label71.height: 24

*frame6.class: frame
*frame6.static: true
*frame6.name: frame6
*frame6.parent: form6
*frame6.width: 125
*frame6.height: 50
*frame6.isCompound: "true"
*frame6.compoundIcon: "frame.xpm"
*frame6.compoundName: "frame_"
*frame6.x: 10
*frame6.y: 120
*frame6.background: "#9ac0cd"
*frame6.shadowType: "shadow_etched_out"

*label64.class: label
*label64.static: true
*label64.name: label64
*label64.parent: frame6
*label64.isCompound: "true"
*label64.compoundIcon: "label.xpm"
*label64.compoundName: "label_"
*label64.x: 0
*label64.y: 4
*label64.background: "#9ac0cd"
*label64.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label64.labelString: "Start Date:\nYYYY-MM-DD"
*label64.height: 54
*label64.width: 110

*separator11.class: separator
*separator11.static: true
*separator11.name: separator11
*separator11.parent: form6
*separator11.width: 412
*separator11.height: 8
*separator11.isCompound: "true"
*separator11.compoundIcon: "sep.xpm"
*separator11.compoundName: "separator_"
*separator11.x: 0
*separator11.y: 216
*separator11.background: "#9ac0cd"

*label65.class: label
*label65.static: true
*label65.name: label65
*label65.parent: form6
*label65.isCompound: "true"
*label65.compoundIcon: "label.xpm"
*label65.compoundName: "label_"
*label65.x: 76
*label65.y: 188
*label65.background: "#9ac0cd"
*label65.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label65.labelString: "Total Jobs:"
*label65.height: 24

*totalJobsTF.class: textField
*totalJobsTF.static: true
*totalJobsTF.name: totalJobsTF
*totalJobsTF.parent: form6
*totalJobsTF.width: 92
*totalJobsTF.isCompound: "true"
*totalJobsTF.compoundIcon: "textfield.xpm"
*totalJobsTF.compoundName: "text_Field"
*totalJobsTF.x: 188
*totalJobsTF.y: 182
*totalJobsTF.background: "LightSkyBlue3"
*totalJobsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*totalJobsTF.editable: "false"
*totalJobsTF.height: 32
*totalJobsTF.marginHeight: 2
*totalJobsTF.marginWidth: 5

*clearPB.class: pushButton
*clearPB.static: true
*clearPB.name: clearPB
*clearPB.parent: form6
*clearPB.isCompound: "true"
*clearPB.compoundIcon: "push.xpm"
*clearPB.compoundName: "push_Button"
*clearPB.x: 260
*clearPB.y: 602
*clearPB.background: "CadetBlue"
*clearPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*clearPB.height: 36
*clearPB.shadowThickness: 4
*clearPB.labelString: "Clear"
*clearPB.width: 108
*clearPB.activateCallback.source: public
*clearPB.activateCallback: photoJob_clearCb

*label49.class: label
*label49.static: true
*label49.name: label49
*label49.parent: form6
*label49.isCompound: "true"
*label49.compoundIcon: "label.xpm"
*label49.compoundName: "label_"
*label49.x: 20
*label49.y: 232
*label49.background: "#9ac0cd"
*label49.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label49.labelString: "Job ID"

*statusOptionMenu.class: rowColumn
*statusOptionMenu.static: true
*statusOptionMenu.name: statusOptionMenu
*statusOptionMenu.parent: form6
*statusOptionMenu.rowColumnType: "menu_option"
*statusOptionMenu.subMenuId: "statusOM_pane"
*statusOptionMenu.isCompound: "true"
*statusOptionMenu.compoundIcon: "optionM.xpm"
*statusOptionMenu.compoundName: "option_Menu"
*statusOptionMenu.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*statusOptionMenu.x: 173
*statusOptionMenu.y: 88
*statusOptionMenu.background: "#9ac0cd"
*statusOptionMenu.width: 200
*statusOptionMenu.labelString: " "

*statusOM_pane.class: rowColumn
*statusOM_pane.static: true
*statusOM_pane.name: statusOM_pane
*statusOM_pane.parent: statusOptionMenu
*statusOM_pane.rowColumnType: "menu_pulldown"
*statusOM_pane.background: "#9ac0cd"
*statusOM_pane.width: 150
*statusOM_pane.userData: (XtPointer) 1

*allPB.class: pushButton
*allPB.static: true
*allPB.name: allPB
*allPB.parent: statusOM_pane
*allPB.labelString: "ALL"
*allPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*allPB.activateCallbackClientData: (XtPointer) 0
*allPB.background: "LightSkyBlue3"
*allPB.activateCallback.source: public
*allPB.activateCallback: photoJob_optionmenu_toggledCb

*frame5.class: frame
*frame5.static: true
*frame5.name: frame5
*frame5.parent: photoJob
*frame5.width: 530
*frame5.height: 651
*frame5.isCompound: "true"
*frame5.compoundIcon: "frame.xpm"
*frame5.compoundName: "frame_"
*frame5.x: 505
*frame5.y: 73
*frame5.background: "#9ac0cd"
*frame5.shadowThickness: 4
*frame5.shadowType: "shadow_etched_out"

*form7.class: form
*form7.static: true
*form7.name: form7
*form7.parent: frame5
*form7.width: 200
*form7.height: 200
*form7.resizePolicy: "resize_none"
*form7.isCompound: "true"
*form7.compoundIcon: "form.xpm"
*form7.compoundName: "form_"
*form7.x: -88
*form7.y: 0
*form7.background: "#9ac0cd"

*label54.class: label
*label54.static: true
*label54.name: label54
*label54.parent: form7
*label54.isCompound: "true"
*label54.compoundIcon: "label.xpm"
*label54.compoundName: "label_"
*label54.x: 16
*label54.y: 160
*label54.background: "#9ac0cd"
*label54.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label54.labelString: "Order ID"

*orderIdSW2.class: scrolledWindow
*orderIdSW2.static: true
*orderIdSW2.name: orderIdSW2
*orderIdSW2.parent: form7
*orderIdSW2.scrollingPolicy: "automatic"
*orderIdSW2.visualPolicy: "constant"
*orderIdSW2.scrollBarDisplayPolicy: "as_needed"
*orderIdSW2.shadowThickness: 0
*orderIdSW2.isCompound: "true"
*orderIdSW2.compoundIcon: "scrllist.xpm"
*orderIdSW2.compoundName: "scrolled_List"
*orderIdSW2.x: 12
*orderIdSW2.y: 184
*orderIdSW2.background: "LightSkyBlue3"
*orderIdSW2.height: 405
*orderIdSW2.width: 108

*orderIdSL2.class: scrolledList
*orderIdSL2.static: true
*orderIdSL2.name: orderIdSL2
*orderIdSL2.parent: orderIdSW2
*orderIdSL2.width: 108
*orderIdSL2.height: 405
*orderIdSL2.background: "LightSkyBlue3"
*orderIdSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderIdSL2.listSizePolicy: "constant"
*orderIdSL2.selectionPolicy: "browse_select"
*orderIdSL2.itemCount: 0
*orderIdSL2.x: 0
*orderIdSL2.y: 176
*orderIdSL2.browseSelectionCallback.source: public
*orderIdSL2.browseSelectionCallback: photoJob_queueLists_selectionCb
*orderIdSL2.browseSelectionCallbackClientData: (XtPointer) 1

*label55.class: label
*label55.static: true
*label55.name: label55
*label55.parent: form7
*label55.isCompound: "true"
*label55.compoundIcon: "label.xpm"
*label55.compoundName: "label_"
*label55.x: 272
*label55.y: 64
*label55.background: "#9ac0cd"
*label55.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label55.labelString: "Order Date:"

*orderDateTF.class: textField
*orderDateTF.static: true
*orderDateTF.name: orderDateTF
*orderDateTF.parent: form7
*orderDateTF.width: 116
*orderDateTF.isCompound: "true"
*orderDateTF.compoundIcon: "textfield.xpm"
*orderDateTF.compoundName: "text_Field"
*orderDateTF.x: 384
*orderDateTF.y: 56
*orderDateTF.background: "LightSkyBlue3"
*orderDateTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderDateTF.editable: "false"
*orderDateTF.marginWidth: 1

*label56.class: label
*label56.static: true
*label56.name: label56
*label56.parent: form7
*label56.isCompound: "true"
*label56.compoundIcon: "label.xpm"
*label56.compoundName: "label_"
*label56.x: 16
*label56.y: 64
*label56.background: "#9ac0cd"
*label56.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label56.labelString: "Work Order:"

*workOrderTF.class: textField
*workOrderTF.static: true
*workOrderTF.name: workOrderTF
*workOrderTF.parent: form7
*workOrderTF.width: 88
*workOrderTF.isCompound: "true"
*workOrderTF.compoundIcon: "textfield.xpm"
*workOrderTF.compoundName: "text_Field"
*workOrderTF.x: 124
*workOrderTF.y: 56
*workOrderTF.background: "LightSkyBlue3"
*workOrderTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*workOrderTF.editable: "false"
*workOrderTF.height: 36

*label57.class: label
*label57.static: true
*label57.name: label57
*label57.parent: form7
*label57.isCompound: "true"
*label57.compoundIcon: "label.xpm"
*label57.compoundName: "label_"
*label57.x: 100
*label57.y: 16
*label57.background: "#9ac0cd"
*label57.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label57.labelString: "Photo Job ID:"

*jobIdTF2.class: textField
*jobIdTF2.static: true
*jobIdTF2.name: jobIdTF2
*jobIdTF2.parent: form7
*jobIdTF2.width: 152
*jobIdTF2.isCompound: "true"
*jobIdTF2.compoundIcon: "textfield.xpm"
*jobIdTF2.compoundName: "text_Field"
*jobIdTF2.x: 240
*jobIdTF2.y: 8
*jobIdTF2.background: "LightSkyBlue3"
*jobIdTF2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*jobIdTF2.editable: "false"

*label58.class: label
*label58.static: true
*label58.name: label58
*label58.parent: form7
*label58.isCompound: "true"
*label58.compoundIcon: "label.xpm"
*label58.compoundName: "label_"
*label58.x: 16
*label58.y: 104
*label58.background: "#9ac0cd"
*label58.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label58.labelString: "Total Prints:"

*label59.class: label
*label59.static: true
*label59.name: label59
*label59.parent: form7
*label59.isCompound: "true"
*label59.compoundIcon: "label.xpm"
*label59.compoundName: "label_"
*label59.x: 272
*label59.y: 104
*label59.background: "#9ac0cd"
*label59.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label59.labelString: "Total Cost($):"

*totalPrintsTF.class: textField
*totalPrintsTF.static: true
*totalPrintsTF.name: totalPrintsTF
*totalPrintsTF.parent: form7
*totalPrintsTF.width: 88
*totalPrintsTF.isCompound: "true"
*totalPrintsTF.compoundIcon: "textfield.xpm"
*totalPrintsTF.compoundName: "text_Field"
*totalPrintsTF.x: 124
*totalPrintsTF.y: 96
*totalPrintsTF.background: "LightSkyBlue3"
*totalPrintsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*totalPrintsTF.editable: "true"

*totalCostTF.class: textField
*totalCostTF.static: true
*totalCostTF.name: totalCostTF
*totalCostTF.parent: form7
*totalCostTF.width: 116
*totalCostTF.isCompound: "true"
*totalCostTF.compoundIcon: "textfield.xpm"
*totalCostTF.compoundName: "text_Field"
*totalCostTF.x: 384
*totalCostTF.y: 96
*totalCostTF.background: "LightSkyBlue3"
*totalCostTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*totalCostTF.editable: "true"

*label60.class: label
*label60.static: true
*label60.name: label60
*label60.parent: form7
*label60.isCompound: "true"
*label60.compoundIcon: "label.xpm"
*label60.compoundName: "label_"
*label60.x: 120
*label60.y: 160
*label60.background: "#9ac0cd"
*label60.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label60.labelString: "Item"

*label61.class: label
*label61.static: true
*label61.name: label61
*label61.parent: form7
*label61.isCompound: "true"
*label61.compoundIcon: "label.xpm"
*label61.compoundName: "label_"
*label61.x: 172
*label61.y: 160
*label61.background: "#9ac0cd"
*label61.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label61.labelString: "Product ID"

*label62.class: label
*label62.static: true
*label62.name: label62
*label62.parent: form7
*label62.isCompound: "true"
*label62.compoundIcon: "label.xpm"
*label62.compoundName: "label_"
*label62.x: 304
*label62.y: 160
*label62.background: "#9ac0cd"
*label62.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label62.labelString: "Qty"
*label62.width: 32

*itemIdSW2.class: scrolledWindow
*itemIdSW2.static: true
*itemIdSW2.name: itemIdSW2
*itemIdSW2.parent: form7
*itemIdSW2.scrollingPolicy: "automatic"
*itemIdSW2.visualPolicy: "constant"
*itemIdSW2.scrollBarDisplayPolicy: "as_needed"
*itemIdSW2.shadowThickness: 0
*itemIdSW2.isCompound: "true"
*itemIdSW2.compoundIcon: "scrllist.xpm"
*itemIdSW2.compoundName: "scrolled_List"
*itemIdSW2.x: 124
*itemIdSW2.y: 184
*itemIdSW2.background: "LightSkyBlue3"
*itemIdSW2.height: 405
*itemIdSW2.width: 37

*itemIdSL2.class: scrolledList
*itemIdSL2.static: true
*itemIdSL2.name: itemIdSL2
*itemIdSL2.parent: itemIdSW2
*itemIdSL2.width: 37
*itemIdSL2.height: 405
*itemIdSL2.background: "LightSkyBlue3"
*itemIdSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemIdSL2.listSizePolicy: "constant"
*itemIdSL2.selectionPolicy: "browse_select"
*itemIdSL2.x: 0
*itemIdSL2.y: 176
*itemIdSL2.browseSelectionCallback.source: public
*itemIdSL2.browseSelectionCallback: photoJob_queueLists_selectionCb
*itemIdSL2.browseSelectionCallbackClientData: (XtPointer) 2

*productIdSW2.class: scrolledWindow
*productIdSW2.static: true
*productIdSW2.name: productIdSW2
*productIdSW2.parent: form7
*productIdSW2.scrollingPolicy: "automatic"
*productIdSW2.visualPolicy: "constant"
*productIdSW2.scrollBarDisplayPolicy: "as_needed"
*productIdSW2.shadowThickness: 0
*productIdSW2.isCompound: "true"
*productIdSW2.compoundIcon: "scrllist.xpm"
*productIdSW2.compoundName: "scrolled_List"
*productIdSW2.x: 164
*productIdSW2.y: 184
*productIdSW2.background: "LightSkyBlue3"
*productIdSW2.height: 405
*productIdSW2.width: 135

*productIdSL2.class: scrolledList
*productIdSL2.static: true
*productIdSL2.name: productIdSL2
*productIdSL2.parent: productIdSW2
*productIdSL2.width: 135
*productIdSL2.height: 405
*productIdSL2.background: "LightSkyBlue3"
*productIdSL2.listSizePolicy: "constant"
*productIdSL2.selectionPolicy: "browse_select"
*productIdSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*productIdSL2.x: 0
*productIdSL2.y: 176
*productIdSL2.browseSelectionCallback.source: public
*productIdSL2.browseSelectionCallback: photoJob_queueLists_selectionCb
*productIdSL2.browseSelectionCallbackClientData: (XtPointer) 3

*qtySW2.class: scrolledWindow
*qtySW2.static: true
*qtySW2.name: qtySW2
*qtySW2.parent: form7
*qtySW2.scrollingPolicy: "automatic"
*qtySW2.visualPolicy: "constant"
*qtySW2.scrollBarDisplayPolicy: "as_needed"
*qtySW2.shadowThickness: 0
*qtySW2.isCompound: "true"
*qtySW2.compoundIcon: "scrllist.xpm"
*qtySW2.compoundName: "scrolled_List"
*qtySW2.x: 304
*qtySW2.y: 184
*qtySW2.background: "LightSkyBlue3"
*qtySW2.height: 405
*qtySW2.width: 36

*qtySL2.class: scrolledList
*qtySL2.static: true
*qtySL2.name: qtySL2
*qtySL2.parent: qtySW2
*qtySL2.width: 36
*qtySL2.height: 405
*qtySL2.background: "LightSkyBlue3"
*qtySL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*qtySL2.listSizePolicy: "constant"
*qtySL2.selectionPolicy: "browse_select"
*qtySL2.x: 0
*qtySL2.y: 176
*qtySL2.browseSelectionCallback.source: public
*qtySL2.browseSelectionCallback: photoJob_queueLists_selectionCb
*qtySL2.browseSelectionCallbackClientData: (XtPointer) 4

*photoJobDummySW2.class: scrolledWindow
*photoJobDummySW2.static: true
*photoJobDummySW2.name: photoJobDummySW2
*photoJobDummySW2.parent: form7
*photoJobDummySW2.scrollingPolicy: "application_defined"
*photoJobDummySW2.visualPolicy: "variable"
*photoJobDummySW2.scrollBarDisplayPolicy: "static"
*photoJobDummySW2.shadowThickness: 0
*photoJobDummySW2.isCompound: "true"
*photoJobDummySW2.compoundIcon: "scrllist.xpm"
*photoJobDummySW2.compoundName: "scrolled_List"
*photoJobDummySW2.x: 500
*photoJobDummySW2.y: 184
*photoJobDummySW2.width: 15
*photoJobDummySW2.background: "LightSkyBlue3"
*photoJobDummySW2.height: 403

*photoJobDummySL2.class: scrolledList
*photoJobDummySL2.static: true
*photoJobDummySL2.name: photoJobDummySL2
*photoJobDummySL2.parent: photoJobDummySW2
*photoJobDummySL2.width: 2
*photoJobDummySL2.height: 380
*photoJobDummySL2.background: "LightSkyBlue3"
*photoJobDummySL2.listSizePolicy: "variable"
*photoJobDummySL2.scrollBarDisplayPolicy: "static"
*photoJobDummySL2.selectionPolicy: "browse_select"
*photoJobDummySL2.x: 0
*photoJobDummySL2.y: 176
*photoJobDummySL2.visibleItemCount: 22

*processPB.class: pushButton
*processPB.static: true
*processPB.name: processPB
*processPB.parent: form7
*processPB.isCompound: "true"
*processPB.compoundIcon: "push.xpm"
*processPB.compoundName: "push_Button"
*processPB.x: 24
*processPB.y: 600
*processPB.background: "CadetBlue"
*processPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*processPB.height: 36
*processPB.shadowThickness: 4
*processPB.labelString: "Process Job"
*processPB.width: 136
*processPB.sensitive: "false"
*processPB.activateCallback.source: public
*processPB.activateCallback: photoJob_processCb
*processPB.marginWidth: 0

*separator8.class: separator
*separator8.static: true
*separator8.name: separator8
*separator8.parent: form7
*separator8.width: 524
*separator8.height: 8
*separator8.isCompound: "true"
*separator8.compoundIcon: "sep.xpm"
*separator8.compoundName: "separator_"
*separator8.x: 0
*separator8.y: 140
*separator8.background: "#9ac0cd"

*label52.class: label
*label52.static: true
*label52.name: label52
*label52.parent: form7
*label52.isCompound: "true"
*label52.compoundIcon: "label.xpm"
*label52.compoundName: "label_"
*label52.x: 344
*label52.y: 160
*label52.background: "#9ac0cd"
*label52.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label52.labelString: "Quality"

*qualitySW2.class: scrolledWindow
*qualitySW2.static: true
*qualitySW2.name: qualitySW2
*qualitySW2.parent: form7
*qualitySW2.scrollingPolicy: "automatic"
*qualitySW2.visualPolicy: "constant"
*qualitySW2.scrollBarDisplayPolicy: "as_needed"
*qualitySW2.shadowThickness: 0
*qualitySW2.isCompound: "true"
*qualitySW2.compoundIcon: "scrllist.xpm"
*qualitySW2.compoundName: "scrolled_List"
*qualitySW2.x: 344
*qualitySW2.y: 184
*qualitySW2.background: "LightSkyBlue3"
*qualitySW2.height: 405
*qualitySW2.width: 69

*qualitySL2.class: scrolledList
*qualitySL2.static: true
*qualitySL2.name: qualitySL2
*qualitySL2.parent: qualitySW2
*qualitySL2.width: 69
*qualitySL2.height: 405
*qualitySL2.background: "LightSkyBlue3"
*qualitySL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*qualitySL2.listSizePolicy: "constant"
*qualitySL2.selectionPolicy: "browse_select"
*qualitySL2.x: 0
*qualitySL2.y: 176
*qualitySL2.browseSelectionCallback.source: public
*qualitySL2.browseSelectionCallback: photoJob_queueLists_selectionCb
*qualitySL2.browseSelectionCallbackClientData: (XtPointer) 5

*completePB.class: pushButton
*completePB.static: true
*completePB.name: completePB
*completePB.parent: form7
*completePB.isCompound: "true"
*completePB.compoundIcon: "push.xpm"
*completePB.compoundName: "push_Button"
*completePB.x: 196
*completePB.y: 600
*completePB.background: "CadetBlue"
*completePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*completePB.height: 36
*completePB.shadowThickness: 4
*completePB.labelString: "Complete Job"
*completePB.width: 130
*completePB.sensitive: "false"
*completePB.activateCallback.source: public
*completePB.activateCallback: photoJob_completeCb
*completePB.marginWidth: 0

*qualitySW1.class: scrolledWindow
*qualitySW1.static: true
*qualitySW1.name: qualitySW1
*qualitySW1.parent: form7
*qualitySW1.scrollingPolicy: "automatic"
*qualitySW1.visualPolicy: "constant"
*qualitySW1.scrollBarDisplayPolicy: "as_needed"
*qualitySW1.shadowThickness: 0
*qualitySW1.isCompound: "true"
*qualitySW1.compoundIcon: "scrllist.xpm"
*qualitySW1.compoundName: "scrolled_List"
*qualitySW1.x: 428
*qualitySW1.y: 184
*qualitySW1.background: "LightSkyBlue3"
*qualitySW1.height: 405
*qualitySW1.width: 22

*goodSL2.class: scrolledList
*goodSL2.static: true
*goodSL2.name: goodSL2
*goodSL2.parent: qualitySW1
*goodSL2.width: 22
*goodSL2.height: 405
*goodSL2.background: "LightSkyBlue3"
*goodSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*goodSL2.listSizePolicy: "constant"
*goodSL2.selectionPolicy: "browse_select"
*goodSL2.x: 0
*goodSL2.y: 176
*goodSL2.foreground: "MediumBlue"
*goodSL2.browseSelectionCallback.source: public
*goodSL2.browseSelectionCallback: photoJob_quality_validsCb
*goodSL2.browseSelectionCallbackClientData: (XtPointer) 6

*qualitySW3.class: scrolledWindow
*qualitySW3.static: true
*qualitySW3.name: qualitySW3
*qualitySW3.parent: form7
*qualitySW3.scrollingPolicy: "automatic"
*qualitySW3.visualPolicy: "constant"
*qualitySW3.scrollBarDisplayPolicy: "as_needed"
*qualitySW3.shadowThickness: 0
*qualitySW3.isCompound: "true"
*qualitySW3.compoundIcon: "scrllist.xpm"
*qualitySW3.compoundName: "scrolled_List"
*qualitySW3.x: 452
*qualitySW3.y: 184
*qualitySW3.background: "LightSkyBlue3"
*qualitySW3.height: 405
*qualitySW3.width: 22

*regenSL2.class: scrolledList
*regenSL2.static: true
*regenSL2.name: regenSL2
*regenSL2.parent: qualitySW3
*regenSL2.width: 22
*regenSL2.height: 405
*regenSL2.background: "LightSkyBlue3"
*regenSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*regenSL2.listSizePolicy: "constant"
*regenSL2.selectionPolicy: "browse_select"
*regenSL2.x: 0
*regenSL2.y: 176
*regenSL2.foreground: "MediumBlue"
*regenSL2.browseSelectionCallback.source: public
*regenSL2.browseSelectionCallback: photoJob_quality_validsCb
*regenSL2.browseSelectionCallbackClientData: (XtPointer) 7

*qualitySW4.class: scrolledWindow
*qualitySW4.static: true
*qualitySW4.name: qualitySW4
*qualitySW4.parent: form7
*qualitySW4.scrollingPolicy: "automatic"
*qualitySW4.visualPolicy: "constant"
*qualitySW4.scrollBarDisplayPolicy: "as_needed"
*qualitySW4.shadowThickness: 0
*qualitySW4.isCompound: "true"
*qualitySW4.compoundIcon: "scrllist.xpm"
*qualitySW4.compoundName: "scrolled_List"
*qualitySW4.x: 476
*qualitySW4.y: 184
*qualitySW4.background: "LightSkyBlue3"
*qualitySW4.height: 405
*qualitySW4.width: 22

*cancelSL2.class: scrolledList
*cancelSL2.static: true
*cancelSL2.name: cancelSL2
*cancelSL2.parent: qualitySW4
*cancelSL2.width: 22
*cancelSL2.height: 405
*cancelSL2.background: "LightSkyBlue3"
*cancelSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*cancelSL2.listSizePolicy: "constant"
*cancelSL2.selectionPolicy: "browse_select"
*cancelSL2.x: 0
*cancelSL2.y: 176
*cancelSL2.foreground: "MediumBlue"
*cancelSL2.browseSelectionCallback.source: public
*cancelSL2.browseSelectionCallback: photoJob_quality_validsCb
*cancelSL2.browseSelectionCallbackClientData: (XtPointer) 8

*label42.class: label
*label42.static: true
*label42.name: label42
*label42.parent: form7
*label42.isCompound: "true"
*label42.compoundIcon: "label.xpm"
*label42.compoundName: "label_"
*label42.x: 420
*label42.y: 161
*label42.background: "#9ac0cd"
*label42.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*label42.labelString: "Quality Valids"

*commentPB.class: pushButton
*commentPB.static: true
*commentPB.name: commentPB
*commentPB.parent: form7
*commentPB.isCompound: "true"
*commentPB.compoundIcon: "push.xpm"
*commentPB.compoundName: "push_Button"
*commentPB.x: 363
*commentPB.y: 600
*commentPB.background: "CadetBlue"
*commentPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentPB.height: 36
*commentPB.shadowThickness: 4
*commentPB.labelString: "Edit Comment"
*commentPB.width: 136
*commentPB.sensitive: "false"
*commentPB.activateCallback.source: public
*commentPB.activateCallback: photoJob_commentCb
*commentPB.recomputeSize: "false"
*commentPB.resizable: "false"
*commentPB.marginWidth: 0

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: photoJob
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 800
*closePB.y: 744
*closePB.background: "CadetBlue"
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.height: 36
*closePB.shadowThickness: 4
*closePB.labelString: "CLOSE    SCREEN"
*closePB.width: 180
*closePB.activateCallback.source: public
*closePB.activateCallback: photoJob_closeCb

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: photoJob
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 60
*printPB.y: 744
*printPB.background: "CadetBlue"
*printPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*printPB.height: 36
*printPB.shadowThickness: 4
*printPB.labelString: "PRINT    SCREEN"
*printPB.width: 180
*printPB.activateCallback.source: public
*printPB.activateCallback: photoJob_printCb

*viewPB.class: pushButton
*viewPB.static: true
*viewPB.name: viewPB
*viewPB.parent: photoJob
*viewPB.isCompound: "true"
*viewPB.compoundIcon: "push.xpm"
*viewPB.compoundName: "push_Button"
*viewPB.x: 424
*viewPB.y: 300
*viewPB.background: "cadetBlue"
*viewPB.labelPixmap: "/local/imsdads/app-defaults/pixmaps/arrowV"
*viewPB.labelType: "pixmap"
*viewPB.shadowThickness: 4
*viewPB.activateCallback.source: public
*viewPB.activateCallback: photoJob_viewCb
*viewPB.sensitive: "false"
*viewPB.recomputeSize: "false"
*viewPB.height: 81
*viewPB.width: 81
*viewPB.labelInsensitivePixmap: "/local/imsdads/app-defaults/pixmaps/cal.xbm"
*viewPB.highlightThickness: 2

