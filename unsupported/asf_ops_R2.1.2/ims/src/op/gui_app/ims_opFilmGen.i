! UIMX ascii 2.9 key: 6181                                                      

*filmGeneration.class: form
*filmGeneration.classinc:
*filmGeneration.classspec:
*filmGeneration.classmembers:
*filmGeneration.classconstructor:
*filmGeneration.classdestructor:
*filmGeneration.gbldecl: #include <stdio.h>\
#include <ims_opCb.h>
*filmGeneration.ispecdecl:
*filmGeneration.funcdecl: swidget create_filmGeneration(swidget UxParent)
*filmGeneration.funcname: create_filmGeneration
*filmGeneration.funcdef: "swidget", "<create_filmGeneration>(%)"
*filmGeneration.argdecl: swidget UxParent;
*filmGeneration.arglist: UxParent
*filmGeneration.arglist.UxParent: "swidget", "%UxParent%"
*filmGeneration.icode:
*filmGeneration.fcode: return(rtrn);\

*filmGeneration.auxdecl:
*filmGeneration.static: true
*filmGeneration.name: filmGeneration
*filmGeneration.parent: NO_PARENT
*filmGeneration.parentExpression: UxParent
*filmGeneration.defaultShell: transientShell
*filmGeneration.width: 1023
*filmGeneration.height: 790
*filmGeneration.resizePolicy: "resize_none"
*filmGeneration.isCompound: "true"
*filmGeneration.compoundIcon: "form.xpm"
*filmGeneration.compoundName: "form_"
*filmGeneration.x: 99
*filmGeneration.y: 50
*filmGeneration.unitType: "pixels"
*filmGeneration.background: "#9ac0cd"

*menuBar5.class: rowColumn
*menuBar5.static: true
*menuBar5.name: menuBar5
*menuBar5.parent: filmGeneration
*menuBar5.rowColumnType: "menu_bar"
*menuBar5.isCompound: "true"
*menuBar5.compoundIcon: "pulldownM.xpm"
*menuBar5.compoundName: "menu_Bar"
*menuBar5.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*menuBar5.x: -1
*menuBar5.y: 0
*menuBar5.background: "CadetBlue"
*menuBar5.height: 36
*menuBar5.menuAccelerator: "<KeyUp>F10"
*menuBar5.menuHelpWidget: "menuBar1_top_b10"
*menuBar5.rightAttachment: "attach_form"
*menuBar5.leftAttachment: "attach_form"

*menuBar_p5.class: rowColumn
*menuBar_p5.static: true
*menuBar_p5.name: menuBar_p5
*menuBar_p5.parent: menuBar5
*menuBar_p5.rowColumnType: "menu_pulldown"

*welcomeMPB.class: pushButton
*welcomeMPB.static: true
*welcomeMPB.name: welcomeMPB
*welcomeMPB.parent: menuBar_p5
*welcomeMPB.labelString: "Welcome Screen"
*welcomeMPB.background: "CadetBlue"
*welcomeMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*welcomeMPB.mnemonic: "W"
*welcomeMPB.activateCallback.source: public
*welcomeMPB.activateCallback: filmGen_goto_welcomeCb

*menuBar_p1_b13.class: separator
*menuBar_p1_b13.static: true
*menuBar_p1_b13.name: menuBar_p1_b13
*menuBar_p1_b13.parent: menuBar_p5

*closeMPB.class: pushButton
*closeMPB.static: true
*closeMPB.name: closeMPB
*closeMPB.parent: menuBar_p5
*closeMPB.labelString: "Close   Screen"
*closeMPB.mnemonic: "C"
*closeMPB.background: "cadetBlue"
*closeMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*closeMPB.activateCallback.source: public
*closeMPB.activateCallback: filmGen_closeCb

*menuBar1_p9.class: rowColumn
*menuBar1_p9.static: true
*menuBar1_p9.name: menuBar1_p9
*menuBar1_p9.parent: menuBar5
*menuBar1_p9.rowColumnType: "menu_pulldown"

*printMPB.class: pushButton
*printMPB.static: true
*printMPB.name: printMPB
*printMPB.parent: menuBar1_p9
*printMPB.labelString: "Print  Screen"
*printMPB.mnemonic: "P"
*printMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*printMPB.background: "CadetBlue"
*printMPB.activateCallback.source: public
*printMPB.activateCallback: filmGen_printCb

*menuBar5_p4.class: rowColumn
*menuBar5_p4.static: true
*menuBar5_p4.name: menuBar5_p4
*menuBar5_p4.parent: menuBar5
*menuBar5_p4.rowColumnType: "menu_pulldown"

*updateFireStatusMPB.class: pushButton
*updateFireStatusMPB.static: true
*updateFireStatusMPB.name: updateFireStatusMPB
*updateFireStatusMPB.parent: menuBar5_p4
*updateFireStatusMPB.labelString: "Update Fire Item Status"
*updateFireStatusMPB.mnemonic: "i"
*updateFireStatusMPB.activateCallback.source: public
*updateFireStatusMPB.activateCallback: filmGen_fireStatus_updateCb
*updateFireStatusMPB.background: "cadetBlue"
*updateFireStatusMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*updateFireStatusMPB.sensitive: "false"

*menuBar5_p4_b2.class: separator
*menuBar5_p4_b2.static: true
*menuBar5_p4_b2.name: menuBar5_p4_b2
*menuBar5_p4_b2.parent: menuBar5_p4

*editFireCommentMPB.class: pushButton
*editFireCommentMPB.static: true
*editFireCommentMPB.name: editFireCommentMPB
*editFireCommentMPB.parent: menuBar5_p4
*editFireCommentMPB.labelString: "Edit Fire Item Comment"
*editFireCommentMPB.mnemonic: "E"
*editFireCommentMPB.activateCallback.source: public
*editFireCommentMPB.activateCallback: filmGen_fire_commentCb
*editFireCommentMPB.background: "cadetBlue"
*editFireCommentMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*editFireCommentMPB.sensitive: "false"

*menuBar5_p4_b6.class: separator
*menuBar5_p4_b6.static: true
*menuBar5_p4_b6.name: menuBar5_p4_b6
*menuBar5_p4_b6.parent: menuBar5_p4

*fireItemRegenMPB.class: pushButton
*fireItemRegenMPB.static: true
*fireItemRegenMPB.name: fireItemRegenMPB
*fireItemRegenMPB.parent: menuBar5_p4
*fireItemRegenMPB.labelString: "Fire Item PPS Regenerate"
*fireItemRegenMPB.mnemonic: "I"
*fireItemRegenMPB.background: "cadetBlue"
*fireItemRegenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fireItemRegenMPB.sensitive: "false"
*fireItemRegenMPB.activateCallback.source: public
*fireItemRegenMPB.activateCallback: filmGen_fire_regenCb

*menuBar5_p5.class: rowColumn
*menuBar5_p5.static: true
*menuBar5_p5.name: menuBar5_p5
*menuBar5_p5.parent: menuBar5
*menuBar5_p5.rowColumnType: "menu_pulldown"

*updateLaserStatusMPB.class: pushButton
*updateLaserStatusMPB.static: true
*updateLaserStatusMPB.name: updateLaserStatusMPB
*updateLaserStatusMPB.parent: menuBar5_p5
*updateLaserStatusMPB.labelString: "Update Laser Item Status"
*updateLaserStatusMPB.mnemonic: "s"
*updateLaserStatusMPB.activateCallback.source: public
*updateLaserStatusMPB.activateCallback: filmGen_laserStatus_updateCb
*updateLaserStatusMPB.background: "cadetBlue"
*updateLaserStatusMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*updateLaserStatusMPB.sensitive: "false"

*menuBar5_p5_b2.class: separator
*menuBar5_p5_b2.static: true
*menuBar5_p5_b2.name: menuBar5_p5_b2
*menuBar5_p5_b2.parent: menuBar5_p5

*editLaserCommentMPB.class: pushButton
*editLaserCommentMPB.static: true
*editLaserCommentMPB.name: editLaserCommentMPB
*editLaserCommentMPB.parent: menuBar5_p5
*editLaserCommentMPB.labelString: "Edit Laser Item Comment"
*editLaserCommentMPB.mnemonic: "a"
*editLaserCommentMPB.activateCallback.source: public
*editLaserCommentMPB.activateCallback: filmGen_laser_commentCb
*editLaserCommentMPB.background: "cadetBlue"
*editLaserCommentMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*editLaserCommentMPB.sensitive: "false"

*menuBar5_p5_b4.class: separator
*menuBar5_p5_b4.static: true
*menuBar5_p5_b4.name: menuBar5_p5_b4
*menuBar5_p5_b4.parent: menuBar5_p5

*laserItemRegenMPB.class: pushButton
*laserItemRegenMPB.static: true
*laserItemRegenMPB.name: laserItemRegenMPB
*laserItemRegenMPB.parent: menuBar5_p5
*laserItemRegenMPB.labelString: "Laser Item PPS Regenerate"
*laserItemRegenMPB.mnemonic: "R"
*laserItemRegenMPB.background: "cadetBlue"
*laserItemRegenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*laserItemRegenMPB.sensitive: "false"
*laserItemRegenMPB.activateCallback.source: public
*laserItemRegenMPB.activateCallback: filmGen_laser_regenCb

*menuBar1_p10.class: rowColumn
*menuBar1_p10.static: true
*menuBar1_p10.name: menuBar1_p10
*menuBar1_p10.parent: menuBar5
*menuBar1_p10.rowColumnType: "menu_pulldown"

*menuBar1_p3_b5.class: pushButton
*menuBar1_p3_b5.static: true
*menuBar1_p3_b5.name: menuBar1_p3_b5
*menuBar1_p3_b5.parent: menuBar1_p10
*menuBar1_p3_b5.labelString: "No Help Available"
*menuBar1_p3_b5.background: "CadetBlue"
*menuBar1_p3_b5.activateCallback.source: public
*menuBar1_p3_b5.activateCallback: 

*menuBar_top_b5.class: cascadeButton
*menuBar_top_b5.static: true
*menuBar_top_b5.name: menuBar_top_b5
*menuBar_top_b5.parent: menuBar5
*menuBar_top_b5.labelString: "Go To"
*menuBar_top_b5.subMenuId: "menuBar_p5"
*menuBar_top_b5.mnemonic: "G"
*menuBar_top_b5.background: "CadetBlue"
*menuBar_top_b5.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_top_b5.marginWidth: 10
*menuBar_top_b5.x: 0
*menuBar_top_b5.y: 0

*menuBar1_top_b9.class: cascadeButtonGadget
*menuBar1_top_b9.static: true
*menuBar1_top_b9.name: menuBar1_top_b9
*menuBar1_top_b9.parent: menuBar5
*menuBar1_top_b9.labelString: "Screen Functions"
*menuBar1_top_b9.mnemonic: "S"
*menuBar1_top_b9.subMenuId: "menuBar1_p9"
*menuBar1_top_b9.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b9.marginWidth: 20
*menuBar1_top_b9.x: 0
*menuBar1_top_b9.y: 0

*menuBar5_top_b1.class: cascadeButton
*menuBar5_top_b1.static: true
*menuBar5_top_b1.name: menuBar5_top_b1
*menuBar5_top_b1.parent: menuBar5
*menuBar5_top_b1.labelString: "Fire Queue Functions"
*menuBar5_top_b1.mnemonic: "F"
*menuBar5_top_b1.subMenuId: "menuBar5_p4"
*menuBar5_top_b1.background: "cadetBlue"
*menuBar5_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar5_top_b1.marginLeft: 12
*menuBar5_top_b1.marginRight: 12

*menuBar5_top_b2.class: cascadeButton
*menuBar5_top_b2.static: true
*menuBar5_top_b2.name: menuBar5_top_b2
*menuBar5_top_b2.parent: menuBar5
*menuBar5_top_b2.labelString: "Laser Queue Functions"
*menuBar5_top_b2.mnemonic: "L"
*menuBar5_top_b2.subMenuId: "menuBar5_p5"
*menuBar5_top_b2.background: "cadetBlue"
*menuBar5_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar5_top_b2.marginLeft: 12
*menuBar5_top_b2.marginWidth: 10
*menuBar5_top_b2.marginRight: 12

*menuBar1_top_b10.class: cascadeButton
*menuBar1_top_b10.static: true
*menuBar1_top_b10.name: menuBar1_top_b10
*menuBar1_top_b10.parent: menuBar5
*menuBar1_top_b10.labelString: "Help"
*menuBar1_top_b10.mnemonic: "H"
*menuBar1_top_b10.subMenuId: "menuBar1_p10"
*menuBar1_top_b10.background: "CadetBlue"
*menuBar1_top_b10.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b10.x: 0
*menuBar1_top_b10.y: 0

*label73.class: label
*label73.static: true
*label73.name: label73
*label73.parent: filmGeneration
*label73.isCompound: "true"
*label73.compoundIcon: "label.xpm"
*label73.compoundName: "label_"
*label73.x: 363
*label73.y: 38
*label73.background: "#9ac0cd"
*label73.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label73.labelString: "Create     Film    TTDL    Screen"
*label73.height: 32

*separator15.class: separator
*separator15.static: true
*separator15.name: separator15
*separator15.parent: filmGeneration
*separator15.width: 1024
*separator15.height: 4
*separator15.isCompound: "true"
*separator15.compoundIcon: "sep.xpm"
*separator15.compoundName: "separator_"
*separator15.x: 0
*separator15.y: 732
*separator15.background: "#9ac0cd"

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: filmGeneration
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 48
*printPB.y: 744
*printPB.background: "CadetBlue"
*printPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*printPB.height: 36
*printPB.shadowThickness: 4
*printPB.labelString: "PRINT    SCREEN"
*printPB.width: 180
*printPB.activateCallback.source: public
*printPB.activateCallback: filmGen_printCb

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: filmGeneration
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 796
*closePB.y: 744
*closePB.background: "CadetBlue"
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.height: 36
*closePB.shadowThickness: 4
*closePB.labelString: "CLOSE    SCREEN"
*closePB.width: 180
*closePB.activateCallback.source: public
*closePB.activateCallback: filmGen_closeCb

*frame7.class: frame
*frame7.static: true
*frame7.name: frame7
*frame7.parent: filmGeneration
*frame7.width: 315
*frame7.height: 640
*frame7.isCompound: "true"
*frame7.compoundIcon: "frame.xpm"
*frame7.compoundName: "frame_"
*frame7.x: 20
*frame7.y: 81
*frame7.background: "#9ac0cd"
*frame7.shadowThickness: 4
*frame7.shadowType: "shadow_etched_out"

*form2.class: form
*form2.static: true
*form2.name: form2
*form2.parent: frame7
*form2.width: 450
*form2.height: 720
*form2.resizePolicy: "resize_none"
*form2.isCompound: "true"
*form2.compoundIcon: "form.xpm"
*form2.compoundName: "form_"
*form2.x: 4
*form2.y: 85
*form2.background: "#9ac0cd"

*label74.class: label
*label74.static: true
*label74.name: label74
*label74.parent: form2
*label74.isCompound: "true"
*label74.compoundIcon: "label.xpm"
*label74.compoundName: "label_"
*label74.x: 126
*label74.y: 154
*label74.background: "#9ac0cd"
*label74.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label74.labelString: "Item"

*label75.class: label
*label75.static: true
*label75.name: label75
*label75.parent: form2
*label75.isCompound: "true"
*label75.compoundIcon: "label.xpm"
*label75.compoundName: "label_"
*label75.x: 192
*label75.y: 154
*label75.background: "#9ac0cd"
*label75.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label75.labelString: "Status"

*fireDummySW.class: scrolledWindow
*fireDummySW.static: true
*fireDummySW.name: fireDummySW
*fireDummySW.parent: form2
*fireDummySW.scrollingPolicy: "application_defined"
*fireDummySW.visualPolicy: "variable"
*fireDummySW.scrollBarDisplayPolicy: "static"
*fireDummySW.shadowThickness: 0
*fireDummySW.isCompound: "true"
*fireDummySW.compoundIcon: "scrllist.xpm"
*fireDummySW.compoundName: "scrolled_List"
*fireDummySW.x: 280
*fireDummySW.y: 176
*fireDummySW.width: 15
*fireDummySW.background: "LightSkyBlue3"
*fireDummySW.height: 383

*fireDummySL.class: scrolledList
*fireDummySL.static: true
*fireDummySL.name: fireDummySL
*fireDummySL.parent: fireDummySW
*fireDummySL.width: 2
*fireDummySL.height: 366
*fireDummySL.background: "LightSkyBlue3"
*fireDummySL.listSizePolicy: "variable"
*fireDummySL.scrollBarDisplayPolicy: "static"
*fireDummySL.selectionPolicy: "extended_select"
*fireDummySL.x: 0
*fireDummySL.y: 181
*fireDummySL.mappedWhenManaged: "true"
*fireDummySL.visibleItemCount: 22
*fireDummySL.listSpacing: 1

*label77.class: label
*label77.static: true
*label77.name: label77
*label77.parent: form2
*label77.isCompound: "true"
*label77.compoundIcon: "label.xpm"
*label77.compoundName: "label_"
*label77.x: 52
*label77.y: 8
*label77.background: "#9ac0cd"
*label77.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label77.labelString: "Fire  Recorder  Queue"

*fireClearPB.class: pushButton
*fireClearPB.static: true
*fireClearPB.name: fireClearPB
*fireClearPB.parent: form2
*fireClearPB.isCompound: "true"
*fireClearPB.compoundIcon: "push.xpm"
*fireClearPB.compoundName: "push_Button"
*fireClearPB.x: 8
*fireClearPB.y: 590
*fireClearPB.background: "CadetBlue"
*fireClearPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fireClearPB.height: 36
*fireClearPB.shadowThickness: 4
*fireClearPB.labelString: "Clear"
*fireClearPB.width: 108
*fireClearPB.activateCallback.source: public
*fireClearPB.activateCallback: filmGen_fire_clearCb

*separator14.class: separator
*separator14.static: true
*separator14.name: separator14
*separator14.parent: form2
*separator14.width: 312
*separator14.height: 5
*separator14.isCompound: "true"
*separator14.compoundIcon: "sep.xpm"
*separator14.compoundName: "separator_"
*separator14.x: 0
*separator14.y: 36
*separator14.background: "#9ac0cd"

*label79.class: label
*label79.static: true
*label79.name: label79
*label79.parent: form2
*label79.isCompound: "true"
*label79.compoundIcon: "label.xpm"
*label79.compoundName: "label_"
*label79.x: 44
*label79.y: 108
*label79.background: "#9ac0cd"
*label79.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label79.labelString: "Total Items:"

*fireTotalItemsTF.class: textField
*fireTotalItemsTF.static: true
*fireTotalItemsTF.name: fireTotalItemsTF
*fireTotalItemsTF.parent: form2
*fireTotalItemsTF.width: 90
*fireTotalItemsTF.isCompound: "true"
*fireTotalItemsTF.compoundIcon: "textfield.xpm"
*fireTotalItemsTF.compoundName: "text_Field"
*fireTotalItemsTF.x: 156
*fireTotalItemsTF.y: 100
*fireTotalItemsTF.background: "LightSkyBlue3"
*fireTotalItemsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fireTotalItemsTF.editable: "false"
*fireTotalItemsTF.cursorPositionVisible: "false"
*fireTotalItemsTF.height: 32
*fireTotalItemsTF.marginHeight: 2
*fireTotalItemsTF.marginWidth: 8

*label72.class: label
*label72.static: true
*label72.name: label72
*label72.parent: form2
*label72.isCompound: "true"
*label72.compoundIcon: "label.xpm"
*label72.compoundName: "label_"
*label72.x: 24
*label72.y: 154
*label72.background: "#9ac0cd"
*label72.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label72.labelString: "Order ID"
*label72.height: 20

*separator18.class: separator
*separator18.static: true
*separator18.name: separator18
*separator18.parent: form2
*separator18.width: 312
*separator18.height: 5
*separator18.isCompound: "true"
*separator18.compoundIcon: "sep.xpm"
*separator18.compoundName: "separator_"
*separator18.x: 0
*separator18.y: 88
*separator18.background: "#9ac0cd"

*separator19.class: separator
*separator19.static: true
*separator19.name: separator19
*separator19.parent: form2
*separator19.width: 312
*separator19.height: 5
*separator19.isCompound: "true"
*separator19.compoundIcon: "sep.xpm"
*separator19.compoundName: "separator_"
*separator19.x: -4
*separator19.y: 140
*separator19.background: "#9ac0cd"

*label89.class: label
*label89.static: true
*label89.name: label89
*label89.parent: form2
*label89.isCompound: "true"
*label89.compoundIcon: "label.xpm"
*label89.compoundName: "label_"
*label89.x: 100
*label89.y: 54
*label89.background: "#9ac0cd"
*label89.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label89.labelString: "Status:"
*label89.height: 24

*separator21.class: separator
*separator21.static: true
*separator21.name: separator21
*separator21.parent: form2
*separator21.width: 312
*separator21.height: 5
*separator21.isCompound: "true"
*separator21.compoundIcon: "sep.xpm"
*separator21.compoundName: "separator_"
*separator21.x: -4
*separator21.y: 580
*separator21.background: "#9ac0cd"

*fireAddPB.class: pushButton
*fireAddPB.static: true
*fireAddPB.name: fireAddPB
*fireAddPB.parent: form2
*fireAddPB.isCompound: "true"
*fireAddPB.compoundIcon: "push.xpm"
*fireAddPB.compoundName: "push_Button"
*fireAddPB.x: 189
*fireAddPB.y: 590
*fireAddPB.background: "CadetBlue"
*fireAddPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fireAddPB.height: 36
*fireAddPB.shadowThickness: 4
*fireAddPB.labelString: "Add >>"
*fireAddPB.width: 108
*fireAddPB.labelPixmap: "/usr/openwin/include/X11/bitmaps/right_ptrmsk"
*fireAddPB.sensitive: "false"
*fireAddPB.activateCallback.source: public
*fireAddPB.activateCallback: filmGen_fire_addCb
*fireAddPB.activateCallbackClientData: (XtPointer) 0

*fireSearchStatusOM.class: rowColumn
*fireSearchStatusOM.static: true
*fireSearchStatusOM.name: fireSearchStatusOM
*fireSearchStatusOM.parent: form2
*fireSearchStatusOM.rowColumnType: "menu_option"
*fireSearchStatusOM.subMenuId: "fireSearchStatusOM_pane"
*fireSearchStatusOM.isCompound: "true"
*fireSearchStatusOM.compoundIcon: "optionM.xpm"
*fireSearchStatusOM.compoundName: "option_Menu"
*fireSearchStatusOM.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*fireSearchStatusOM.x: 154
*fireSearchStatusOM.y: 46
*fireSearchStatusOM.background: "#9ac0cd"
*fireSearchStatusOM.width: 200
*fireSearchStatusOM.marginWidth: 0
*fireSearchStatusOM.labelString: ""

*fireSearchStatusOM_pane.class: rowColumn
*fireSearchStatusOM_pane.static: true
*fireSearchStatusOM_pane.name: fireSearchStatusOM_pane
*fireSearchStatusOM_pane.parent: fireSearchStatusOM
*fireSearchStatusOM_pane.rowColumnType: "menu_pulldown"
*fireSearchStatusOM_pane.background: "#9ac0cd"
*fireSearchStatusOM_pane.width: 150
*fireSearchStatusOM_pane.userData: (XtPointer) 1
*fireSearchStatusOM_pane.labelString: ""

*fireDummyPB.class: pushButton
*fireDummyPB.static: true
*fireDummyPB.name: fireDummyPB
*fireDummyPB.parent: fireSearchStatusOM_pane
*fireDummyPB.labelString: "ALL"
*fireDummyPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fireDummyPB.activateCallbackClientData: (XtPointer) 0
*fireDummyPB.background: "LightSkyBlue3"
*fireDummyPB.activateCallback.source: public
*fireDummyPB.activateCallback: filmGen_optionmenu_toggledCb

*fireSearchPB.class: pushButton
*fireSearchPB.static: true
*fireSearchPB.name: fireSearchPB
*fireSearchPB.parent: form2
*fireSearchPB.isCompound: "true"
*fireSearchPB.compoundIcon: "push.xpm"
*fireSearchPB.compoundName: "push_Button"
*fireSearchPB.x: 8
*fireSearchPB.y: 46
*fireSearchPB.background: "CadetBlue"
*fireSearchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*fireSearchPB.height: 36
*fireSearchPB.shadowThickness: 4
*fireSearchPB.labelString: "Search"
*fireSearchPB.width: 85
*fireSearchPB.activateCallback.source: public
*fireSearchPB.activateCallback: filmGen_fire_searchCb

*fireOrderIdSW.class: scrolledWindow
*fireOrderIdSW.static: true
*fireOrderIdSW.name: fireOrderIdSW
*fireOrderIdSW.parent: form2
*fireOrderIdSW.scrollingPolicy: "automatic"
*fireOrderIdSW.visualPolicy: "constant"
*fireOrderIdSW.scrollBarDisplayPolicy: "as_needed"
*fireOrderIdSW.shadowThickness: 0
*fireOrderIdSW.isCompound: "true"
*fireOrderIdSW.compoundIcon: "scrllist.xpm"
*fireOrderIdSW.compoundName: "scrolled_List"
*fireOrderIdSW.x: 16
*fireOrderIdSW.y: 176
*fireOrderIdSW.background: "LightSkyBlue3"
*fireOrderIdSW.height: 388
*fireOrderIdSW.width: 108
*fireOrderIdSW.bottomAttachment: "attach_form"
*fireOrderIdSW.bottomWidget: ""
*fireOrderIdSW.topAttachment: "attach_form"
*fireOrderIdSW.topOffset: 176
*fireOrderIdSW.isResizable: "false"
*fireOrderIdSW.resizable: "false"
*fireOrderIdSW.bottomOffset: 69

*fireOrderIdSL.class: scrolledList
*fireOrderIdSL.static: true
*fireOrderIdSL.name: fireOrderIdSL
*fireOrderIdSL.parent: fireOrderIdSW
*fireOrderIdSL.width: 108
*fireOrderIdSL.height: 387
*fireOrderIdSL.background: "LightSkyBlue3"
*fireOrderIdSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*fireOrderIdSL.listSizePolicy: "constant"
*fireOrderIdSL.selectionPolicy: "browse_select"
*fireOrderIdSL.x: 0
*fireOrderIdSL.y: 181
*fireOrderIdSL.extendedSelectionCallbackClientData: (XtPointer) 1
*fireOrderIdSL.listSpacing: 1
*fireOrderIdSL.visibleItemCount: 21
*fireOrderIdSL.itemCount: 0
*fireOrderIdSL.extendedSelectionCallback.source: public
*fireOrderIdSL.extendedSelectionCallback: filmGen_fireLists_selectionCb
*fireOrderIdSL.defaultActionCallback.source: public
*fireOrderIdSL.defaultActionCallback: filmGen_fire_commentCb
*fireOrderIdSL.isResizable: "false"
*fireOrderIdSL.unitType: "pixels"

*fireItemSW.class: scrolledWindow
*fireItemSW.static: true
*fireItemSW.name: fireItemSW
*fireItemSW.parent: form2
*fireItemSW.scrollingPolicy: "automatic"
*fireItemSW.visualPolicy: "constant"
*fireItemSW.scrollBarDisplayPolicy: "as_needed"
*fireItemSW.shadowThickness: 0
*fireItemSW.isCompound: "true"
*fireItemSW.compoundIcon: "scrllist.xpm"
*fireItemSW.compoundName: "scrolled_List"
*fireItemSW.x: 128
*fireItemSW.y: 176
*fireItemSW.background: "LightSkyBlue3"
*fireItemSW.height: 388
*fireItemSW.width: 37
*fireItemSW.bottomAttachment: "attach_form"
*fireItemSW.bottomWidget: ""
*fireItemSW.topAttachment: "attach_form"
*fireItemSW.topOffset: 176
*fireItemSW.isResizable: "false"
*fireItemSW.resizable: "false"
*fireItemSW.bottomOffset: 69

*fireItemSL.class: scrolledList
*fireItemSL.static: true
*fireItemSL.name: fireItemSL
*fireItemSL.parent: fireItemSW
*fireItemSL.width: 37
*fireItemSL.height: 387
*fireItemSL.background: "LightSkyBlue3"
*fireItemSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*fireItemSL.listSizePolicy: "constant"
*fireItemSL.selectionPolicy: "browse_select"
*fireItemSL.x: 0
*fireItemSL.y: 181
*fireItemSL.extendedSelectionCallbackClientData: (XtPointer) 2
*fireItemSL.listSpacing: 1
*fireItemSL.visibleItemCount: 21
*fireItemSL.extendedSelectionCallback.source: public
*fireItemSL.extendedSelectionCallback: filmGen_fireLists_selectionCb
*fireItemSL.itemCount: 0
*fireItemSL.defaultActionCallback.source: public
*fireItemSL.defaultActionCallback: filmGen_fire_commentCb
*fireItemSL.isResizable: "false"

*fireStatusSW.class: scrolledWindow
*fireStatusSW.static: true
*fireStatusSW.name: fireStatusSW
*fireStatusSW.parent: form2
*fireStatusSW.scrollingPolicy: "automatic"
*fireStatusSW.visualPolicy: "constant"
*fireStatusSW.scrollBarDisplayPolicy: "as_needed"
*fireStatusSW.shadowThickness: 0
*fireStatusSW.isCompound: "true"
*fireStatusSW.compoundIcon: "scrllist.xpm"
*fireStatusSW.compoundName: "scrolled_List"
*fireStatusSW.x: 168
*fireStatusSW.y: 176
*fireStatusSW.background: "LightSkyBlue3"
*fireStatusSW.height: 388
*fireStatusSW.width: 110
*fireStatusSW.bottomAttachment: "attach_form"
*fireStatusSW.topAttachment: "attach_form"
*fireStatusSW.topOffset: 176
*fireStatusSW.bottomPosition: 0
*fireStatusSW.bottomOffset: 69
*fireStatusSW.isResizable: "false"
*fireStatusSW.resizable: "false"

*fireStatusSL.class: scrolledList
*fireStatusSL.static: true
*fireStatusSL.name: fireStatusSL
*fireStatusSL.parent: fireStatusSW
*fireStatusSL.width: 110
*fireStatusSL.height: 387
*fireStatusSL.background: "LightSkyBlue3"
*fireStatusSL.listSizePolicy: "constant"
*fireStatusSL.selectionPolicy: "browse_select"
*fireStatusSL.x: 0
*fireStatusSL.y: 181
*fireStatusSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*fireStatusSL.extendedSelectionCallbackClientData: (XtPointer) 3
*fireStatusSL.listSpacing: 1
*fireStatusSL.visibleItemCount: 21
*fireStatusSL.extendedSelectionCallback.source: public
*fireStatusSL.extendedSelectionCallback: filmGen_fireLists_selectionCb
*fireStatusSL.itemCount: 0
*fireStatusSL.defaultActionCallback.source: public
*fireStatusSL.defaultActionCallback: filmGen_fire_commentCb
*fireStatusSL.scrollBarDisplayPolicy: "as_needed"

*frame8.class: frame
*frame8.static: true
*frame8.name: frame8
*frame8.parent: filmGeneration
*frame8.width: 315
*frame8.height: 640
*frame8.isCompound: "true"
*frame8.compoundIcon: "frame.xpm"
*frame8.compoundName: "frame_"
*frame8.x: 689
*frame8.y: 81
*frame8.background: "#9ac0cd"
*frame8.shadowThickness: 4
*frame8.shadowType: "shadow_etched_out"

*form5.class: form
*form5.static: true
*form5.name: form5
*form5.parent: frame8
*form5.width: 450
*form5.height: 720
*form5.resizePolicy: "resize_none"
*form5.isCompound: "true"
*form5.compoundIcon: "form.xpm"
*form5.compoundName: "form_"
*form5.x: 4
*form5.y: 85
*form5.background: "#9ac0cd"

*laserOrderIdSW.class: scrolledWindow
*laserOrderIdSW.static: true
*laserOrderIdSW.name: laserOrderIdSW
*laserOrderIdSW.parent: form5
*laserOrderIdSW.scrollingPolicy: "automatic"
*laserOrderIdSW.visualPolicy: "constant"
*laserOrderIdSW.scrollBarDisplayPolicy: "as_needed"
*laserOrderIdSW.shadowThickness: 0
*laserOrderIdSW.isCompound: "true"
*laserOrderIdSW.compoundIcon: "scrllist.xpm"
*laserOrderIdSW.compoundName: "scrolled_List"
*laserOrderIdSW.x: 16
*laserOrderIdSW.y: 176
*laserOrderIdSW.background: "LightSkyBlue3"
*laserOrderIdSW.height: 388
*laserOrderIdSW.width: 108
*laserOrderIdSW.bottomAttachment: "attach_form"
*laserOrderIdSW.bottomOffset: 69

*laserOrderIdSL.class: scrolledList
*laserOrderIdSL.static: true
*laserOrderIdSL.name: laserOrderIdSL
*laserOrderIdSL.parent: laserOrderIdSW
*laserOrderIdSL.width: 108
*laserOrderIdSL.height: 388
*laserOrderIdSL.background: "LightSkyBlue3"
*laserOrderIdSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*laserOrderIdSL.listSizePolicy: "constant"
*laserOrderIdSL.selectionPolicy: "extended_select"
*laserOrderIdSL.x: 0
*laserOrderIdSL.y: 181
*laserOrderIdSL.extendedSelectionCallbackClientData: (XtPointer) 1
*laserOrderIdSL.listSpacing: 1
*laserOrderIdSL.visibleItemCount: 21
*laserOrderIdSL.extendedSelectionCallback.source: public
*laserOrderIdSL.extendedSelectionCallback: filmGen_laserLists_selectionCb
*laserOrderIdSL.defaultActionCallback.source: public
*laserOrderIdSL.defaultActionCallback: filmGen_laser_commentCb

*laserItemSW.class: scrolledWindow
*laserItemSW.static: true
*laserItemSW.name: laserItemSW
*laserItemSW.parent: form5
*laserItemSW.scrollingPolicy: "automatic"
*laserItemSW.visualPolicy: "constant"
*laserItemSW.scrollBarDisplayPolicy: "as_needed"
*laserItemSW.shadowThickness: 0
*laserItemSW.isCompound: "true"
*laserItemSW.compoundIcon: "scrllist.xpm"
*laserItemSW.compoundName: "scrolled_List"
*laserItemSW.x: 128
*laserItemSW.y: 176
*laserItemSW.background: "LightSkyBlue3"
*laserItemSW.height: 388
*laserItemSW.width: 37
*laserItemSW.bottomAttachment: "attach_form"
*laserItemSW.bottomOffset: 69

*laserItemSL.class: scrolledList
*laserItemSL.static: true
*laserItemSL.name: laserItemSL
*laserItemSL.parent: laserItemSW
*laserItemSL.width: 37
*laserItemSL.height: 388
*laserItemSL.background: "LightSkyBlue3"
*laserItemSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*laserItemSL.listSizePolicy: "constant"
*laserItemSL.selectionPolicy: "extended_select"
*laserItemSL.x: 0
*laserItemSL.y: 181
*laserItemSL.extendedSelectionCallbackClientData: (XtPointer) 2
*laserItemSL.listSpacing: 1
*laserItemSL.visibleItemCount: 21
*laserItemSL.extendedSelectionCallback.source: public
*laserItemSL.extendedSelectionCallback: filmGen_laserLists_selectionCb
*laserItemSL.defaultActionCallback.source: public
*laserItemSL.defaultActionCallback: filmGen_laser_commentCb

*laserStatusSW.class: scrolledWindow
*laserStatusSW.static: true
*laserStatusSW.name: laserStatusSW
*laserStatusSW.parent: form5
*laserStatusSW.scrollingPolicy: "automatic"
*laserStatusSW.visualPolicy: "constant"
*laserStatusSW.scrollBarDisplayPolicy: "as_needed"
*laserStatusSW.shadowThickness: 0
*laserStatusSW.isCompound: "true"
*laserStatusSW.compoundIcon: "scrllist.xpm"
*laserStatusSW.compoundName: "scrolled_List"
*laserStatusSW.x: 168
*laserStatusSW.y: 176
*laserStatusSW.background: "LightSkyBlue3"
*laserStatusSW.height: 388
*laserStatusSW.width: 110
*laserStatusSW.bottomAttachment: "attach_form"
*laserStatusSW.bottomOffset: 69

*laserStatusSL.class: scrolledList
*laserStatusSL.static: true
*laserStatusSL.name: laserStatusSL
*laserStatusSL.parent: laserStatusSW
*laserStatusSL.width: 110
*laserStatusSL.height: 388
*laserStatusSL.background: "LightSkyBlue3"
*laserStatusSL.listSizePolicy: "constant"
*laserStatusSL.selectionPolicy: "extended_select"
*laserStatusSL.x: 0
*laserStatusSL.y: 181
*laserStatusSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*laserStatusSL.extendedSelectionCallbackClientData: (XtPointer) 3
*laserStatusSL.listSpacing: 1
*laserStatusSL.visibleItemCount: 21
*laserStatusSL.extendedSelectionCallback.source: public
*laserStatusSL.extendedSelectionCallback: filmGen_laserLists_selectionCb
*laserStatusSL.defaultActionCallback.source: public
*laserStatusSL.defaultActionCallback: filmGen_laser_commentCb

*laserDummySW.class: scrolledWindow
*laserDummySW.static: true
*laserDummySW.name: laserDummySW
*laserDummySW.parent: form5
*laserDummySW.scrollingPolicy: "application_defined"
*laserDummySW.visualPolicy: "variable"
*laserDummySW.scrollBarDisplayPolicy: "static"
*laserDummySW.shadowThickness: 0
*laserDummySW.isCompound: "true"
*laserDummySW.compoundIcon: "scrllist.xpm"
*laserDummySW.compoundName: "scrolled_List"
*laserDummySW.x: 280
*laserDummySW.y: 176
*laserDummySW.width: 15
*laserDummySW.background: "LightSkyBlue3"
*laserDummySW.height: 383

*laserDummySL.class: scrolledList
*laserDummySL.static: true
*laserDummySL.name: laserDummySL
*laserDummySL.parent: laserDummySW
*laserDummySL.width: 1
*laserDummySL.height: 366
*laserDummySL.background: "LightSkyBlue3"
*laserDummySL.listSizePolicy: "variable"
*laserDummySL.scrollBarDisplayPolicy: "static"
*laserDummySL.selectionPolicy: "extended_select"
*laserDummySL.x: 0
*laserDummySL.y: 181
*laserDummySL.mappedWhenManaged: "true"
*laserDummySL.visibleItemCount: 24
*laserDummySL.listSpacing: 1

*separator16.class: separator
*separator16.static: true
*separator16.name: separator16
*separator16.parent: form5
*separator16.width: 312
*separator16.height: 5
*separator16.isCompound: "true"
*separator16.compoundIcon: "sep.xpm"
*separator16.compoundName: "separator_"
*separator16.x: 0
*separator16.y: 36
*separator16.background: "#9ac0cd"

*label82.class: label
*label82.static: true
*label82.name: label82
*label82.parent: form5
*label82.isCompound: "true"
*label82.compoundIcon: "label.xpm"
*label82.compoundName: "label_"
*label82.x: 40
*label82.y: 108
*label82.background: "#9ac0cd"
*label82.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label82.labelString: "Total Items:"

*label81.class: label
*label81.static: true
*label81.name: label81
*label81.parent: form5
*label81.isCompound: "true"
*label81.compoundIcon: "label.xpm"
*label81.compoundName: "label_"
*label81.x: 68
*label81.y: 8
*label81.background: "#9ac0cd"
*label81.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label81.labelString: "Laser  Tech  Queue"

*laserTotalItemsTF.class: textField
*laserTotalItemsTF.static: true
*laserTotalItemsTF.name: laserTotalItemsTF
*laserTotalItemsTF.parent: form5
*laserTotalItemsTF.width: 90
*laserTotalItemsTF.isCompound: "true"
*laserTotalItemsTF.compoundIcon: "textfield.xpm"
*laserTotalItemsTF.compoundName: "text_Field"
*laserTotalItemsTF.x: 152
*laserTotalItemsTF.y: 101
*laserTotalItemsTF.background: "LightSkyBlue3"
*laserTotalItemsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*laserTotalItemsTF.editable: "false"
*laserTotalItemsTF.cursorPositionVisible: "false"
*laserTotalItemsTF.height: 32
*laserTotalItemsTF.marginHeight: 2
*laserTotalItemsTF.marginWidth: 8

*laserAddPB.class: pushButton
*laserAddPB.static: true
*laserAddPB.name: laserAddPB
*laserAddPB.parent: form5
*laserAddPB.isCompound: "true"
*laserAddPB.compoundIcon: "push.xpm"
*laserAddPB.compoundName: "push_Button"
*laserAddPB.x: 16
*laserAddPB.y: 590
*laserAddPB.background: "CadetBlue"
*laserAddPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*laserAddPB.height: 36
*laserAddPB.shadowThickness: 4
*laserAddPB.labelString: "<< Add"
*laserAddPB.width: 108
*laserAddPB.sensitive: "false"
*laserAddPB.activateCallbackClientData: (XtPointer) 0
*laserAddPB.activateCallback.source: public
*laserAddPB.activateCallback: filmGen_laser_addCb

*label76.class: label
*label76.static: true
*label76.name: label76
*label76.parent: form5
*label76.isCompound: "true"
*label76.compoundIcon: "label.xpm"
*label76.compoundName: "label_"
*label76.x: 24
*label76.y: 154
*label76.background: "#9ac0cd"
*label76.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label76.labelString: "Order ID"
*label76.height: 20

*label78.class: label
*label78.static: true
*label78.name: label78
*label78.parent: form5
*label78.isCompound: "true"
*label78.compoundIcon: "label.xpm"
*label78.compoundName: "label_"
*label78.x: 128
*label78.y: 154
*label78.background: "#9ac0cd"
*label78.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label78.labelString: "Item"

*label80.class: label
*label80.static: true
*label80.name: label80
*label80.parent: form5
*label80.isCompound: "true"
*label80.compoundIcon: "label.xpm"
*label80.compoundName: "label_"
*label80.x: 196
*label80.y: 154
*label80.background: "#9ac0cd"
*label80.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label80.labelString: "Status"

*laserClearPB.class: pushButton
*laserClearPB.static: true
*laserClearPB.name: laserClearPB
*laserClearPB.parent: form5
*laserClearPB.isCompound: "true"
*laserClearPB.compoundIcon: "push.xpm"
*laserClearPB.compoundName: "push_Button"
*laserClearPB.x: 187
*laserClearPB.y: 590
*laserClearPB.background: "CadetBlue"
*laserClearPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*laserClearPB.height: 36
*laserClearPB.shadowThickness: 4
*laserClearPB.labelString: "Clear"
*laserClearPB.width: 108
*laserClearPB.activateCallback.source: public
*laserClearPB.activateCallback: filmGen_laser_clearCb

*label90.class: label
*label90.static: true
*label90.name: label90
*label90.parent: form5
*label90.isCompound: "true"
*label90.compoundIcon: "label.xpm"
*label90.compoundName: "label_"
*label90.x: 12
*label90.y: 54
*label90.background: "#9ac0cd"
*label90.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label90.labelString: "Status:"
*label90.height: 24

*laserSearchStatusOM.class: rowColumn
*laserSearchStatusOM.static: true
*laserSearchStatusOM.name: laserSearchStatusOM
*laserSearchStatusOM.parent: form5
*laserSearchStatusOM.rowColumnType: "menu_option"
*laserSearchStatusOM.subMenuId: "laserSearchStatusOM_pane"
*laserSearchStatusOM.isCompound: "true"
*laserSearchStatusOM.compoundIcon: "optionM.xpm"
*laserSearchStatusOM.compoundName: "option_Menu"
*laserSearchStatusOM.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*laserSearchStatusOM.x: 68
*laserSearchStatusOM.y: 46
*laserSearchStatusOM.background: "#9ac0cd"
*laserSearchStatusOM.width: 200
*laserSearchStatusOM.marginWidth: 0
*laserSearchStatusOM.labelString: ""

*laserSearchStatusOM_pane.class: rowColumn
*laserSearchStatusOM_pane.static: true
*laserSearchStatusOM_pane.name: laserSearchStatusOM_pane
*laserSearchStatusOM_pane.parent: laserSearchStatusOM
*laserSearchStatusOM_pane.rowColumnType: "menu_pulldown"
*laserSearchStatusOM_pane.background: "#9ac0cd"
*laserSearchStatusOM_pane.width: 150
*laserSearchStatusOM_pane.userData: (XtPointer) 3

*laserDummyPB.class: pushButton
*laserDummyPB.static: true
*laserDummyPB.name: laserDummyPB
*laserDummyPB.parent: laserSearchStatusOM_pane
*laserDummyPB.labelString: "ALL"
*laserDummyPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*laserDummyPB.activateCallbackClientData: (XtPointer) 0
*laserDummyPB.background: "LightSkyBlue3"
*laserDummyPB.activateCallback.source: public
*laserDummyPB.activateCallback: filmGen_optionmenu_toggledCb

*laserSearchPB.class: pushButton
*laserSearchPB.static: true
*laserSearchPB.name: laserSearchPB
*laserSearchPB.parent: form5
*laserSearchPB.isCompound: "true"
*laserSearchPB.compoundIcon: "push.xpm"
*laserSearchPB.compoundName: "push_Button"
*laserSearchPB.x: 214
*laserSearchPB.y: 46
*laserSearchPB.background: "CadetBlue"
*laserSearchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*laserSearchPB.height: 36
*laserSearchPB.shadowThickness: 4
*laserSearchPB.labelString: "Search"
*laserSearchPB.width: 85
*laserSearchPB.activateCallback.source: public
*laserSearchPB.activateCallback: filmGen_laser_searchCb

*separator23.class: separator
*separator23.static: true
*separator23.name: separator23
*separator23.parent: form5
*separator23.width: 312
*separator23.height: 5
*separator23.isCompound: "true"
*separator23.compoundIcon: "sep.xpm"
*separator23.compoundName: "separator_"
*separator23.x: 0
*separator23.y: 88
*separator23.background: "#9ac0cd"

*separator24.class: separator
*separator24.static: true
*separator24.name: separator24
*separator24.parent: form5
*separator24.width: 312
*separator24.height: 5
*separator24.isCompound: "true"
*separator24.compoundIcon: "sep.xpm"
*separator24.compoundName: "separator_"
*separator24.x: 0
*separator24.y: 140
*separator24.background: "#9ac0cd"

*separator26.class: separator
*separator26.static: true
*separator26.name: separator26
*separator26.parent: form5
*separator26.width: 312
*separator26.height: 5
*separator26.isCompound: "true"
*separator26.compoundIcon: "sep.xpm"
*separator26.compoundName: "separator_"
*separator26.x: 1
*separator26.y: 580
*separator26.background: "#9ac0cd"

*frame9.class: frame
*frame9.static: true
*frame9.name: frame9
*frame9.parent: filmGeneration
*frame9.width: 318
*frame9.height: 640
*frame9.isCompound: "true"
*frame9.compoundIcon: "frame.xpm"
*frame9.compoundName: "frame_"
*frame9.x: 354
*frame9.y: 81
*frame9.background: "#9ac0cd"
*frame9.shadowThickness: 4
*frame9.shadowType: "shadow_etched_out"

*form8.class: form
*form8.static: true
*form8.name: form8
*form8.parent: frame9
*form8.width: 308
*form8.height: 632
*form8.resizePolicy: "resize_none"
*form8.isCompound: "true"
*form8.compoundIcon: "form.xpm"
*form8.compoundName: "form_"
*form8.x: 16
*form8.y: 4
*form8.background: "#9ac0cd"

*label83.class: label
*label83.static: true
*label83.name: label83
*label83.parent: form8
*label83.isCompound: "true"
*label83.compoundIcon: "label.xpm"
*label83.compoundName: "label_"
*label83.x: 21
*label83.y: 100
*label83.background: "#9ac0cd"
*label83.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label83.labelString: "Order ID"
*label83.height: 20

*ttdlOrderIdSW.class: scrolledWindow
*ttdlOrderIdSW.static: true
*ttdlOrderIdSW.name: ttdlOrderIdSW
*ttdlOrderIdSW.parent: form8
*ttdlOrderIdSW.scrollingPolicy: "automatic"
*ttdlOrderIdSW.visualPolicy: "constant"
*ttdlOrderIdSW.scrollBarDisplayPolicy: "as_needed"
*ttdlOrderIdSW.shadowThickness: 0
*ttdlOrderIdSW.isCompound: "true"
*ttdlOrderIdSW.compoundIcon: "scrllist.xpm"
*ttdlOrderIdSW.compoundName: "scrolled_List"
*ttdlOrderIdSW.x: 10
*ttdlOrderIdSW.y: 124
*ttdlOrderIdSW.background: "LightSkyBlue3"
*ttdlOrderIdSW.height: 440
*ttdlOrderIdSW.width: 106

*ttdlOrderIdSL.class: scrolledList
*ttdlOrderIdSL.static: true
*ttdlOrderIdSL.name: ttdlOrderIdSL
*ttdlOrderIdSL.parent: ttdlOrderIdSW
*ttdlOrderIdSL.width: 106
*ttdlOrderIdSL.height: 440
*ttdlOrderIdSL.background: "LightSkyBlue3"
*ttdlOrderIdSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*ttdlOrderIdSL.listSizePolicy: "constant"
*ttdlOrderIdSL.selectionPolicy: "extended_select"
*ttdlOrderIdSL.x: 0
*ttdlOrderIdSL.y: 181
*ttdlOrderIdSL.extendedSelectionCallbackClientData: (XtPointer) 1
*ttdlOrderIdSL.listSpacing: 1
*ttdlOrderIdSL.visibleItemCount: 24
*ttdlOrderIdSL.itemCount: 0
*ttdlOrderIdSL.extendedSelectionCallback.source: public
*ttdlOrderIdSL.extendedSelectionCallback: filmGen_ttdlLists_selectionCb

*label84.class: label
*label84.static: true
*label84.name: label84
*label84.parent: form8
*label84.isCompound: "true"
*label84.compoundIcon: "label.xpm"
*label84.compoundName: "label_"
*label84.x: 120
*label84.y: 100
*label84.background: "#9ac0cd"
*label84.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label84.labelString: "Item"

*ttdlItemSW.class: scrolledWindow
*ttdlItemSW.static: true
*ttdlItemSW.name: ttdlItemSW
*ttdlItemSW.parent: form8
*ttdlItemSW.scrollingPolicy: "automatic"
*ttdlItemSW.visualPolicy: "constant"
*ttdlItemSW.scrollBarDisplayPolicy: "as_needed"
*ttdlItemSW.shadowThickness: 0
*ttdlItemSW.isCompound: "true"
*ttdlItemSW.compoundIcon: "scrllist.xpm"
*ttdlItemSW.compoundName: "scrolled_List"
*ttdlItemSW.x: 120
*ttdlItemSW.y: 124
*ttdlItemSW.background: "LightSkyBlue3"
*ttdlItemSW.height: 440
*ttdlItemSW.width: 37

*ttdlItemSL.class: scrolledList
*ttdlItemSL.static: true
*ttdlItemSL.name: ttdlItemSL
*ttdlItemSL.parent: ttdlItemSW
*ttdlItemSL.width: 37
*ttdlItemSL.height: 440
*ttdlItemSL.background: "LightSkyBlue3"
*ttdlItemSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*ttdlItemSL.listSizePolicy: "constant"
*ttdlItemSL.selectionPolicy: "extended_select"
*ttdlItemSL.x: 0
*ttdlItemSL.y: 181
*ttdlItemSL.extendedSelectionCallbackClientData: (XtPointer) 2
*ttdlItemSL.listSpacing: 1
*ttdlItemSL.visibleItemCount: 24
*ttdlItemSL.extendedSelectionCallback.source: public
*ttdlItemSL.extendedSelectionCallback: filmGen_ttdlLists_selectionCb

*label85.class: label
*label85.static: true
*label85.name: label85
*label85.parent: form8
*label85.isCompound: "true"
*label85.compoundIcon: "label.xpm"
*label85.compoundName: "label_"
*label85.x: 174
*label85.y: 100
*label85.background: "#9ac0cd"
*label85.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label85.labelString: "Queue Type"

*ttdlQueueTypeSW.class: scrolledWindow
*ttdlQueueTypeSW.static: true
*ttdlQueueTypeSW.name: ttdlQueueTypeSW
*ttdlQueueTypeSW.parent: form8
*ttdlQueueTypeSW.scrollingPolicy: "automatic"
*ttdlQueueTypeSW.visualPolicy: "constant"
*ttdlQueueTypeSW.scrollBarDisplayPolicy: "as_needed"
*ttdlQueueTypeSW.shadowThickness: 0
*ttdlQueueTypeSW.isCompound: "true"
*ttdlQueueTypeSW.compoundIcon: "scrllist.xpm"
*ttdlQueueTypeSW.compoundName: "scrolled_List"
*ttdlQueueTypeSW.x: 160
*ttdlQueueTypeSW.y: 124
*ttdlQueueTypeSW.background: "LightSkyBlue3"
*ttdlQueueTypeSW.height: 440
*ttdlQueueTypeSW.width: 126

*ttdlQueueTypeSL.class: scrolledList
*ttdlQueueTypeSL.static: true
*ttdlQueueTypeSL.name: ttdlQueueTypeSL
*ttdlQueueTypeSL.parent: ttdlQueueTypeSW
*ttdlQueueTypeSL.width: 126
*ttdlQueueTypeSL.height: 440
*ttdlQueueTypeSL.background: "LightSkyBlue3"
*ttdlQueueTypeSL.listSizePolicy: "constant"
*ttdlQueueTypeSL.selectionPolicy: "extended_select"
*ttdlQueueTypeSL.x: 0
*ttdlQueueTypeSL.y: 181
*ttdlQueueTypeSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*ttdlQueueTypeSL.extendedSelectionCallbackClientData: (XtPointer) 3
*ttdlQueueTypeSL.listSpacing: 1
*ttdlQueueTypeSL.visibleItemCount: 24
*ttdlQueueTypeSL.itemCount: 0
*ttdlQueueTypeSL.extendedSelectionCallback.source: public
*ttdlQueueTypeSL.extendedSelectionCallback: filmGen_ttdlLists_selectionCb

*ttdlDummySW.class: scrolledWindow
*ttdlDummySW.static: true
*ttdlDummySW.name: ttdlDummySW
*ttdlDummySW.parent: form8
*ttdlDummySW.scrollingPolicy: "application_defined"
*ttdlDummySW.visualPolicy: "variable"
*ttdlDummySW.scrollBarDisplayPolicy: "static"
*ttdlDummySW.shadowThickness: 0
*ttdlDummySW.isCompound: "true"
*ttdlDummySW.compoundIcon: "scrllist.xpm"
*ttdlDummySW.compoundName: "scrolled_List"
*ttdlDummySW.x: 288
*ttdlDummySW.y: 124
*ttdlDummySW.width: 15
*ttdlDummySW.background: "LightSkyBlue3"
*ttdlDummySW.height: 436

*ttdlDummySL.class: scrolledList
*ttdlDummySL.static: true
*ttdlDummySL.name: ttdlDummySL
*ttdlDummySL.parent: ttdlDummySW
*ttdlDummySL.width: 1
*ttdlDummySL.height: 420
*ttdlDummySL.background: "LightSkyBlue3"
*ttdlDummySL.listSizePolicy: "variable"
*ttdlDummySL.scrollBarDisplayPolicy: "static"
*ttdlDummySL.selectionPolicy: "extended_select"
*ttdlDummySL.x: 0
*ttdlDummySL.y: 181
*ttdlDummySL.mappedWhenManaged: "true"
*ttdlDummySL.visibleItemCount: 24
*ttdlDummySL.listSpacing: 1

*label86.class: label
*label86.static: true
*label86.name: label86
*label86.parent: form8
*label86.isCompound: "true"
*label86.compoundIcon: "label.xpm"
*label86.compoundName: "label_"
*label86.x: 88
*label86.y: 8
*label86.background: "#9ac0cd"
*label86.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label86.labelString: "TTDL     Queue"

*separator17.class: separator
*separator17.static: true
*separator17.name: separator17
*separator17.parent: form8
*separator17.width: 312
*separator17.height: 5
*separator17.isCompound: "true"
*separator17.compoundIcon: "sep.xpm"
*separator17.compoundName: "separator_"
*separator17.x: 0
*separator17.y: 36
*separator17.background: "#9ac0cd"

*label87.class: label
*label87.static: true
*label87.name: label87
*label87.parent: form8
*label87.isCompound: "true"
*label87.compoundIcon: "label.xpm"
*label87.compoundName: "label_"
*label87.x: 40
*label87.y: 54
*label87.background: "#9ac0cd"
*label87.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label87.labelString: "Total Items:"

*ttdlTotalItemsTF.class: textField
*ttdlTotalItemsTF.static: true
*ttdlTotalItemsTF.name: ttdlTotalItemsTF
*ttdlTotalItemsTF.parent: form8
*ttdlTotalItemsTF.width: 90
*ttdlTotalItemsTF.isCompound: "true"
*ttdlTotalItemsTF.compoundIcon: "textfield.xpm"
*ttdlTotalItemsTF.compoundName: "text_Field"
*ttdlTotalItemsTF.x: 156
*ttdlTotalItemsTF.y: 47
*ttdlTotalItemsTF.background: "LightSkyBlue3"
*ttdlTotalItemsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ttdlTotalItemsTF.editable: "false"
*ttdlTotalItemsTF.cursorPositionVisible: "false"
*ttdlTotalItemsTF.height: 32
*ttdlTotalItemsTF.marginWidth: 8
*ttdlTotalItemsTF.marginHeight: 2

*ttdlDeletePB.class: pushButton
*ttdlDeletePB.static: true
*ttdlDeletePB.name: ttdlDeletePB
*ttdlDeletePB.parent: form8
*ttdlDeletePB.isCompound: "true"
*ttdlDeletePB.compoundIcon: "push.xpm"
*ttdlDeletePB.compoundName: "push_Button"
*ttdlDeletePB.x: 20
*ttdlDeletePB.y: 590
*ttdlDeletePB.background: "CadetBlue"
*ttdlDeletePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ttdlDeletePB.height: 36
*ttdlDeletePB.shadowThickness: 4
*ttdlDeletePB.labelString: "Delete"
*ttdlDeletePB.width: 108
*ttdlDeletePB.sensitive: "false"
*ttdlDeletePB.activateCallback.source: public
*ttdlDeletePB.activateCallback: filmGen_ttdl_deleteCb

*ttdlProcessPB.class: pushButton
*ttdlProcessPB.static: true
*ttdlProcessPB.name: ttdlProcessPB
*ttdlProcessPB.parent: form8
*ttdlProcessPB.isCompound: "true"
*ttdlProcessPB.compoundIcon: "push.xpm"
*ttdlProcessPB.compoundName: "push_Button"
*ttdlProcessPB.x: 182
*ttdlProcessPB.y: 590
*ttdlProcessPB.background: "CadetBlue"
*ttdlProcessPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ttdlProcessPB.height: 36
*ttdlProcessPB.shadowThickness: 4
*ttdlProcessPB.labelString: "Process"
*ttdlProcessPB.width: 108
*ttdlProcessPB.sensitive: "false"
*ttdlProcessPB.activateCallback.source: public
*ttdlProcessPB.activateCallback: filmGen_ttdl_processCb

*separator22.class: separator
*separator22.static: true
*separator22.name: separator22
*separator22.parent: form8
*separator22.width: 312
*separator22.height: 5
*separator22.isCompound: "true"
*separator22.compoundIcon: "sep.xpm"
*separator22.compoundName: "separator_"
*separator22.x: 0
*separator22.y: 88
*separator22.background: "#9ac0cd"

*separator27.class: separator
*separator27.static: true
*separator27.name: separator27
*separator27.parent: form8
*separator27.width: 312
*separator27.height: 5
*separator27.isCompound: "true"
*separator27.compoundIcon: "sep.xpm"
*separator27.compoundName: "separator_"
*separator27.x: -3
*separator27.y: 580
*separator27.background: "#9ac0cd"

