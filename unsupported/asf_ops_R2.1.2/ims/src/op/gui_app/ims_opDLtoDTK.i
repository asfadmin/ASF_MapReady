! UIMX ascii 2.9 key: 2246                                                      

*DLtoDTK.class: form
*DLtoDTK.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*DLtoDTK.funcdecl: swidget create_DLtoDTK(swidget UxParent)
*DLtoDTK.funcname: create_DLtoDTK
*DLtoDTK.funcdef: "swidget", "<create_DLtoDTK>(%)"
*DLtoDTK.argdecl: swidget UxParent;
*DLtoDTK.arglist: UxParent
*DLtoDTK.arglist.UxParent: "swidget", "%UxParent%"
*DLtoDTK.fcode: return(rtrn);\

*DLtoDTK.static: true
*DLtoDTK.name: DLtoDTK
*DLtoDTK.parent: NO_PARENT
*DLtoDTK.parentExpression: UxParent
*DLtoDTK.defaultShell: topLevelShell
*DLtoDTK.width: 891
*DLtoDTK.height: 817
*DLtoDTK.resizePolicy: "resize_none"
*DLtoDTK.isCompound: "true"
*DLtoDTK.compoundIcon: "form.xpm"
*DLtoDTK.compoundName: "form_"
*DLtoDTK.x: 120
*DLtoDTK.y: 60
*DLtoDTK.unitType: "pixels"
*DLtoDTK.background: "#9ac0cd"
*DLtoDTK.noResize: "true"
*DLtoDTK.initialFocus: "dtkProcAuthFRAME"

*downlinkMB.class: rowColumn
*downlinkMB.static: true
*downlinkMB.name: downlinkMB
*downlinkMB.parent: DLtoDTK
*downlinkMB.rowColumnType: "menu_bar"
*downlinkMB.isCompound: "true"
*downlinkMB.compoundIcon: "pulldownM.xpm"
*downlinkMB.compoundName: "menu_Bar"
*downlinkMB.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*downlinkMB.x: 0
*downlinkMB.y: 1
*downlinkMB.menuAccelerator: "<KeyUp>F10"
*downlinkMB.menuHelpWidget: "downlinkMB_p3_top_b2"
*downlinkMB.background: "CadetBlue"
*downlinkMB.shadowThickness: 3
*downlinkMB.width: 1500
*downlinkMB.rightAttachment: "attach_form"
*downlinkMB.leftAttachment: "attach_form"

*gotoP.class: rowColumn
*gotoP.static: true
*gotoP.name: gotoP
*gotoP.parent: downlinkMB
*gotoP.rowColumnType: "menu_pulldown"

*welcomeScreenMPB.class: pushButton
*welcomeScreenMPB.static: true
*welcomeScreenMPB.name: welcomeScreenMPB
*welcomeScreenMPB.parent: gotoP
*welcomeScreenMPB.labelString: "Welcome Screen"
*welcomeScreenMPB.background: "CadetBlue"
*welcomeScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*welcomeScreenMPB.activateCallback.source: public
*welcomeScreenMPB.activateCallback: dl2dtk_goto_welcomeCb
*welcomeScreenMPB.mnemonic: "W"

*downlinkMB_p1_b3.class: separator
*downlinkMB_p1_b3.static: true
*downlinkMB_p1_b3.name: downlinkMB_p1_b3
*downlinkMB_p1_b3.parent: gotoP

*searchScreenMPB.class: pushButton
*searchScreenMPB.static: true
*searchScreenMPB.name: searchScreenMPB
*searchScreenMPB.parent: gotoP
*searchScreenMPB.labelString: "Downlink Search Screen"
*searchScreenMPB.background: "CadetBlue"
*searchScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*searchScreenMPB.activateCallback.source: public
*searchScreenMPB.activateCallback: dl2dtk_goto_searchCb
*searchScreenMPB.mnemonic: "D"

*downlinkMB_p1_b4.class: separator
*downlinkMB_p1_b4.static: true
*downlinkMB_p1_b4.name: downlinkMB_p1_b4
*downlinkMB_p1_b4.parent: gotoP

*closeScreenMPB.class: pushButton
*closeScreenMPB.static: true
*closeScreenMPB.name: closeScreenMPB
*closeScreenMPB.parent: gotoP
*closeScreenMPB.labelString: "Close  Screen"
*closeScreenMPB.background: "CadetBlue"
*closeScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*closeScreenMPB.activateCallback.source: public
*closeScreenMPB.activateCallback: dl2dtk_closeCb
*closeScreenMPB.mnemonic: "C"

*helpP.class: rowColumn
*helpP.static: true
*helpP.name: helpP
*helpP.parent: downlinkMB
*helpP.rowColumnType: "menu_pulldown"

*downlinkMB_p3_b2.class: pushButton
*downlinkMB_p3_b2.static: true
*downlinkMB_p3_b2.name: downlinkMB_p3_b2
*downlinkMB_p3_b2.parent: helpP
*downlinkMB_p3_b2.labelString: "No Help Available"
*downlinkMB_p3_b2.background: "cadetBlue"
*downlinkMB_p3_b2.mnemonic: "N"
*downlinkMB_p3_b2.activateCallback.source: public
*downlinkMB_p3_b2.activateCallback: 

*downlinkP.class: rowColumn
*downlinkP.static: true
*downlinkP.name: downlinkP
*downlinkP.parent: downlinkMB
*downlinkP.rowColumnType: "menu_pulldown"

*viewDownlinkDetailsMPB.class: pushButton
*viewDownlinkDetailsMPB.static: true
*viewDownlinkDetailsMPB.name: viewDownlinkDetailsMPB
*viewDownlinkDetailsMPB.parent: downlinkP
*viewDownlinkDetailsMPB.labelString: "View Downlink Details"
*viewDownlinkDetailsMPB.background: "CadetBlue"
*viewDownlinkDetailsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*viewDownlinkDetailsMPB.activateCallback.source: public
*viewDownlinkDetailsMPB.activateCallback: dl2dtk_view_DL_detailsCb
*viewDownlinkDetailsMPB.sensitive: "false"
*viewDownlinkDetailsMPB.mnemonic: "V"

*downlinkP_Sep1.class: separator
*downlinkP_Sep1.static: true
*downlinkP_Sep1.name: downlinkP_Sep1
*downlinkP_Sep1.parent: downlinkP

*viewDTKsMPB.class: pushButton
*viewDTKsMPB.static: true
*viewDTKsMPB.name: viewDTKsMPB
*viewDTKsMPB.parent: downlinkP
*viewDTKsMPB.labelString: "List Data-takes"
*viewDTKsMPB.mnemonic: "L"
*viewDTKsMPB.activateCallback.source: public
*viewDTKsMPB.activateCallback: dl2dtk_show_downlinkDTKsCb
*viewDTKsMPB.background: "CadetBlue"
*viewDTKsMPB.sensitive: "false"
*viewDTKsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*datatakeP.class: rowColumn
*datatakeP.static: true
*datatakeP.name: datatakeP
*datatakeP.parent: downlinkMB
*datatakeP.rowColumnType: "menu_pulldown"

*viewDTKDetailsMPB.class: pushButton
*viewDTKDetailsMPB.static: true
*viewDTKDetailsMPB.name: viewDTKDetailsMPB
*viewDTKDetailsMPB.parent: datatakeP
*viewDTKDetailsMPB.labelString: "View Data-take Details"
*viewDTKDetailsMPB.background: "CadetBlue"
*viewDTKDetailsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*viewDTKDetailsMPB.sensitive: "false"
*viewDTKDetailsMPB.activateCallback.source: public
*viewDTKDetailsMPB.activateCallback: dl2dtk_view_DTK_detailsCb
*viewDTKDetailsMPB.mnemonic: "V"

*downlinkMB_p5_b1.class: separator
*downlinkMB_p5_b1.static: true
*downlinkMB_p5_b1.name: downlinkMB_p5_b1
*downlinkMB_p5_b1.parent: datatakeP

*toggleProcAuthFlagMPB.class: pushButton
*toggleProcAuthFlagMPB.static: true
*toggleProcAuthFlagMPB.name: toggleProcAuthFlagMPB
*toggleProcAuthFlagMPB.parent: datatakeP
*toggleProcAuthFlagMPB.labelString: "Toggle Proc. Auth."
*toggleProcAuthFlagMPB.background: "cadetBlue"
*toggleProcAuthFlagMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*toggleProcAuthFlagMPB.sensitive: "false"
*toggleProcAuthFlagMPB.activateCallback.source: public
*toggleProcAuthFlagMPB.activateCallback: dl2dtk_toggle_proc_auth_flagCb
*toggleProcAuthFlagMPB.activateCallbackClientData: (XtPointer) 0
*toggleProcAuthFlagMPB.mnemonic: "T"

*downlinkMB_p5_b2.class: separator
*downlinkMB_p5_b2.static: true
*downlinkMB_p5_b2.name: downlinkMB_p5_b2
*downlinkMB_p5_b2.parent: datatakeP

*resetDTK_MPB.class: pushButton
*resetDTK_MPB.static: true
*resetDTK_MPB.name: resetDTK_MPB
*resetDTK_MPB.parent: datatakeP
*resetDTK_MPB.labelString: "Reset Data-take(s)"
*resetDTK_MPB.background: "cadetBlue"
*resetDTK_MPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*resetDTK_MPB.activateCallback.source: public
*resetDTK_MPB.activateCallback: dl2dtk_reset_dtkCb
*resetDTK_MPB.sensitive: "false"
*resetDTK_MPB.mnemonic: "R"

*screenFuncP.class: rowColumn
*screenFuncP.static: true
*screenFuncP.name: screenFuncP
*screenFuncP.parent: downlinkMB
*screenFuncP.rowColumnType: "menu_pulldown"
*screenFuncP.width: 114

*refreshSearchMPB.class: pushButton
*refreshSearchMPB.static: true
*refreshSearchMPB.name: refreshSearchMPB
*refreshSearchMPB.parent: screenFuncP
*refreshSearchMPB.labelString: "Refresh Search"
*refreshSearchMPB.background: "cadetBlue"
*refreshSearchMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*refreshSearchMPB.activateCallback.source: public
*refreshSearchMPB.activateCallback: dl2dtk_refreshSearchCb
*refreshSearchMPB.mnemonic: "R"
*refreshSearchMPB.positionIndex: 0
*refreshSearchMPB.sensitive: "false"

*downlinkMB_p10_b1.class: separator
*downlinkMB_p10_b1.static: true
*downlinkMB_p10_b1.name: downlinkMB_p10_b1
*downlinkMB_p10_b1.parent: screenFuncP

*saveChangesMPB.class: pushButton
*saveChangesMPB.static: true
*saveChangesMPB.name: saveChangesMPB
*saveChangesMPB.parent: screenFuncP
*saveChangesMPB.labelString: "Update"
*saveChangesMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*saveChangesMPB.background: "cadetBlue"
*saveChangesMPB.marginWidth: 5
*saveChangesMPB.activateCallback.source: public
*saveChangesMPB.activateCallback: dl2dtk_updateCb
*saveChangesMPB.mnemonic: "U"
*saveChangesMPB.positionIndex: 2
*saveChangesMPB.sensitive: "false"

*downlinkMB_p10_b3.class: separator
*downlinkMB_p10_b3.static: true
*downlinkMB_p10_b3.name: downlinkMB_p10_b3
*downlinkMB_p10_b3.parent: screenFuncP

*printScreenMPB.class: pushButton
*printScreenMPB.static: true
*printScreenMPB.name: printScreenMPB
*printScreenMPB.parent: screenFuncP
*printScreenMPB.labelString: "Print Screen"
*printScreenMPB.background: "cadetBlue"
*printScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*printScreenMPB.activateCallback.source: public
*printScreenMPB.activateCallback: dl2dtk_printScreenCb
*printScreenMPB.mnemonic: "P"

*downlinkMB_p1_top_b2.class: cascadeButton
*downlinkMB_p1_top_b2.static: true
*downlinkMB_p1_top_b2.name: downlinkMB_p1_top_b2
*downlinkMB_p1_top_b2.parent: downlinkMB
*downlinkMB_p1_top_b2.labelString: "Go To"
*downlinkMB_p1_top_b2.subMenuId: "gotoP"
*downlinkMB_p1_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkMB_p1_top_b2.background: "CadetBlue"
*downlinkMB_p1_top_b2.marginWidth: 12
*downlinkMB_p1_top_b2.mnemonic: "G"
*downlinkMB_p1_top_b2.x: 0
*downlinkMB_p1_top_b2.y: 0

*downlinkMB_p3_top_b2.class: cascadeButton
*downlinkMB_p3_top_b2.static: true
*downlinkMB_p3_top_b2.name: downlinkMB_p3_top_b2
*downlinkMB_p3_top_b2.parent: downlinkMB
*downlinkMB_p3_top_b2.labelString: "Help"
*downlinkMB_p3_top_b2.subMenuId: "helpP"
*downlinkMB_p3_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkMB_p3_top_b2.background: "CadetBlue"
*downlinkMB_p3_top_b2.x: 0
*downlinkMB_p3_top_b2.y: 0
*downlinkMB_p3_top_b2.mnemonic: "H"

*downlinkMB_top_b3.class: cascadeButtonGadget
*downlinkMB_top_b3.static: true
*downlinkMB_top_b3.name: downlinkMB_top_b3
*downlinkMB_top_b3.parent: downlinkMB
*downlinkMB_top_b3.labelString: "Downlink Functions"
*downlinkMB_top_b3.subMenuId: "downlinkP"
*downlinkMB_top_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkMB_top_b3.mnemonic: "o"
*downlinkMB_top_b3.marginWidth: 20
*downlinkMB_top_b3.x: 0
*downlinkMB_top_b3.y: 0

*downlinkMB_top_b5.class: cascadeButtonGadget
*downlinkMB_top_b5.static: true
*downlinkMB_top_b5.name: downlinkMB_top_b5
*downlinkMB_top_b5.parent: downlinkMB
*downlinkMB_top_b5.labelString: "Data-take Functions"
*downlinkMB_top_b5.mnemonic: "a"
*downlinkMB_top_b5.subMenuId: "datatakeP"
*downlinkMB_top_b5.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkMB_top_b5.marginWidth: 20
*downlinkMB_top_b5.x: 0
*downlinkMB_top_b5.y: 0

*downlinkMB_top_b6.class: cascadeButtonGadget
*downlinkMB_top_b6.static: true
*downlinkMB_top_b6.name: downlinkMB_top_b6
*downlinkMB_top_b6.parent: downlinkMB
*downlinkMB_top_b6.labelString: "Screen Functions"
*downlinkMB_top_b6.subMenuId: "screenFuncP"
*downlinkMB_top_b6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkMB_top_b6.width: 155
*downlinkMB_top_b6.x: 400
*downlinkMB_top_b6.marginLeft: 0
*downlinkMB_top_b6.marginWidth: 20
*downlinkMB_top_b6.mnemonic: "S"
*downlinkMB_top_b6.y: 0

*label155.class: label
*label155.static: true
*label155.name: label155
*label155.parent: DLtoDTK
*label155.isCompound: "true"
*label155.compoundIcon: "label.xpm"
*label155.compoundName: "label_"
*label155.x: 296
*label155.y: 42
*label155.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label155.labelString: "Downlink to Datatake Screen"
*label155.background: "#9ac0cd"

*downlinkIdLBL.class: label
*downlinkIdLBL.static: true
*downlinkIdLBL.name: downlinkIdLBL
*downlinkIdLBL.parent: DLtoDTK
*downlinkIdLBL.isCompound: "true"
*downlinkIdLBL.compoundIcon: "label.xpm"
*downlinkIdLBL.compoundName: "label_"
*downlinkIdLBL.x: 58
*downlinkIdLBL.y: 133
*downlinkIdLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkIdLBL.labelString: "Downlink ID"
*downlinkIdLBL.background: "#9ac0cd"
*downlinkIdLBL.height: 20

*dlActivityLBL.class: label
*dlActivityLBL.static: true
*dlActivityLBL.name: dlActivityLBL
*dlActivityLBL.parent: DLtoDTK
*dlActivityLBL.isCompound: "true"
*dlActivityLBL.compoundIcon: "label.xpm"
*dlActivityLBL.compoundName: "label_"
*dlActivityLBL.x: 188
*dlActivityLBL.y: 133
*dlActivityLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlActivityLBL.labelString: "Act."
*dlActivityLBL.background: "#9ac0cd"
*dlActivityLBL.height: 20

*dlStationLBL.class: label
*dlStationLBL.static: true
*dlStationLBL.name: dlStationLBL
*dlStationLBL.parent: DLtoDTK
*dlStationLBL.isCompound: "true"
*dlStationLBL.compoundIcon: "label.xpm"
*dlStationLBL.compoundName: "label_"
*dlStationLBL.x: 226
*dlStationLBL.y: 133
*dlStationLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlStationLBL.labelString: "Stn"
*dlStationLBL.background: "#9ac0cd"
*dlStationLBL.height: 20

*dlAntennaLBL.class: label
*dlAntennaLBL.static: true
*dlAntennaLBL.name: dlAntennaLBL
*dlAntennaLBL.parent: DLtoDTK
*dlAntennaLBL.isCompound: "true"
*dlAntennaLBL.compoundIcon: "label.xpm"
*dlAntennaLBL.compoundName: "label_"
*dlAntennaLBL.x: 270
*dlAntennaLBL.y: 133
*dlAntennaLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlAntennaLBL.labelString: "Antenna"
*dlAntennaLBL.background: "#9ac0cd"
*dlAntennaLBL.height: 20

*dlTimeOnLBL.class: label
*dlTimeOnLBL.static: true
*dlTimeOnLBL.name: dlTimeOnLBL
*dlTimeOnLBL.parent: DLtoDTK
*dlTimeOnLBL.isCompound: "true"
*dlTimeOnLBL.compoundIcon: "label.xpm"
*dlTimeOnLBL.compoundName: "label_"
*dlTimeOnLBL.x: 390
*dlTimeOnLBL.y: 133
*dlTimeOnLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlTimeOnLBL.labelString: "Time On"
*dlTimeOnLBL.background: "#9ac0cd"
*dlTimeOnLBL.height: 20

*dlTimeOffLBL.class: label
*dlTimeOffLBL.static: true
*dlTimeOffLBL.name: dlTimeOffLBL
*dlTimeOffLBL.parent: DLtoDTK
*dlTimeOffLBL.isCompound: "true"
*dlTimeOffLBL.compoundIcon: "label.xpm"
*dlTimeOffLBL.compoundName: "label_"
*dlTimeOffLBL.x: 542
*dlTimeOffLBL.y: 133
*dlTimeOffLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlTimeOffLBL.labelString: "Time Off"
*dlTimeOffLBL.background: "#9ac0cd"
*dlTimeOffLBL.height: 20

*dlStatusLBL.class: label
*dlStatusLBL.static: true
*dlStatusLBL.name: dlStatusLBL
*dlStatusLBL.parent: DLtoDTK
*dlStatusLBL.isCompound: "true"
*dlStatusLBL.compoundIcon: "label.xpm"
*dlStatusLBL.compoundName: "label_"
*dlStatusLBL.x: 680
*dlStatusLBL.y: 133
*dlStatusLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlStatusLBL.labelString: "Status"
*dlStatusLBL.background: "#9ac0cd"
*dlStatusLBL.height: 20

*dlNumDTKsLBL.class: label
*dlNumDTKsLBL.static: true
*dlNumDTKsLBL.name: dlNumDTKsLBL
*dlNumDTKsLBL.parent: DLtoDTK
*dlNumDTKsLBL.isCompound: "true"
*dlNumDTKsLBL.compoundIcon: "label.xpm"
*dlNumDTKsLBL.compoundName: "label_"
*dlNumDTKsLBL.x: 768
*dlNumDTKsLBL.y: 133
*dlNumDTKsLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlNumDTKsLBL.labelString: "#DTKs"
*dlNumDTKsLBL.background: "#9ac0cd"
*dlNumDTKsLBL.height: 20

*dlPlatformSW.class: scrolledWindow
*dlPlatformSW.static: true
*dlPlatformSW.name: dlPlatformSW
*dlPlatformSW.parent: DLtoDTK
*dlPlatformSW.scrollingPolicy: "automatic"
*dlPlatformSW.x: 58
*dlPlatformSW.y: 156
*dlPlatformSW.visualPolicy: "constant"
*dlPlatformSW.scrollBarDisplayPolicy: "as_needed"
*dlPlatformSW.shadowThickness: 0
*dlPlatformSW.compoundIcon: "scrllist.xpm"
*dlPlatformSW.compoundName: "scrolled_List"
*dlPlatformSW.background: "#9ac0cd"
*dlPlatformSW.height: 230
*dlPlatformSW.isCompound: "true"
*dlPlatformSW.width: 27

*dlPlatformLIST.class: scrolledList
*dlPlatformLIST.static: true
*dlPlatformLIST.name: dlPlatformLIST
*dlPlatformLIST.parent: dlPlatformSW
*dlPlatformLIST.width: 27
*dlPlatformLIST.height: 230
*dlPlatformLIST.background: "LightSkyBlue3"
*dlPlatformLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlPlatformLIST.listSizePolicy: "constant"
*dlPlatformLIST.defaultActionCallback.source: public
*dlPlatformLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlPlatformLIST.browseSelectionCallback.source: public
*dlPlatformLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlSensorSW.class: scrolledWindow
*dlSensorSW.static: true
*dlSensorSW.name: dlSensorSW
*dlSensorSW.parent: DLtoDTK
*dlSensorSW.scrollingPolicy: "automatic"
*dlSensorSW.x: 86
*dlSensorSW.y: 156
*dlSensorSW.visualPolicy: "constant"
*dlSensorSW.scrollBarDisplayPolicy: "as_needed"
*dlSensorSW.shadowThickness: 0
*dlSensorSW.compoundIcon: "scrllist.xpm"
*dlSensorSW.compoundName: "scrolled_List"
*dlSensorSW.background: "#9ac0cd"
*dlSensorSW.height: 230
*dlSensorSW.isCompound: "true"
*dlSensorSW.width: 16

*dlSensorLIST.class: scrolledList
*dlSensorLIST.static: true
*dlSensorLIST.name: dlSensorLIST
*dlSensorLIST.parent: dlSensorSW
*dlSensorLIST.width: 16
*dlSensorLIST.height: 230
*dlSensorLIST.background: "LightSkyBlue3"
*dlSensorLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlSensorLIST.listSizePolicy: "constant"
*dlSensorLIST.defaultActionCallback.source: public
*dlSensorLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlSensorLIST.browseSelectionCallback.source: public
*dlSensorLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlRevSW.class: scrolledWindow
*dlRevSW.static: true
*dlRevSW.name: dlRevSW
*dlRevSW.parent: DLtoDTK
*dlRevSW.scrollingPolicy: "automatic"
*dlRevSW.x: 103
*dlRevSW.y: 156
*dlRevSW.visualPolicy: "constant"
*dlRevSW.scrollBarDisplayPolicy: "as_needed"
*dlRevSW.shadowThickness: 0
*dlRevSW.compoundIcon: "scrllist.xpm"
*dlRevSW.compoundName: "scrolled_List"
*dlRevSW.background: "#9ac0cd"
*dlRevSW.height: 230
*dlRevSW.isCompound: "true"
*dlRevSW.width: 45

*dlRevLIST.class: scrolledList
*dlRevLIST.static: true
*dlRevLIST.name: dlRevLIST
*dlRevLIST.parent: dlRevSW
*dlRevLIST.width: 45
*dlRevLIST.height: 230
*dlRevLIST.background: "LightSkyBlue3"
*dlRevLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlRevLIST.listSizePolicy: "constant"
*dlRevLIST.defaultActionCallback.source: public
*dlRevLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlRevLIST.browseSelectionCallback.source: public
*dlRevLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlSequenceSW.class: scrolledWindow
*dlSequenceSW.static: true
*dlSequenceSW.name: dlSequenceSW
*dlSequenceSW.parent: DLtoDTK
*dlSequenceSW.scrollingPolicy: "automatic"
*dlSequenceSW.x: 149
*dlSequenceSW.y: 156
*dlSequenceSW.visualPolicy: "constant"
*dlSequenceSW.scrollBarDisplayPolicy: "as_needed"
*dlSequenceSW.shadowThickness: 0
*dlSequenceSW.compoundIcon: "scrllist.xpm"
*dlSequenceSW.compoundName: "scrolled_List"
*dlSequenceSW.background: "#9ac0cd"
*dlSequenceSW.height: 230
*dlSequenceSW.isCompound: "true"
*dlSequenceSW.width: 20

*dlSequenceLIST.class: scrolledList
*dlSequenceLIST.static: true
*dlSequenceLIST.name: dlSequenceLIST
*dlSequenceLIST.parent: dlSequenceSW
*dlSequenceLIST.width: 20
*dlSequenceLIST.height: 230
*dlSequenceLIST.background: "LightSkyBlue3"
*dlSequenceLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlSequenceLIST.listSizePolicy: "constant"
*dlSequenceLIST.defaultActionCallback.source: public
*dlSequenceLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlSequenceLIST.browseSelectionCallback.source: public
*dlSequenceLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlActivitySW.class: scrolledWindow
*dlActivitySW.static: true
*dlActivitySW.name: dlActivitySW
*dlActivitySW.parent: DLtoDTK
*dlActivitySW.scrollingPolicy: "automatic"
*dlActivitySW.x: 183
*dlActivitySW.y: 156
*dlActivitySW.visualPolicy: "constant"
*dlActivitySW.scrollBarDisplayPolicy: "as_needed"
*dlActivitySW.shadowThickness: 0
*dlActivitySW.compoundIcon: "scrllist.xpm"
*dlActivitySW.compoundName: "scrolled_List"
*dlActivitySW.background: "#9ac0cd"
*dlActivitySW.height: 230
*dlActivitySW.isCompound: "true"
*dlActivitySW.width: 43

*dlActivityLIST.class: scrolledList
*dlActivityLIST.static: true
*dlActivityLIST.name: dlActivityLIST
*dlActivityLIST.parent: dlActivitySW
*dlActivityLIST.width: 43
*dlActivityLIST.height: 230
*dlActivityLIST.background: "LightSkyBlue3"
*dlActivityLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlActivityLIST.listSizePolicy: "constant"
*dlActivityLIST.defaultActionCallback.source: public
*dlActivityLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlActivityLIST.browseSelectionCallback.source: public
*dlActivityLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlStationSW.class: scrolledWindow
*dlStationSW.static: true
*dlStationSW.name: dlStationSW
*dlStationSW.parent: DLtoDTK
*dlStationSW.scrollingPolicy: "automatic"
*dlStationSW.x: 228
*dlStationSW.y: 156
*dlStationSW.visualPolicy: "constant"
*dlStationSW.scrollBarDisplayPolicy: "as_needed"
*dlStationSW.shadowThickness: 0
*dlStationSW.compoundIcon: "scrllist.xpm"
*dlStationSW.compoundName: "scrolled_List"
*dlStationSW.background: "#9ac0cd"
*dlStationSW.height: 230
*dlStationSW.isCompound: "true"
*dlStationSW.width: 27

*dlStationLIST.class: scrolledList
*dlStationLIST.static: true
*dlStationLIST.name: dlStationLIST
*dlStationLIST.parent: dlStationSW
*dlStationLIST.width: 27
*dlStationLIST.height: 230
*dlStationLIST.background: "LightSkyBlue3"
*dlStationLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlStationLIST.listSizePolicy: "constant"
*dlStationLIST.defaultActionCallback.source: public
*dlStationLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlStationLIST.browseSelectionCallback.source: public
*dlStationLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlAntennaSW.class: scrolledWindow
*dlAntennaSW.static: true
*dlAntennaSW.name: dlAntennaSW
*dlAntennaSW.parent: DLtoDTK
*dlAntennaSW.scrollingPolicy: "automatic"
*dlAntennaSW.x: 257
*dlAntennaSW.y: 156
*dlAntennaSW.visualPolicy: "constant"
*dlAntennaSW.scrollBarDisplayPolicy: "as_needed"
*dlAntennaSW.shadowThickness: 0
*dlAntennaSW.compoundIcon: "scrllist.xpm"
*dlAntennaSW.compoundName: "scrolled_List"
*dlAntennaSW.background: "#9ac0cd"
*dlAntennaSW.height: 230
*dlAntennaSW.isCompound: "true"
*dlAntennaSW.width: 95

*dlAntennaLIST.class: scrolledList
*dlAntennaLIST.static: true
*dlAntennaLIST.name: dlAntennaLIST
*dlAntennaLIST.parent: dlAntennaSW
*dlAntennaLIST.width: 94
*dlAntennaLIST.height: 230
*dlAntennaLIST.background: "LightSkyBlue3"
*dlAntennaLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlAntennaLIST.listSizePolicy: "constant"
*dlAntennaLIST.defaultActionCallback.source: public
*dlAntennaLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlAntennaLIST.browseSelectionCallback.source: public
*dlAntennaLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlTimeOnSW.class: scrolledWindow
*dlTimeOnSW.static: true
*dlTimeOnSW.name: dlTimeOnSW
*dlTimeOnSW.parent: DLtoDTK
*dlTimeOnSW.scrollingPolicy: "automatic"
*dlTimeOnSW.x: 353
*dlTimeOnSW.y: 156
*dlTimeOnSW.visualPolicy: "constant"
*dlTimeOnSW.scrollBarDisplayPolicy: "as_needed"
*dlTimeOnSW.shadowThickness: 0
*dlTimeOnSW.compoundIcon: "scrllist.xpm"
*dlTimeOnSW.compoundName: "scrolled_List"
*dlTimeOnSW.background: "#9ac0cd"
*dlTimeOnSW.height: 230
*dlTimeOnSW.isCompound: "true"
*dlTimeOnSW.width: 150

*dlTimeOnLIST.class: scrolledList
*dlTimeOnLIST.static: true
*dlTimeOnLIST.name: dlTimeOnLIST
*dlTimeOnLIST.parent: dlTimeOnSW
*dlTimeOnLIST.width: 150
*dlTimeOnLIST.height: 230
*dlTimeOnLIST.background: "LightSkyBlue3"
*dlTimeOnLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlTimeOnLIST.listSizePolicy: "constant"
*dlTimeOnLIST.defaultActionCallback.source: public
*dlTimeOnLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlTimeOnLIST.browseSelectionCallback.source: public
*dlTimeOnLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlTimeOffSW.class: scrolledWindow
*dlTimeOffSW.static: true
*dlTimeOffSW.name: dlTimeOffSW
*dlTimeOffSW.parent: DLtoDTK
*dlTimeOffSW.scrollingPolicy: "automatic"
*dlTimeOffSW.x: 506
*dlTimeOffSW.y: 156
*dlTimeOffSW.visualPolicy: "constant"
*dlTimeOffSW.scrollBarDisplayPolicy: "as_needed"
*dlTimeOffSW.shadowThickness: 0
*dlTimeOffSW.compoundIcon: "scrllist.xpm"
*dlTimeOffSW.compoundName: "scrolled_List"
*dlTimeOffSW.background: "#9ac0cd"
*dlTimeOffSW.height: 230
*dlTimeOffSW.isCompound: "true"
*dlTimeOffSW.width: 150

*dlTimeOffLIST.class: scrolledList
*dlTimeOffLIST.static: true
*dlTimeOffLIST.name: dlTimeOffLIST
*dlTimeOffLIST.parent: dlTimeOffSW
*dlTimeOffLIST.width: 150
*dlTimeOffLIST.height: 230
*dlTimeOffLIST.background: "LightSkyBlue3"
*dlTimeOffLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlTimeOffLIST.listSizePolicy: "constant"
*dlTimeOffLIST.defaultActionCallback.source: public
*dlTimeOffLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlTimeOffLIST.browseSelectionCallback.source: public
*dlTimeOffLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlStatusSW.class: scrolledWindow
*dlStatusSW.static: true
*dlStatusSW.name: dlStatusSW
*dlStatusSW.parent: DLtoDTK
*dlStatusSW.scrollingPolicy: "automatic"
*dlStatusSW.x: 659
*dlStatusSW.y: 156
*dlStatusSW.visualPolicy: "constant"
*dlStatusSW.scrollBarDisplayPolicy: "as_needed"
*dlStatusSW.shadowThickness: 0
*dlStatusSW.compoundIcon: "scrllist.xpm"
*dlStatusSW.compoundName: "scrolled_List"
*dlStatusSW.background: "#9ac0cd"
*dlStatusSW.height: 230
*dlStatusSW.isCompound: "true"
*dlStatusSW.width: 95

*dlStatusLIST.class: scrolledList
*dlStatusLIST.static: true
*dlStatusLIST.name: dlStatusLIST
*dlStatusLIST.parent: dlStatusSW
*dlStatusLIST.width: 94
*dlStatusLIST.height: 230
*dlStatusLIST.background: "LightSkyBlue3"
*dlStatusLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlStatusLIST.listSizePolicy: "constant"
*dlStatusLIST.defaultActionCallback.source: public
*dlStatusLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlStatusLIST.browseSelectionCallback.source: public
*dlStatusLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlNumDTKsSW.class: scrolledWindow
*dlNumDTKsSW.static: true
*dlNumDTKsSW.name: dlNumDTKsSW
*dlNumDTKsSW.parent: DLtoDTK
*dlNumDTKsSW.scrollingPolicy: "automatic"
*dlNumDTKsSW.x: 770
*dlNumDTKsSW.y: 156
*dlNumDTKsSW.visualPolicy: "constant"
*dlNumDTKsSW.scrollBarDisplayPolicy: "as_needed"
*dlNumDTKsSW.shadowThickness: 0
*dlNumDTKsSW.compoundIcon: "scrllist.xpm"
*dlNumDTKsSW.compoundName: "scrolled_List"
*dlNumDTKsSW.background: "#9ac0cd"
*dlNumDTKsSW.height: 230
*dlNumDTKsSW.isCompound: "true"
*dlNumDTKsSW.width: 50

*dlNumDTKsLIST.class: scrolledList
*dlNumDTKsLIST.static: true
*dlNumDTKsLIST.name: dlNumDTKsLIST
*dlNumDTKsLIST.parent: dlNumDTKsSW
*dlNumDTKsLIST.width: 50
*dlNumDTKsLIST.height: 230
*dlNumDTKsLIST.background: "LightSkyBlue3"
*dlNumDTKsLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlNumDTKsLIST.listSizePolicy: "constant"
*dlNumDTKsLIST.defaultActionCallback.source: public
*dlNumDTKsLIST.defaultActionCallback: dl2dtk_show_downlinkDTKsCb
*dlNumDTKsLIST.browseSelectionCallback.source: public
*dlNumDTKsLIST.browseSelectionCallback: dl2dtk_dlLists_selectionCb

*dlDummySW.class: scrolledWindow
*dlDummySW.static: true
*dlDummySW.name: dlDummySW
*dlDummySW.parent: DLtoDTK
*dlDummySW.scrollingPolicy: "application_defined"
*dlDummySW.visualPolicy: "variable"
*dlDummySW.scrollBarDisplayPolicy: "static"
*dlDummySW.shadowThickness: 0
*dlDummySW.isCompound: "true"
*dlDummySW.compoundIcon: "scrllist.xpm"
*dlDummySW.compoundName: "scrolled_List"
*dlDummySW.x: 822
*dlDummySW.y: 156
*dlDummySW.height: 246
*dlDummySW.width: 15
*dlDummySW.background: "#9ac0cd"

*dlDummyLIST.class: scrolledList
*dlDummyLIST.static: true
*dlDummyLIST.name: dlDummyLIST
*dlDummyLIST.parent: dlDummySW
*dlDummyLIST.width: 2
*dlDummyLIST.height: 230
*dlDummyLIST.listSizePolicy: "constant"
*dlDummyLIST.mappedWhenManaged: "false"
*dlDummyLIST.scrollBarDisplayPolicy: "static"
*dlDummyLIST.visibleItemCount: 13
*dlDummyLIST.selectionPolicy: "browse_select"
*dlDummyLIST.shadowThickness: 1
*dlDummyLIST.background: "LightSkyBlue3"
*dlDummyLIST.doubleClickInterval: 0

*separator12.class: separator
*separator12.static: true
*separator12.name: separator12
*separator12.parent: DLtoDTK
*separator12.x: 8
*separator12.y: 405
*separator12.width: 875
*separator12.height: 9
*separator12.background: "#9ac0cd"

*dtkIdLBL.class: label
*dtkIdLBL.static: true
*dtkIdLBL.name: dtkIdLBL
*dtkIdLBL.parent: DLtoDTK
*dtkIdLBL.isCompound: "true"
*dtkIdLBL.compoundIcon: "label.xpm"
*dtkIdLBL.compoundName: "label_"
*dtkIdLBL.x: 28
*dtkIdLBL.y: 492
*dtkIdLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkIdLBL.labelString: "Datatake ID"
*dtkIdLBL.background: "#9ac0cd"
*dtkIdLBL.height: 20

*dtkSensorModeLBL.class: label
*dtkSensorModeLBL.static: true
*dtkSensorModeLBL.name: dtkSensorModeLBL
*dtkSensorModeLBL.parent: DLtoDTK
*dtkSensorModeLBL.isCompound: "true"
*dtkSensorModeLBL.compoundIcon: "label.xpm"
*dtkSensorModeLBL.compoundName: "label_"
*dtkSensorModeLBL.x: 145
*dtkSensorModeLBL.y: 477
*dtkSensorModeLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkSensorModeLBL.labelString: "Sensor\nMode"
*dtkSensorModeLBL.background: "#9ac0cd"
*dtkSensorModeLBL.height: 35

*dtkTimeOnLBL.class: label
*dtkTimeOnLBL.static: true
*dtkTimeOnLBL.name: dtkTimeOnLBL
*dtkTimeOnLBL.parent: DLtoDTK
*dtkTimeOnLBL.isCompound: "true"
*dtkTimeOnLBL.compoundIcon: "label.xpm"
*dtkTimeOnLBL.compoundName: "label_"
*dtkTimeOnLBL.x: 231
*dtkTimeOnLBL.y: 492
*dtkTimeOnLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkTimeOnLBL.labelString: "Time On"
*dtkTimeOnLBL.background: "#9ac0cd"
*dtkTimeOnLBL.height: 20

*dtkTimeOffLBL.class: label
*dtkTimeOffLBL.static: true
*dtkTimeOffLBL.name: dtkTimeOffLBL
*dtkTimeOffLBL.parent: DLtoDTK
*dtkTimeOffLBL.isCompound: "true"
*dtkTimeOffLBL.compoundIcon: "label.xpm"
*dtkTimeOffLBL.compoundName: "label_"
*dtkTimeOffLBL.x: 381
*dtkTimeOffLBL.y: 492
*dtkTimeOffLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkTimeOffLBL.labelString: "Time Off"
*dtkTimeOffLBL.background: "#9ac0cd"
*dtkTimeOffLBL.height: 20

*dtkSiteNameLBL.class: label
*dtkSiteNameLBL.static: true
*dtkSiteNameLBL.name: dtkSiteNameLBL
*dtkSiteNameLBL.parent: DLtoDTK
*dtkSiteNameLBL.isCompound: "true"
*dtkSiteNameLBL.compoundIcon: "label.xpm"
*dtkSiteNameLBL.compoundName: "label_"
*dtkSiteNameLBL.x: 536
*dtkSiteNameLBL.y: 492
*dtkSiteNameLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkSiteNameLBL.labelString: "Site Name"
*dtkSiteNameLBL.background: "#9ac0cd"
*dtkSiteNameLBL.height: 20

*dtkQuicklookLBL.class: label
*dtkQuicklookLBL.static: true
*dtkQuicklookLBL.name: dtkQuicklookLBL
*dtkQuicklookLBL.parent: DLtoDTK
*dtkQuicklookLBL.isCompound: "true"
*dtkQuicklookLBL.compoundIcon: "label.xpm"
*dtkQuicklookLBL.compoundName: "label_"
*dtkQuicklookLBL.x: 756
*dtkQuicklookLBL.y: 492
*dtkQuicklookLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkQuicklookLBL.labelString: "QLK"
*dtkQuicklookLBL.background: "#9ac0cd"
*dtkQuicklookLBL.height: 20

*dtkFrameModeLBL.class: label
*dtkFrameModeLBL.static: true
*dtkFrameModeLBL.name: dtkFrameModeLBL
*dtkFrameModeLBL.parent: DLtoDTK
*dtkFrameModeLBL.isCompound: "true"
*dtkFrameModeLBL.compoundIcon: "label.xpm"
*dtkFrameModeLBL.compoundName: "label_"
*dtkFrameModeLBL.x: 685
*dtkFrameModeLBL.y: 477
*dtkFrameModeLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkFrameModeLBL.labelString: "Frame\nMode"
*dtkFrameModeLBL.background: "#9ac0cd"
*dtkFrameModeLBL.height: 35

*dtkProcAuthLBL.class: label
*dtkProcAuthLBL.static: true
*dtkProcAuthLBL.name: dtkProcAuthLBL
*dtkProcAuthLBL.parent: DLtoDTK
*dtkProcAuthLBL.isCompound: "true"
*dtkProcAuthLBL.compoundIcon: "label.xpm"
*dtkProcAuthLBL.compoundName: "label_"
*dtkProcAuthLBL.x: 806
*dtkProcAuthLBL.y: 477
*dtkProcAuthLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dtkProcAuthLBL.labelString: "Proc\nAuth"
*dtkProcAuthLBL.background: "#9ac0cd"
*dtkProcAuthLBL.height: 35

*dtklPlatformSW.class: scrolledWindow
*dtklPlatformSW.static: true
*dtklPlatformSW.name: dtklPlatformSW
*dtklPlatformSW.parent: DLtoDTK
*dtklPlatformSW.scrollingPolicy: "automatic"
*dtklPlatformSW.x: 28
*dtklPlatformSW.y: 515
*dtklPlatformSW.visualPolicy: "constant"
*dtklPlatformSW.scrollBarDisplayPolicy: "as_needed"
*dtklPlatformSW.shadowThickness: 0
*dtklPlatformSW.compoundIcon: "scrllist.xpm"
*dtklPlatformSW.compoundName: "scrolled_List"
*dtklPlatformSW.background: "#9ac0cd"
*dtklPlatformSW.height: 230
*dtklPlatformSW.isCompound: "true"
*dtklPlatformSW.width: 27

*dtkPlatformLIST.class: scrolledList
*dtkPlatformLIST.static: true
*dtkPlatformLIST.name: dtkPlatformLIST
*dtkPlatformLIST.parent: dtklPlatformSW
*dtkPlatformLIST.width: 27
*dtkPlatformLIST.height: 230
*dtkPlatformLIST.background: "LightSkyBlue3"
*dtkPlatformLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkPlatformLIST.listSizePolicy: "constant"
*dtkPlatformLIST.defaultActionCallback.source: public
*dtkPlatformLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkPlatformLIST.selectionPolicy: "extended_select"
*dtkPlatformLIST.extendedSelectionCallback.source: public
*dtkPlatformLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkSensorSW.class: scrolledWindow
*dtkSensorSW.static: true
*dtkSensorSW.name: dtkSensorSW
*dtkSensorSW.parent: DLtoDTK
*dtkSensorSW.scrollingPolicy: "automatic"
*dtkSensorSW.x: 56
*dtkSensorSW.y: 515
*dtkSensorSW.visualPolicy: "constant"
*dtkSensorSW.scrollBarDisplayPolicy: "as_needed"
*dtkSensorSW.shadowThickness: 0
*dtkSensorSW.compoundIcon: "scrllist.xpm"
*dtkSensorSW.compoundName: "scrolled_List"
*dtkSensorSW.background: "#9ac0cd"
*dtkSensorSW.height: 230
*dtkSensorSW.isCompound: "true"
*dtkSensorSW.width: 16

*dtkSensorLIST.class: scrolledList
*dtkSensorLIST.static: true
*dtkSensorLIST.name: dtkSensorLIST
*dtkSensorLIST.parent: dtkSensorSW
*dtkSensorLIST.width: 16
*dtkSensorLIST.height: 230
*dtkSensorLIST.background: "LightSkyBlue3"
*dtkSensorLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkSensorLIST.listSizePolicy: "constant"
*dtkSensorLIST.defaultActionCallback.source: public
*dtkSensorLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkSensorLIST.selectionPolicy: "extended_select"
*dtkSensorLIST.extendedSelectionCallback.source: public
*dtkSensorLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkRevSW.class: scrolledWindow
*dtkRevSW.static: true
*dtkRevSW.name: dtkRevSW
*dtkRevSW.parent: DLtoDTK
*dtkRevSW.scrollingPolicy: "automatic"
*dtkRevSW.x: 73
*dtkRevSW.y: 515
*dtkRevSW.visualPolicy: "constant"
*dtkRevSW.scrollBarDisplayPolicy: "as_needed"
*dtkRevSW.shadowThickness: 0
*dtkRevSW.compoundIcon: "scrllist.xpm"
*dtkRevSW.compoundName: "scrolled_List"
*dtkRevSW.background: "#9ac0cd"
*dtkRevSW.height: 230
*dtkRevSW.isCompound: "true"
*dtkRevSW.width: 45

*dtkRevLIST.class: scrolledList
*dtkRevLIST.static: true
*dtkRevLIST.name: dtkRevLIST
*dtkRevLIST.parent: dtkRevSW
*dtkRevLIST.width: 45
*dtkRevLIST.height: 230
*dtkRevLIST.background: "LightSkyBlue3"
*dtkRevLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkRevLIST.listSizePolicy: "constant"
*dtkRevLIST.defaultActionCallback.source: public
*dtkRevLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkRevLIST.selectionPolicy: "extended_select"
*dtkRevLIST.extendedSelectionCallback.source: public
*dtkRevLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkSequenceSW.class: scrolledWindow
*dtkSequenceSW.static: true
*dtkSequenceSW.name: dtkSequenceSW
*dtkSequenceSW.parent: DLtoDTK
*dtkSequenceSW.scrollingPolicy: "automatic"
*dtkSequenceSW.x: 119
*dtkSequenceSW.y: 515
*dtkSequenceSW.visualPolicy: "constant"
*dtkSequenceSW.scrollBarDisplayPolicy: "as_needed"
*dtkSequenceSW.shadowThickness: 0
*dtkSequenceSW.compoundIcon: "scrllist.xpm"
*dtkSequenceSW.compoundName: "scrolled_List"
*dtkSequenceSW.background: "#9ac0cd"
*dtkSequenceSW.height: 230
*dtkSequenceSW.isCompound: "true"
*dtkSequenceSW.width: 20

*dtkSequenceLIST.class: scrolledList
*dtkSequenceLIST.static: true
*dtkSequenceLIST.name: dtkSequenceLIST
*dtkSequenceLIST.parent: dtkSequenceSW
*dtkSequenceLIST.width: 20
*dtkSequenceLIST.height: 230
*dtkSequenceLIST.background: "LightSkyBlue3"
*dtkSequenceLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkSequenceLIST.listSizePolicy: "constant"
*dtkSequenceLIST.defaultActionCallback.source: public
*dtkSequenceLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkSequenceLIST.selectionPolicy: "extended_select"
*dtkSequenceLIST.extendedSelectionCallback.source: public
*dtkSequenceLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkSensorModeSW.class: scrolledWindow
*dtkSensorModeSW.static: true
*dtkSensorModeSW.name: dtkSensorModeSW
*dtkSensorModeSW.parent: DLtoDTK
*dtkSensorModeSW.scrollingPolicy: "automatic"
*dtkSensorModeSW.x: 153
*dtkSensorModeSW.y: 515
*dtkSensorModeSW.visualPolicy: "constant"
*dtkSensorModeSW.scrollBarDisplayPolicy: "as_needed"
*dtkSensorModeSW.shadowThickness: 0
*dtkSensorModeSW.compoundIcon: "scrllist.xpm"
*dtkSensorModeSW.compoundName: "scrolled_List"
*dtkSensorModeSW.background: "#9ac0cd"
*dtkSensorModeSW.height: 230
*dtkSensorModeSW.isCompound: "true"
*dtkSensorModeSW.width: 40

*dtkSensorModeLIST.class: scrolledList
*dtkSensorModeLIST.static: true
*dtkSensorModeLIST.name: dtkSensorModeLIST
*dtkSensorModeLIST.parent: dtkSensorModeSW
*dtkSensorModeLIST.width: 40
*dtkSensorModeLIST.height: 230
*dtkSensorModeLIST.background: "LightSkyBlue3"
*dtkSensorModeLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkSensorModeLIST.listSizePolicy: "constant"
*dtkSensorModeLIST.defaultActionCallback.source: public
*dtkSensorModeLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkSensorModeLIST.selectionPolicy: "extended_select"
*dtkSensorModeLIST.extendedSelectionCallback.source: public
*dtkSensorModeLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkTimeOnSW.class: scrolledWindow
*dtkTimeOnSW.static: true
*dtkTimeOnSW.name: dtkTimeOnSW
*dtkTimeOnSW.parent: DLtoDTK
*dtkTimeOnSW.scrollingPolicy: "automatic"
*dtkTimeOnSW.x: 194
*dtkTimeOnSW.y: 515
*dtkTimeOnSW.visualPolicy: "constant"
*dtkTimeOnSW.scrollBarDisplayPolicy: "as_needed"
*dtkTimeOnSW.shadowThickness: 0
*dtkTimeOnSW.compoundIcon: "scrllist.xpm"
*dtkTimeOnSW.compoundName: "scrolled_List"
*dtkTimeOnSW.background: "#9ac0cd"
*dtkTimeOnSW.height: 230
*dtkTimeOnSW.isCompound: "true"
*dtkTimeOnSW.width: 150

*dtkTimeOnLIST.class: scrolledList
*dtkTimeOnLIST.static: true
*dtkTimeOnLIST.name: dtkTimeOnLIST
*dtkTimeOnLIST.parent: dtkTimeOnSW
*dtkTimeOnLIST.width: 150
*dtkTimeOnLIST.height: 230
*dtkTimeOnLIST.background: "LightSkyBlue3"
*dtkTimeOnLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkTimeOnLIST.listSizePolicy: "constant"
*dtkTimeOnLIST.defaultActionCallback.source: public
*dtkTimeOnLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkTimeOnLIST.selectionPolicy: "extended_select"
*dtkTimeOnLIST.extendedSelectionCallback.source: public
*dtkTimeOnLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkTimeOffSW.class: scrolledWindow
*dtkTimeOffSW.static: true
*dtkTimeOffSW.name: dtkTimeOffSW
*dtkTimeOffSW.parent: DLtoDTK
*dtkTimeOffSW.scrollingPolicy: "automatic"
*dtkTimeOffSW.x: 345
*dtkTimeOffSW.y: 515
*dtkTimeOffSW.visualPolicy: "constant"
*dtkTimeOffSW.scrollBarDisplayPolicy: "as_needed"
*dtkTimeOffSW.shadowThickness: 0
*dtkTimeOffSW.compoundIcon: "scrllist.xpm"
*dtkTimeOffSW.compoundName: "scrolled_List"
*dtkTimeOffSW.background: "#9ac0cd"
*dtkTimeOffSW.height: 230
*dtkTimeOffSW.isCompound: "true"
*dtkTimeOffSW.width: 150

*dtkTimeOffLIST.class: scrolledList
*dtkTimeOffLIST.static: true
*dtkTimeOffLIST.name: dtkTimeOffLIST
*dtkTimeOffLIST.parent: dtkTimeOffSW
*dtkTimeOffLIST.width: 150
*dtkTimeOffLIST.height: 230
*dtkTimeOffLIST.background: "LightSkyBlue3"
*dtkTimeOffLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkTimeOffLIST.listSizePolicy: "constant"
*dtkTimeOffLIST.defaultActionCallback.source: public
*dtkTimeOffLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkTimeOffLIST.selectionPolicy: "extended_select"
*dtkTimeOffLIST.extendedSelectionCallback.source: public
*dtkTimeOffLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkSiteNameSW.class: scrolledWindow
*dtkSiteNameSW.static: true
*dtkSiteNameSW.name: dtkSiteNameSW
*dtkSiteNameSW.parent: DLtoDTK
*dtkSiteNameSW.scrollingPolicy: "automatic"
*dtkSiteNameSW.x: 496
*dtkSiteNameSW.y: 515
*dtkSiteNameSW.visualPolicy: "constant"
*dtkSiteNameSW.scrollBarDisplayPolicy: "as_needed"
*dtkSiteNameSW.shadowThickness: 0
*dtkSiteNameSW.compoundIcon: "scrllist.xpm"
*dtkSiteNameSW.compoundName: "scrolled_List"
*dtkSiteNameSW.background: "#9ac0cd"
*dtkSiteNameSW.height: 230
*dtkSiteNameSW.isCompound: "true"
*dtkSiteNameSW.width: 170

*dtkSiteNameLIST.class: scrolledList
*dtkSiteNameLIST.static: true
*dtkSiteNameLIST.name: dtkSiteNameLIST
*dtkSiteNameLIST.parent: dtkSiteNameSW
*dtkSiteNameLIST.width: 170
*dtkSiteNameLIST.height: 230
*dtkSiteNameLIST.background: "LightSkyBlue3"
*dtkSiteNameLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkSiteNameLIST.listSizePolicy: "constant"
*dtkSiteNameLIST.defaultActionCallback.source: public
*dtkSiteNameLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkSiteNameLIST.selectionPolicy: "extended_select"
*dtkSiteNameLIST.extendedSelectionCallback.source: public
*dtkSiteNameLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkFrameModeSW.class: scrolledWindow
*dtkFrameModeSW.static: true
*dtkFrameModeSW.name: dtkFrameModeSW
*dtkFrameModeSW.parent: DLtoDTK
*dtkFrameModeSW.scrollingPolicy: "automatic"
*dtkFrameModeSW.x: 667
*dtkFrameModeSW.y: 515
*dtkFrameModeSW.visualPolicy: "constant"
*dtkFrameModeSW.scrollBarDisplayPolicy: "as_needed"
*dtkFrameModeSW.shadowThickness: 0
*dtkFrameModeSW.compoundIcon: "scrllist.xpm"
*dtkFrameModeSW.compoundName: "scrolled_List"
*dtkFrameModeSW.background: "#9ac0cd"
*dtkFrameModeSW.height: 230
*dtkFrameModeSW.isCompound: "true"
*dtkFrameModeSW.width: 90

*dtkFrameModeLIST.class: scrolledList
*dtkFrameModeLIST.static: true
*dtkFrameModeLIST.name: dtkFrameModeLIST
*dtkFrameModeLIST.parent: dtkFrameModeSW
*dtkFrameModeLIST.width: 90
*dtkFrameModeLIST.height: 230
*dtkFrameModeLIST.background: "LightSkyBlue3"
*dtkFrameModeLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkFrameModeLIST.listSizePolicy: "constant"
*dtkFrameModeLIST.defaultActionCallback.source: public
*dtkFrameModeLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkFrameModeLIST.selectionPolicy: "extended_select"
*dtkFrameModeLIST.extendedSelectionCallback.source: public
*dtkFrameModeLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkQuicklookSW.class: scrolledWindow
*dtkQuicklookSW.static: true
*dtkQuicklookSW.name: dtkQuicklookSW
*dtkQuicklookSW.parent: DLtoDTK
*dtkQuicklookSW.scrollingPolicy: "automatic"
*dtkQuicklookSW.x: 758
*dtkQuicklookSW.y: 515
*dtkQuicklookSW.visualPolicy: "constant"
*dtkQuicklookSW.scrollBarDisplayPolicy: "as_needed"
*dtkQuicklookSW.shadowThickness: 0
*dtkQuicklookSW.compoundIcon: "scrllist.xpm"
*dtkQuicklookSW.compoundName: "scrolled_List"
*dtkQuicklookSW.background: "#9ac0cd"
*dtkQuicklookSW.height: 230
*dtkQuicklookSW.isCompound: "true"
*dtkQuicklookSW.width: 35

*dtkQuicklookLIST.class: scrolledList
*dtkQuicklookLIST.static: true
*dtkQuicklookLIST.name: dtkQuicklookLIST
*dtkQuicklookLIST.parent: dtkQuicklookSW
*dtkQuicklookLIST.width: 35
*dtkQuicklookLIST.height: 230
*dtkQuicklookLIST.background: "LightSkyBlue3"
*dtkQuicklookLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkQuicklookLIST.listSizePolicy: "constant"
*dtkQuicklookLIST.defaultActionCallback.source: public
*dtkQuicklookLIST.defaultActionCallback: dl2dtk_view_DTK_detailsCb
*dtkQuicklookLIST.selectionPolicy: "extended_select"
*dtkQuicklookLIST.extendedSelectionCallback.source: public
*dtkQuicklookLIST.extendedSelectionCallback: dl2dtk_dtkLists_selectionCb

*dtkProcAuthFRAME.class: frame
*dtkProcAuthFRAME.static: true
*dtkProcAuthFRAME.name: dtkProcAuthFRAME
*dtkProcAuthFRAME.parent: DLtoDTK
*dtkProcAuthFRAME.width: 27
*dtkProcAuthFRAME.height: 234
*dtkProcAuthFRAME.isCompound: "true"
*dtkProcAuthFRAME.compoundIcon: "frame.xpm"
*dtkProcAuthFRAME.compoundName: "frame_"
*dtkProcAuthFRAME.x: 809
*dtkProcAuthFRAME.y: 513
*dtkProcAuthFRAME.background: "LightSkyBlue3"
*dtkProcAuthFRAME.shadowType: "shadow_in"

*dtkProcAuthSW.class: scrolledWindow
*dtkProcAuthSW.static: true
*dtkProcAuthSW.name: dtkProcAuthSW
*dtkProcAuthSW.parent: dtkProcAuthFRAME
*dtkProcAuthSW.scrollingPolicy: "automatic"
*dtkProcAuthSW.x: 2
*dtkProcAuthSW.y: 2
*dtkProcAuthSW.visualPolicy: "constant"
*dtkProcAuthSW.scrollBarDisplayPolicy: "as_needed"
*dtkProcAuthSW.shadowThickness: 0
*dtkProcAuthSW.compoundIcon: "scrllist.xpm"
*dtkProcAuthSW.compoundName: "scrolled_List"
*dtkProcAuthSW.background: "#9ac0cd"
*dtkProcAuthSW.height: 230
*dtkProcAuthSW.isCompound: "true"
*dtkProcAuthSW.width: 30
*dtkProcAuthSW.topShadowColor: "#d4d4e4e4eaea"

*dtkProcAuthLIST.class: scrolledList
*dtkProcAuthLIST.static: true
*dtkProcAuthLIST.name: dtkProcAuthLIST
*dtkProcAuthLIST.parent: dtkProcAuthSW
*dtkProcAuthLIST.width: 23
*dtkProcAuthLIST.height: 230
*dtkProcAuthLIST.background: "LightSkyBlue3"
*dtkProcAuthLIST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkProcAuthLIST.listSizePolicy: "constant"
*dtkProcAuthLIST.selectionPolicy: "extended_select"
*dtkProcAuthLIST.extendedSelectionCallback.source: public
*dtkProcAuthLIST.extendedSelectionCallback: dl2dtk_toggle_proc_auth_flagCb

*ProcFlagFootnote.class: label
*ProcFlagFootnote.static: true
*ProcFlagFootnote.name: ProcFlagFootnote
*ProcFlagFootnote.parent: DLtoDTK
*ProcFlagFootnote.x: 705
*ProcFlagFootnote.y: 751
*ProcFlagFootnote.width: 148
*ProcFlagFootnote.height: 17
*ProcFlagFootnote.background: "#9ac0cd"
*ProcFlagFootnote.labelString: "*Proc Auth CANNOT be changed"
*ProcFlagFootnote.fontList: "-adobe-times-bold-i-normal--11-80-100-100-p-57-iso8859-1"
*ProcFlagFootnote.mappedWhenManaged: "false"
*ProcFlagFootnote.rightAttachment: "attach_none"

*dtkDummySW.class: scrolledWindow
*dtkDummySW.static: true
*dtkDummySW.name: dtkDummySW
*dtkDummySW.parent: DLtoDTK
*dtkDummySW.scrollingPolicy: "application_defined"
*dtkDummySW.visualPolicy: "variable"
*dtkDummySW.scrollBarDisplayPolicy: "static"
*dtkDummySW.shadowThickness: 0
*dtkDummySW.isCompound: "true"
*dtkDummySW.compoundIcon: "scrllist.xpm"
*dtkDummySW.compoundName: "scrolled_List"
*dtkDummySW.x: 838
*dtkDummySW.y: 515
*dtkDummySW.height: 246
*dtkDummySW.width: 15
*dtkDummySW.background: "#9ac0cd"

*dtkDummyLIST.class: scrolledList
*dtkDummyLIST.static: true
*dtkDummyLIST.name: dtkDummyLIST
*dtkDummyLIST.parent: dtkDummySW
*dtkDummyLIST.width: 2
*dtkDummyLIST.height: 230
*dtkDummyLIST.listSizePolicy: "constant"
*dtkDummyLIST.mappedWhenManaged: "false"
*dtkDummyLIST.scrollBarDisplayPolicy: "static"
*dtkDummyLIST.visibleItemCount: 13
*dtkDummyLIST.selectionPolicy: "browse_select"
*dtkDummyLIST.shadowThickness: 1
*dtkDummyLIST.background: "LightSkyBlue3"
*dtkDummyLIST.doubleClickInterval: 0

*dl2dtkUpdatePB.class: pushButton
*dl2dtkUpdatePB.static: true
*dl2dtkUpdatePB.name: dl2dtkUpdatePB
*dl2dtkUpdatePB.parent: DLtoDTK
*dl2dtkUpdatePB.isCompound: "true"
*dl2dtkUpdatePB.compoundIcon: "push.xpm"
*dl2dtkUpdatePB.compoundName: "push_Button"
*dl2dtkUpdatePB.x: 24
*dl2dtkUpdatePB.y: 773
*dl2dtkUpdatePB.width: 185
*dl2dtkUpdatePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dl2dtkUpdatePB.labelString: "UPDATE"
*dl2dtkUpdatePB.height: 32
*dl2dtkUpdatePB.background: "CadetBlue"
*dl2dtkUpdatePB.shadowThickness: 4
*dl2dtkUpdatePB.traversalOn: "false"
*dl2dtkUpdatePB.activateCallback.source: public
*dl2dtkUpdatePB.activateCallback: dl2dtk_updateCb
*dl2dtkUpdatePB.sensitive: "false"

*dl2dtkRefreshSearchPB.class: pushButton
*dl2dtkRefreshSearchPB.static: true
*dl2dtkRefreshSearchPB.name: dl2dtkRefreshSearchPB
*dl2dtkRefreshSearchPB.parent: DLtoDTK
*dl2dtkRefreshSearchPB.isCompound: "true"
*dl2dtkRefreshSearchPB.compoundIcon: "push.xpm"
*dl2dtkRefreshSearchPB.compoundName: "push_Button"
*dl2dtkRefreshSearchPB.x: 241
*dl2dtkRefreshSearchPB.y: 773
*dl2dtkRefreshSearchPB.width: 185
*dl2dtkRefreshSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dl2dtkRefreshSearchPB.labelString: "REFRESH   SEARCH"
*dl2dtkRefreshSearchPB.height: 32
*dl2dtkRefreshSearchPB.background: "CadetBlue"
*dl2dtkRefreshSearchPB.shadowThickness: 4
*dl2dtkRefreshSearchPB.traversalOn: "false"
*dl2dtkRefreshSearchPB.activateCallback.source: public
*dl2dtkRefreshSearchPB.activateCallback: dl2dtk_refreshSearchCb
*dl2dtkRefreshSearchPB.sensitive: "false"

*dl2dtkPrintScreenPB.class: pushButton
*dl2dtkPrintScreenPB.static: true
*dl2dtkPrintScreenPB.name: dl2dtkPrintScreenPB
*dl2dtkPrintScreenPB.parent: DLtoDTK
*dl2dtkPrintScreenPB.isCompound: "true"
*dl2dtkPrintScreenPB.compoundIcon: "push.xpm"
*dl2dtkPrintScreenPB.compoundName: "push_Button"
*dl2dtkPrintScreenPB.x: 459
*dl2dtkPrintScreenPB.y: 773
*dl2dtkPrintScreenPB.width: 185
*dl2dtkPrintScreenPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dl2dtkPrintScreenPB.labelString: "PRINT   SCREEN"
*dl2dtkPrintScreenPB.height: 32
*dl2dtkPrintScreenPB.background: "CadetBlue"
*dl2dtkPrintScreenPB.shadowThickness: 4
*dl2dtkPrintScreenPB.traversalOn: "false"
*dl2dtkPrintScreenPB.activateCallback.source: public
*dl2dtkPrintScreenPB.activateCallback: dl2dtk_printScreenCb

*dl2dtkCloseScreenPB.class: pushButton
*dl2dtkCloseScreenPB.static: true
*dl2dtkCloseScreenPB.name: dl2dtkCloseScreenPB
*dl2dtkCloseScreenPB.parent: DLtoDTK
*dl2dtkCloseScreenPB.isCompound: "true"
*dl2dtkCloseScreenPB.compoundIcon: "push.xpm"
*dl2dtkCloseScreenPB.compoundName: "push_Button"
*dl2dtkCloseScreenPB.x: 676
*dl2dtkCloseScreenPB.y: 773
*dl2dtkCloseScreenPB.width: 185
*dl2dtkCloseScreenPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dl2dtkCloseScreenPB.labelString: "CLOSE"
*dl2dtkCloseScreenPB.height: 32
*dl2dtkCloseScreenPB.background: "CadetBlue"
*dl2dtkCloseScreenPB.shadowThickness: 4
*dl2dtkCloseScreenPB.traversalOn: "false"
*dl2dtkCloseScreenPB.activateCallback.source: public
*dl2dtkCloseScreenPB.activateCallback: dl2dtk_closeCb

*downlinkIdLBL1.class: label
*downlinkIdLBL1.static: true
*downlinkIdLBL1.name: downlinkIdLBL1
*downlinkIdLBL1.parent: DLtoDTK
*downlinkIdLBL1.isCompound: "true"
*downlinkIdLBL1.compoundIcon: "label.xpm"
*downlinkIdLBL1.compoundName: "label_"
*downlinkIdLBL1.x: 58
*downlinkIdLBL1.y: 133
*downlinkIdLBL1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*downlinkIdLBL1.labelString: "Downlink ID"
*downlinkIdLBL1.background: "#9ac0cd"
*downlinkIdLBL1.height: 20

*dlTotalNumDlksLBL.class: label
*dlTotalNumDlksLBL.static: true
*dlTotalNumDlksLBL.name: dlTotalNumDlksLBL
*dlTotalNumDlksLBL.parent: DLtoDTK
*dlTotalNumDlksLBL.isCompound: "true"
*dlTotalNumDlksLBL.compoundIcon: "label.xpm"
*dlTotalNumDlksLBL.compoundName: "label_"
*dlTotalNumDlksLBL.x: 30
*dlTotalNumDlksLBL.y: 88
*dlTotalNumDlksLBL.width: 240
*dlTotalNumDlksLBL.height: 20
*dlTotalNumDlksLBL.background: "#9ac0cd"
*dlTotalNumDlksLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlTotalNumDlksLBL.labelString: "Total Number of Downlinks:"

*label21.class: label
*label21.static: true
*label21.name: label21
*label21.parent: DLtoDTK
*label21.isCompound: "true"
*label21.compoundIcon: "label.xpm"
*label21.compoundName: "label_"
*label21.x: 30
*label21.y: 439
*label21.width: 245
*label21.height: 20
*label21.background: "#9ac0cd"
*label21.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label21.labelString: "Total Number of Datatakes:"

*dlTotalDlksTF.class: textField
*dlTotalDlksTF.static: true
*dlTotalDlksTF.name: dlTotalDlksTF
*dlTotalDlksTF.parent: DLtoDTK
*dlTotalDlksTF.width: 110
*dlTotalDlksTF.isCompound: "true"
*dlTotalDlksTF.compoundIcon: "textfield.xpm"
*dlTotalDlksTF.compoundName: "text_Field"
*dlTotalDlksTF.x: 292
*dlTotalDlksTF.y: 81
*dlTotalDlksTF.height: 36
*dlTotalDlksTF.background: "LightSkyBlue3"
*dlTotalDlksTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlTotalDlksTF.editable: "false"

*dtkTotalDksTF.class: textField
*dtkTotalDksTF.static: true
*dtkTotalDksTF.name: dtkTotalDksTF
*dtkTotalDksTF.parent: DLtoDTK
*dtkTotalDksTF.width: 110
*dtkTotalDksTF.isCompound: "true"
*dtkTotalDksTF.compoundIcon: "textfield.xpm"
*dtkTotalDksTF.compoundName: "text_Field"
*dtkTotalDksTF.x: 292
*dtkTotalDksTF.y: 435
*dtkTotalDksTF.height: 36
*dtkTotalDksTF.background: "LightSkyBlue3"
*dtkTotalDksTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dtkTotalDksTF.editable: "false"

