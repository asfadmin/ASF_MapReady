! UIMX ascii 2.9 key: 5715                                                      

*DLtoDTKsearch.class: form
*DLtoDTKsearch.classinc:
*DLtoDTKsearch.classspec:
*DLtoDTKsearch.classmembers:
*DLtoDTKsearch.classconstructor:
*DLtoDTKsearch.classdestructor:
*DLtoDTKsearch.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*DLtoDTKsearch.ispecdecl:
*DLtoDTKsearch.funcdecl: swidget create_DLtoDTKsearch(swidget UxParent)
*DLtoDTKsearch.funcname: create_DLtoDTKsearch
*DLtoDTKsearch.funcdef: "swidget", "<create_DLtoDTKsearch>(%)"
*DLtoDTKsearch.argdecl: swidget UxParent;
*DLtoDTKsearch.arglist: UxParent
*DLtoDTKsearch.arglist.UxParent: "swidget", "%UxParent%"
*DLtoDTKsearch.icode:
*DLtoDTKsearch.fcode: XtAddCallback( dlSearchTimeOnStartText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchTimeOnEndText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchTimeOffStartText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchTimeOffEndText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchRevStartText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchRevEndText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchSequenceStartText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
XtAddCallback( dlSearchSequenceEndText, XmNactivateCallback,\
	(XtCallbackProc) XmProcessTraversal,\
	(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );\
\
return(rtrn);\

*DLtoDTKsearch.auxdecl:
*DLtoDTKsearch.static: true
*DLtoDTKsearch.name: DLtoDTKsearch
*DLtoDTKsearch.parent: NO_PARENT
*DLtoDTKsearch.parentExpression: UxParent
*DLtoDTKsearch.defaultShell: topLevelShell
*DLtoDTKsearch.width: 1067
*DLtoDTKsearch.height: 616
*DLtoDTKsearch.resizePolicy: "resize_none"
*DLtoDTKsearch.isCompound: "true"
*DLtoDTKsearch.compoundIcon: "form.xpm"
*DLtoDTKsearch.compoundName: "form_"
*DLtoDTKsearch.x: 51
*DLtoDTKsearch.y: 140
*DLtoDTKsearch.unitType: "pixels"
*DLtoDTKsearch.dialogTitle: "Downlink (to Datatake) Search Screen"
*DLtoDTKsearch.background: "#9ac0cd"
*DLtoDTKsearch.noResize: "true"

*menuBar1.class: rowColumn
*menuBar1.static: true
*menuBar1.name: menuBar1
*menuBar1.parent: DLtoDTKsearch
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
*srchWelcomeMPB.activateCallback: dl_search_goto_welcomeCb

*menuBar_p1_b2.class: separator
*menuBar_p1_b2.static: true
*menuBar_p1_b2.name: menuBar_p1_b2
*menuBar_p1_b2.parent: menuBar_p1

*gotoDL2DTK_MPB.class: pushButton
*gotoDL2DTK_MPB.static: true
*gotoDL2DTK_MPB.name: gotoDL2DTK_MPB
*gotoDL2DTK_MPB.parent: menuBar_p1
*gotoDL2DTK_MPB.labelString: "Downlink to Data-take Screen"
*gotoDL2DTK_MPB.background: "cadetBlue"
*gotoDL2DTK_MPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*gotoDL2DTK_MPB.mnemonic: "D"
*gotoDL2DTK_MPB.activateCallback.source: public
*gotoDL2DTK_MPB.activateCallback: dl_search_goto_dl2dtkCb
*gotoDL2DTK_MPB.sensitive: "true"

*menuBar_p1_b4.class: separator
*menuBar_p1_b4.static: true
*menuBar_p1_b4.name: menuBar_p1_b4
*menuBar_p1_b4.parent: menuBar_p1

*closeScreenMPB.class: pushButton
*closeScreenMPB.static: true
*closeScreenMPB.name: closeScreenMPB
*closeScreenMPB.parent: menuBar_p1
*closeScreenMPB.labelString: "Close  Screen"
*closeScreenMPB.mnemonic: "C"
*closeScreenMPB.background: "cadetBlue"
*closeScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*closeScreenMPB.activateCallback.source: public
*closeScreenMPB.activateCallback: dl_search_closeCb

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
*srchExecuteSearchMPB.activateCallback: dl_search_executeCb

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
*srchClearSearchMPB.activateCallback: dl_search_clearCb

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
*srchPrintScreenMPB.activateCallback: dl_search_printScreenCb

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
*menuBar1_p3_b1.activateCallback.source: public
*menuBar1_p3_b1.activateCallback: 

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

*dlSearchPlatformLBL.class: label
*dlSearchPlatformLBL.static: true
*dlSearchPlatformLBL.name: dlSearchPlatformLBL
*dlSearchPlatformLBL.parent: DLtoDTKsearch
*dlSearchPlatformLBL.isCompound: "true"
*dlSearchPlatformLBL.compoundIcon: "label.xpm"
*dlSearchPlatformLBL.compoundName: "label_"
*dlSearchPlatformLBL.x: 38
*dlSearchPlatformLBL.y: 85
*dlSearchPlatformLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchPlatformLBL.labelString: "Platform"
*dlSearchPlatformLBL.background: "#9ac0cd"
*dlSearchPlatformLBL.height: 20

*dlSearchSensorLBL.class: label
*dlSearchSensorLBL.static: true
*dlSearchSensorLBL.name: dlSearchSensorLBL
*dlSearchSensorLBL.parent: DLtoDTKsearch
*dlSearchSensorLBL.isCompound: "true"
*dlSearchSensorLBL.compoundIcon: "label.xpm"
*dlSearchSensorLBL.compoundName: "label_"
*dlSearchSensorLBL.x: 153
*dlSearchSensorLBL.y: 84
*dlSearchSensorLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchSensorLBL.labelString: "Sensor"
*dlSearchSensorLBL.background: "#9ac0cd"

*dlSearchActivityLBL.class: label
*dlSearchActivityLBL.static: true
*dlSearchActivityLBL.name: dlSearchActivityLBL
*dlSearchActivityLBL.parent: DLtoDTKsearch
*dlSearchActivityLBL.isCompound: "true"
*dlSearchActivityLBL.compoundIcon: "label.xpm"
*dlSearchActivityLBL.compoundName: "label_"
*dlSearchActivityLBL.x: 253
*dlSearchActivityLBL.y: 84
*dlSearchActivityLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchActivityLBL.labelString: "Activity ID"
*dlSearchActivityLBL.background: "#9ac0cd"

*dlSearchStationLBL.class: label
*dlSearchStationLBL.static: true
*dlSearchStationLBL.name: dlSearchStationLBL
*dlSearchStationLBL.parent: DLtoDTKsearch
*dlSearchStationLBL.isCompound: "true"
*dlSearchStationLBL.compoundIcon: "label.xpm"
*dlSearchStationLBL.compoundName: "label_"
*dlSearchStationLBL.x: 370
*dlSearchStationLBL.y: 84
*dlSearchStationLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchStationLBL.labelString: "Station ID"
*dlSearchStationLBL.background: "#9ac0cd"

*dk2dtkAntennaLBL.class: label
*dk2dtkAntennaLBL.static: true
*dk2dtkAntennaLBL.name: dk2dtkAntennaLBL
*dk2dtkAntennaLBL.parent: DLtoDTKsearch
*dk2dtkAntennaLBL.isCompound: "true"
*dk2dtkAntennaLBL.compoundIcon: "label.xpm"
*dk2dtkAntennaLBL.compoundName: "label_"
*dk2dtkAntennaLBL.x: 509
*dk2dtkAntennaLBL.y: 84
*dk2dtkAntennaLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dk2dtkAntennaLBL.labelString: "Antenna ID"
*dk2dtkAntennaLBL.background: "#9ac0cd"

*dlSearchTransmitterLBL.class: label
*dlSearchTransmitterLBL.static: true
*dlSearchTransmitterLBL.name: dlSearchTransmitterLBL
*dlSearchTransmitterLBL.parent: DLtoDTKsearch
*dlSearchTransmitterLBL.isCompound: "true"
*dlSearchTransmitterLBL.compoundIcon: "label.xpm"
*dlSearchTransmitterLBL.compoundName: "label_"
*dlSearchTransmitterLBL.x: 694
*dlSearchTransmitterLBL.y: 84
*dlSearchTransmitterLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTransmitterLBL.labelString: "Transmitter ID"
*dlSearchTransmitterLBL.background: "#9ac0cd"

*dlSearchPlatformSW.class: scrolledWindow
*dlSearchPlatformSW.static: true
*dlSearchPlatformSW.name: dlSearchPlatformSW
*dlSearchPlatformSW.parent: DLtoDTKsearch
*dlSearchPlatformSW.scrollingPolicy: "automatic"
*dlSearchPlatformSW.width: 91
*dlSearchPlatformSW.height: 230
*dlSearchPlatformSW.isCompound: "true"
*dlSearchPlatformSW.compoundIcon: "scrlwnd.xpm"
*dlSearchPlatformSW.compoundName: "scrolled_Window"
*dlSearchPlatformSW.x: 30
*dlSearchPlatformSW.y: 113
*dlSearchPlatformSW.background: "LightSkyBlue3"
*dlSearchPlatformSW.scrollBarDisplayPolicy: "as_needed"
*dlSearchPlatformSW.spacing: 4
*dlSearchPlatformSW.leftOffset: 30
*dlSearchPlatformSW.traversalOn: "false"

*platformRC.class: rowColumn
*platformRC.static: true
*platformRC.name: platformRC
*platformRC.parent: dlSearchPlatformSW
*platformRC.isCompound: "true"
*platformRC.compoundIcon: "row.xpm"
*platformRC.compoundName: "row_Column"
*platformRC.x: 2
*platformRC.y: 2
*platformRC.width: 60
*platformRC.height: 226
*platformRC.resizeHeight: "true"
*platformRC.resizeWidth: "false"
*platformRC.spacing: 0
*platformRC.traversalOn: "false"

*dummyTB.class: toggleButton
*dummyTB.static: true
*dummyTB.name: dummyTB
*dummyTB.parent: platformRC
*dummyTB.isCompound: "true"
*dummyTB.compoundIcon: "toggle.xpm"
*dummyTB.compoundName: "toggle_Button"
*dummyTB.x: 3
*dummyTB.y: 3
*dummyTB.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dummyTB.indicatorSize: 25
*dummyTB.labelString: "E2"
*dummyTB.background: "LightSkyBlue3"
*dummyTB.width: 74
*dummyTB.selectColor: "SlateGray4"
*dummyTB.traversalOn: "false"

*dlSearchSensorSW.class: scrolledWindow
*dlSearchSensorSW.static: true
*dlSearchSensorSW.name: dlSearchSensorSW
*dlSearchSensorSW.parent: DLtoDTKsearch
*dlSearchSensorSW.scrollingPolicy: "automatic"
*dlSearchSensorSW.width: 81
*dlSearchSensorSW.height: 230
*dlSearchSensorSW.isCompound: "true"
*dlSearchSensorSW.compoundIcon: "scrlwnd.xpm"
*dlSearchSensorSW.compoundName: "scrolled_Window"
*dlSearchSensorSW.x: 141
*dlSearchSensorSW.y: 112
*dlSearchSensorSW.background: "LightSkyBlue3"
*dlSearchSensorSW.traversalOn: "false"

*sensorRC.class: rowColumn
*sensorRC.static: true
*sensorRC.name: sensorRC
*sensorRC.parent: dlSearchSensorSW
*sensorRC.isCompound: "true"
*sensorRC.compoundIcon: "row.xpm"
*sensorRC.compoundName: "row_Column"
*sensorRC.y: 0
*sensorRC.width: 50
*sensorRC.height: 226
*sensorRC.resizeHeight: "true"
*sensorRC.resizeWidth: "false"
*sensorRC.spacing: 0
*sensorRC.marginHeight: 5
*sensorRC.traversalOn: "false"

*dlSearchActivitySW.class: scrolledWindow
*dlSearchActivitySW.static: true
*dlSearchActivitySW.name: dlSearchActivitySW
*dlSearchActivitySW.parent: DLtoDTKsearch
*dlSearchActivitySW.scrollingPolicy: "automatic"
*dlSearchActivitySW.width: 105
*dlSearchActivitySW.height: 230
*dlSearchActivitySW.isCompound: "true"
*dlSearchActivitySW.compoundIcon: "scrlwnd.xpm"
*dlSearchActivitySW.compoundName: "scrolled_Window"
*dlSearchActivitySW.x: 240
*dlSearchActivitySW.y: 112
*dlSearchActivitySW.background: "LightSkyBlue3"
*dlSearchActivitySW.traversalOn: "false"

*activityRC.class: rowColumn
*activityRC.static: true
*activityRC.name: activityRC
*activityRC.parent: dlSearchActivitySW
*activityRC.isCompound: "true"
*activityRC.compoundIcon: "row.xpm"
*activityRC.compoundName: "row_Column"
*activityRC.x: 2
*activityRC.y: 2
*activityRC.width: 74
*activityRC.height: 226
*activityRC.resizeHeight: "true"
*activityRC.resizeWidth: "false"
*activityRC.spacing: 0
*activityRC.marginHeight: 5
*activityRC.traversalOn: "false"

*dlSearchStationSW.class: scrolledWindow
*dlSearchStationSW.static: true
*dlSearchStationSW.name: dlSearchStationSW
*dlSearchStationSW.parent: DLtoDTKsearch
*dlSearchStationSW.scrollingPolicy: "automatic"
*dlSearchStationSW.width: 95
*dlSearchStationSW.height: 230
*dlSearchStationSW.isCompound: "true"
*dlSearchStationSW.compoundIcon: "scrlwnd.xpm"
*dlSearchStationSW.compoundName: "scrolled_Window"
*dlSearchStationSW.x: 362
*dlSearchStationSW.y: 112
*dlSearchStationSW.background: "LightSkyBlue3"
*dlSearchStationSW.traversalOn: "false"

*stationRC.class: rowColumn
*stationRC.static: true
*stationRC.name: stationRC
*stationRC.parent: dlSearchStationSW
*stationRC.isCompound: "true"
*stationRC.compoundIcon: "row.xpm"
*stationRC.compoundName: "row_Column"
*stationRC.x: 2
*stationRC.y: 2
*stationRC.width: 64
*stationRC.height: 226
*stationRC.resizeHeight: "true"
*stationRC.resizeWidth: "false"
*stationRC.spacing: 0
*stationRC.marginHeight: 5
*stationRC.traversalOn: "false"

*dlSearchAntennaSW.class: scrolledWindow
*dlSearchAntennaSW.static: true
*dlSearchAntennaSW.name: dlSearchAntennaSW
*dlSearchAntennaSW.parent: DLtoDTKsearch
*dlSearchAntennaSW.scrollingPolicy: "automatic"
*dlSearchAntennaSW.height: 230
*dlSearchAntennaSW.isCompound: "true"
*dlSearchAntennaSW.compoundIcon: "scrlwnd.xpm"
*dlSearchAntennaSW.compoundName: "scrolled_Window"
*dlSearchAntennaSW.x: 475
*dlSearchAntennaSW.y: 112
*dlSearchAntennaSW.background: "LightSkyBlue3"
*dlSearchAntennaSW.width: 156
*dlSearchAntennaSW.traversalOn: "false"

*antennaRC.class: rowColumn
*antennaRC.static: true
*antennaRC.name: antennaRC
*antennaRC.parent: dlSearchAntennaSW
*antennaRC.isCompound: "true"
*antennaRC.compoundIcon: "row.xpm"
*antennaRC.compoundName: "row_Column"
*antennaRC.x: 2
*antennaRC.y: 2
*antennaRC.width: 125
*antennaRC.height: 226
*antennaRC.resizeWidth: "false"
*antennaRC.marginWidth: 5
*antennaRC.spacing: 0
*antennaRC.resizeHeight: "true"
*antennaRC.traversalOn: "false"

*dlSearchTransmitterSW.class: scrolledWindow
*dlSearchTransmitterSW.static: true
*dlSearchTransmitterSW.name: dlSearchTransmitterSW
*dlSearchTransmitterSW.parent: DLtoDTKsearch
*dlSearchTransmitterSW.scrollingPolicy: "automatic"
*dlSearchTransmitterSW.width: 208
*dlSearchTransmitterSW.height: 230
*dlSearchTransmitterSW.isCompound: "true"
*dlSearchTransmitterSW.compoundIcon: "scrlwnd.xpm"
*dlSearchTransmitterSW.compoundName: "scrolled_Window"
*dlSearchTransmitterSW.x: 648
*dlSearchTransmitterSW.y: 112
*dlSearchTransmitterSW.background: "LightSkyBlue3"
*dlSearchTransmitterSW.traversalOn: "false"

*transmitterRC.class: rowColumn
*transmitterRC.static: true
*transmitterRC.name: transmitterRC
*transmitterRC.parent: dlSearchTransmitterSW
*transmitterRC.isCompound: "true"
*transmitterRC.compoundIcon: "row.xpm"
*transmitterRC.compoundName: "row_Column"
*transmitterRC.x: 2
*transmitterRC.y: 2
*transmitterRC.width: 177
*transmitterRC.height: 226
*transmitterRC.resizeWidth: "false"
*transmitterRC.spacing: 0
*transmitterRC.resizeHeight: "true"
*transmitterRC.traversalOn: "false"

*dlSearchSeparator1.class: separator
*dlSearchSeparator1.static: true
*dlSearchSeparator1.name: dlSearchSeparator1
*dlSearchSeparator1.parent: DLtoDTKsearch
*dlSearchSeparator1.width: 1015
*dlSearchSeparator1.height: 8
*dlSearchSeparator1.isCompound: "true"
*dlSearchSeparator1.compoundIcon: "sep.xpm"
*dlSearchSeparator1.compoundName: "separator_"
*dlSearchSeparator1.x: 26
*dlSearchSeparator1.y: 353
*dlSearchSeparator1.background: "#9ac0cd"
*dlSearchSeparator1.separatorType: "shadow_etched_in"
*dlSearchSeparator1.shadowThickness: 3

*dlSearchExecuteSearchPB.class: pushButton
*dlSearchExecuteSearchPB.static: true
*dlSearchExecuteSearchPB.name: dlSearchExecuteSearchPB
*dlSearchExecuteSearchPB.parent: DLtoDTKsearch
*dlSearchExecuteSearchPB.isCompound: "true"
*dlSearchExecuteSearchPB.compoundIcon: "push.xpm"
*dlSearchExecuteSearchPB.compoundName: "push_Button"
*dlSearchExecuteSearchPB.x: 59
*dlSearchExecuteSearchPB.y: 554
*dlSearchExecuteSearchPB.width: 195
*dlSearchExecuteSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dlSearchExecuteSearchPB.labelString: " EXECUTE   SEARCH "
*dlSearchExecuteSearchPB.height: 32
*dlSearchExecuteSearchPB.background: "CadetBlue"
*dlSearchExecuteSearchPB.shadowThickness: 4
*dlSearchExecuteSearchPB.traversalOn: "false"
*dlSearchExecuteSearchPB.activateCallback.source: public
*dlSearchExecuteSearchPB.activateCallback: dl_search_executeCb

*dlSearchClearSearchPB.class: pushButton
*dlSearchClearSearchPB.static: true
*dlSearchClearSearchPB.name: dlSearchClearSearchPB
*dlSearchClearSearchPB.parent: DLtoDTKsearch
*dlSearchClearSearchPB.isCompound: "true"
*dlSearchClearSearchPB.compoundIcon: "push.xpm"
*dlSearchClearSearchPB.compoundName: "push_Button"
*dlSearchClearSearchPB.x: 306
*dlSearchClearSearchPB.y: 554
*dlSearchClearSearchPB.width: 195
*dlSearchClearSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dlSearchClearSearchPB.labelString: "CLEAR    SEARCH"
*dlSearchClearSearchPB.height: 32
*dlSearchClearSearchPB.background: "CadetBlue"
*dlSearchClearSearchPB.shadowThickness: 4
*dlSearchClearSearchPB.traversalOn: "false"
*dlSearchClearSearchPB.activateCallback.source: public
*dlSearchClearSearchPB.activateCallback: dl_search_clearCb

*dlSearchCloseSearchPB.class: pushButton
*dlSearchCloseSearchPB.static: true
*dlSearchCloseSearchPB.name: dlSearchCloseSearchPB
*dlSearchCloseSearchPB.parent: DLtoDTKsearch
*dlSearchCloseSearchPB.isCompound: "true"
*dlSearchCloseSearchPB.compoundIcon: "push.xpm"
*dlSearchCloseSearchPB.compoundName: "push_Button"
*dlSearchCloseSearchPB.x: 799
*dlSearchCloseSearchPB.y: 554
*dlSearchCloseSearchPB.width: 195
*dlSearchCloseSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dlSearchCloseSearchPB.labelString: "CLOSE    SCREEN"
*dlSearchCloseSearchPB.height: 32
*dlSearchCloseSearchPB.background: "CadetBlue"
*dlSearchCloseSearchPB.shadowThickness: 4
*dlSearchCloseSearchPB.traversalOn: "false"
*dlSearchCloseSearchPB.activateCallback.source: public
*dlSearchCloseSearchPB.activateCallback: dl_search_closeCb

*dlSearchPrintSearchPB.class: pushButton
*dlSearchPrintSearchPB.static: true
*dlSearchPrintSearchPB.name: dlSearchPrintSearchPB
*dlSearchPrintSearchPB.parent: DLtoDTKsearch
*dlSearchPrintSearchPB.isCompound: "true"
*dlSearchPrintSearchPB.compoundIcon: "push.xpm"
*dlSearchPrintSearchPB.compoundName: "push_Button"
*dlSearchPrintSearchPB.x: 554
*dlSearchPrintSearchPB.y: 554
*dlSearchPrintSearchPB.width: 195
*dlSearchPrintSearchPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*dlSearchPrintSearchPB.labelString: "PRINT     SCREEN"
*dlSearchPrintSearchPB.height: 32
*dlSearchPrintSearchPB.background: "CadetBlue"
*dlSearchPrintSearchPB.shadowThickness: 4
*dlSearchPrintSearchPB.traversalOn: "false"
*dlSearchPrintSearchPB.activateCallback.source: public
*dlSearchPrintSearchPB.activateCallback: dl_search_printScreenCb

*DownlinkSearchLBL.class: label
*DownlinkSearchLBL.static: true
*DownlinkSearchLBL.name: DownlinkSearchLBL
*DownlinkSearchLBL.parent: DLtoDTKsearch
*DownlinkSearchLBL.isCompound: "true"
*DownlinkSearchLBL.compoundIcon: "label.xpm"
*DownlinkSearchLBL.compoundName: "label_"
*DownlinkSearchLBL.x: 439
*DownlinkSearchLBL.y: 47
*DownlinkSearchLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*DownlinkSearchLBL.labelString: "Downlink     Search"
*DownlinkSearchLBL.background: "#9ac0cd"
*DownlinkSearchLBL.height: 28

*dlSearchTimeOnFrame.class: frame
*dlSearchTimeOnFrame.static: true
*dlSearchTimeOnFrame.name: dlSearchTimeOnFrame
*dlSearchTimeOnFrame.parent: DLtoDTKsearch
*dlSearchTimeOnFrame.width: 455
*dlSearchTimeOnFrame.height: 46
*dlSearchTimeOnFrame.isCompound: "true"
*dlSearchTimeOnFrame.compoundIcon: "frame.xpm"
*dlSearchTimeOnFrame.compoundName: "frame_"
*dlSearchTimeOnFrame.x: 546
*dlSearchTimeOnFrame.y: 418
*dlSearchTimeOnFrame.shadowType: "shadow_etched_in"
*dlSearchTimeOnFrame.shadowThickness: 2

*dlSearchTimeOnForm.class: form
*dlSearchTimeOnForm.static: true
*dlSearchTimeOnForm.name: dlSearchTimeOnForm
*dlSearchTimeOnForm.parent: dlSearchTimeOnFrame
*dlSearchTimeOnForm.width: 451
*dlSearchTimeOnForm.height: 46
*dlSearchTimeOnForm.resizePolicy: "resize_none"
*dlSearchTimeOnForm.isCompound: "true"
*dlSearchTimeOnForm.compoundIcon: "form.xpm"
*dlSearchTimeOnForm.compoundName: "form_"
*dlSearchTimeOnForm.x: 2
*dlSearchTimeOnForm.y: 2
*dlSearchTimeOnForm.background: "#9ac0cd"
*dlSearchTimeOnForm.shadowType: "shadow_out"
*dlSearchTimeOnForm.shadowThickness: 0

*dlSearchTimeOnStartLBL.class: label
*dlSearchTimeOnStartLBL.static: true
*dlSearchTimeOnStartLBL.name: dlSearchTimeOnStartLBL
*dlSearchTimeOnStartLBL.parent: dlSearchTimeOnForm
*dlSearchTimeOnStartLBL.isCompound: "true"
*dlSearchTimeOnStartLBL.compoundIcon: "label.xpm"
*dlSearchTimeOnStartLBL.compoundName: "label_"
*dlSearchTimeOnStartLBL.x: 6
*dlSearchTimeOnStartLBL.y: 11
*dlSearchTimeOnStartLBL.labelString: "Start:"
*dlSearchTimeOnStartLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOnStartLBL.width: 54
*dlSearchTimeOnStartLBL.background: "#9ac0cd"
*dlSearchTimeOnStartLBL.height: 22

*dlSearchTimeOnEndLBL.class: label
*dlSearchTimeOnEndLBL.static: true
*dlSearchTimeOnEndLBL.name: dlSearchTimeOnEndLBL
*dlSearchTimeOnEndLBL.parent: dlSearchTimeOnForm
*dlSearchTimeOnEndLBL.isCompound: "true"
*dlSearchTimeOnEndLBL.compoundIcon: "label.xpm"
*dlSearchTimeOnEndLBL.compoundName: "label_"
*dlSearchTimeOnEndLBL.x: 230
*dlSearchTimeOnEndLBL.y: 11
*dlSearchTimeOnEndLBL.labelString: "End:"
*dlSearchTimeOnEndLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOnEndLBL.width: 44
*dlSearchTimeOnEndLBL.background: "#9ac0cd"
*dlSearchTimeOnEndLBL.height: 22

*dlSearchTimeOnStartText.class: text
*dlSearchTimeOnStartText.static: true
*dlSearchTimeOnStartText.name: dlSearchTimeOnStartText
*dlSearchTimeOnStartText.parent: dlSearchTimeOnForm
*dlSearchTimeOnStartText.width: 160
*dlSearchTimeOnStartText.isCompound: "true"
*dlSearchTimeOnStartText.compoundIcon: "text.xpm"
*dlSearchTimeOnStartText.compoundName: "text_"
*dlSearchTimeOnStartText.x: 62
*dlSearchTimeOnStartText.y: 3
*dlSearchTimeOnStartText.background: "LightSkyBlue3"
*dlSearchTimeOnStartText.height: 36
*dlSearchTimeOnStartText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOnStartText.columns: 10
*dlSearchTimeOnStartText.text: ""
*dlSearchTimeOnStartText.modifyVerifyCallback.source: public
*dlSearchTimeOnStartText.modifyVerifyCallback: dl_search_time_modifyVerifyCb
*dlSearchTimeOnStartText.activateCallback.source: public
*dlSearchTimeOnStartText.activateCallback: dl_search_time_loseFocusCb

*dlSearchTimeOnEndText.class: text
*dlSearchTimeOnEndText.static: true
*dlSearchTimeOnEndText.name: dlSearchTimeOnEndText
*dlSearchTimeOnEndText.parent: dlSearchTimeOnForm
*dlSearchTimeOnEndText.width: 160
*dlSearchTimeOnEndText.isCompound: "true"
*dlSearchTimeOnEndText.compoundIcon: "text.xpm"
*dlSearchTimeOnEndText.compoundName: "text_"
*dlSearchTimeOnEndText.x: 278
*dlSearchTimeOnEndText.y: 3
*dlSearchTimeOnEndText.background: "LightSkyBlue3"
*dlSearchTimeOnEndText.height: 36
*dlSearchTimeOnEndText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOnEndText.columns: 10
*dlSearchTimeOnEndText.cursorPosition: 0
*dlSearchTimeOnEndText.text: ""
*dlSearchTimeOnEndText.modifyVerifyCallback.source: public
*dlSearchTimeOnEndText.modifyVerifyCallback: dl_search_time_modifyVerifyCb
*dlSearchTimeOnEndText.activateCallback.source: public
*dlSearchTimeOnEndText.activateCallback: dl_search_time_loseFocusCb

*dlSearchTimeOnLBL.class: label
*dlSearchTimeOnLBL.static: true
*dlSearchTimeOnLBL.name: dlSearchTimeOnLBL
*dlSearchTimeOnLBL.parent: DLtoDTKsearch
*dlSearchTimeOnLBL.isCompound: "true"
*dlSearchTimeOnLBL.compoundIcon: "label.xpm"
*dlSearchTimeOnLBL.compoundName: "label_"
*dlSearchTimeOnLBL.x: 459
*dlSearchTimeOnLBL.y: 431
*dlSearchTimeOnLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOnLBL.labelString: "Time On:"
*dlSearchTimeOnLBL.background: "#9ac0cd"

*dlSearchTimeOffLBL.class: label
*dlSearchTimeOffLBL.static: true
*dlSearchTimeOffLBL.name: dlSearchTimeOffLBL
*dlSearchTimeOffLBL.parent: DLtoDTKsearch
*dlSearchTimeOffLBL.isCompound: "true"
*dlSearchTimeOffLBL.compoundIcon: "label.xpm"
*dlSearchTimeOffLBL.compoundName: "label_"
*dlSearchTimeOffLBL.x: 457
*dlSearchTimeOffLBL.y: 483
*dlSearchTimeOffLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOffLBL.labelString: "Time Off:"
*dlSearchTimeOffLBL.background: "#9ac0cd"

*dlSearchTimeOffFrame.class: frame
*dlSearchTimeOffFrame.static: true
*dlSearchTimeOffFrame.name: dlSearchTimeOffFrame
*dlSearchTimeOffFrame.parent: DLtoDTKsearch
*dlSearchTimeOffFrame.width: 455
*dlSearchTimeOffFrame.height: 46
*dlSearchTimeOffFrame.isCompound: "true"
*dlSearchTimeOffFrame.compoundIcon: "frame.xpm"
*dlSearchTimeOffFrame.compoundName: "frame_"
*dlSearchTimeOffFrame.x: 546
*dlSearchTimeOffFrame.y: 466
*dlSearchTimeOffFrame.shadowType: "shadow_etched_in"

*dlSearchTimeOffForm.class: form
*dlSearchTimeOffForm.static: true
*dlSearchTimeOffForm.name: dlSearchTimeOffForm
*dlSearchTimeOffForm.parent: dlSearchTimeOffFrame
*dlSearchTimeOffForm.width: 405
*dlSearchTimeOffForm.height: 42
*dlSearchTimeOffForm.resizePolicy: "resize_none"
*dlSearchTimeOffForm.isCompound: "true"
*dlSearchTimeOffForm.compoundIcon: "form.xpm"
*dlSearchTimeOffForm.compoundName: "form_"
*dlSearchTimeOffForm.x: 2
*dlSearchTimeOffForm.y: 2
*dlSearchTimeOffForm.background: "#9ac0cd"

*dlSearchTimeOffStartLBL.class: label
*dlSearchTimeOffStartLBL.static: true
*dlSearchTimeOffStartLBL.name: dlSearchTimeOffStartLBL
*dlSearchTimeOffStartLBL.parent: dlSearchTimeOffForm
*dlSearchTimeOffStartLBL.isCompound: "true"
*dlSearchTimeOffStartLBL.compoundIcon: "label.xpm"
*dlSearchTimeOffStartLBL.compoundName: "label_"
*dlSearchTimeOffStartLBL.x: 6
*dlSearchTimeOffStartLBL.y: 11
*dlSearchTimeOffStartLBL.labelString: "Start:"
*dlSearchTimeOffStartLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOffStartLBL.width: 54
*dlSearchTimeOffStartLBL.background: "#9ac0cd"

*dlSearchTimeOffEndLBL.class: label
*dlSearchTimeOffEndLBL.static: true
*dlSearchTimeOffEndLBL.name: dlSearchTimeOffEndLBL
*dlSearchTimeOffEndLBL.parent: dlSearchTimeOffForm
*dlSearchTimeOffEndLBL.isCompound: "true"
*dlSearchTimeOffEndLBL.compoundIcon: "label.xpm"
*dlSearchTimeOffEndLBL.compoundName: "label_"
*dlSearchTimeOffEndLBL.x: 230
*dlSearchTimeOffEndLBL.y: 10
*dlSearchTimeOffEndLBL.labelString: "End:"
*dlSearchTimeOffEndLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOffEndLBL.width: 45
*dlSearchTimeOffEndLBL.background: "#9ac0cd"

*dlSearchTimeOffStartText.class: text
*dlSearchTimeOffStartText.static: true
*dlSearchTimeOffStartText.name: dlSearchTimeOffStartText
*dlSearchTimeOffStartText.parent: dlSearchTimeOffForm
*dlSearchTimeOffStartText.width: 160
*dlSearchTimeOffStartText.isCompound: "true"
*dlSearchTimeOffStartText.compoundIcon: "text.xpm"
*dlSearchTimeOffStartText.compoundName: "text_"
*dlSearchTimeOffStartText.x: 62
*dlSearchTimeOffStartText.y: 3
*dlSearchTimeOffStartText.background: "LightSkyBlue3"
*dlSearchTimeOffStartText.height: 36
*dlSearchTimeOffStartText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOffStartText.columns: 10
*dlSearchTimeOffStartText.modifyVerifyCallback.source: public
*dlSearchTimeOffStartText.modifyVerifyCallback: dl_search_time_modifyVerifyCb
*dlSearchTimeOffStartText.activateCallback.source: public
*dlSearchTimeOffStartText.activateCallback: dl_search_time_loseFocusCb

*dlSearchTimeOffEndText.class: text
*dlSearchTimeOffEndText.static: true
*dlSearchTimeOffEndText.name: dlSearchTimeOffEndText
*dlSearchTimeOffEndText.parent: dlSearchTimeOffForm
*dlSearchTimeOffEndText.width: 160
*dlSearchTimeOffEndText.isCompound: "true"
*dlSearchTimeOffEndText.compoundIcon: "text.xpm"
*dlSearchTimeOffEndText.compoundName: "text_"
*dlSearchTimeOffEndText.x: 277
*dlSearchTimeOffEndText.y: 2
*dlSearchTimeOffEndText.background: "LightSkyBlue3"
*dlSearchTimeOffEndText.height: 36
*dlSearchTimeOffEndText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchTimeOffEndText.columns: 10
*dlSearchTimeOffEndText.modifyVerifyCallback.source: public
*dlSearchTimeOffEndText.modifyVerifyCallback: dl_search_time_modifyVerifyCb
*dlSearchTimeOffEndText.activateCallback.source: public
*dlSearchTimeOffEndText.activateCallback: dl_search_time_loseFocusCb

*frame2.class: frame
*frame2.static: true
*frame2.name: frame2
*frame2.parent: DLtoDTKsearch
*frame2.width: 455
*frame2.height: 47
*frame2.isCompound: "true"
*frame2.compoundIcon: "frame.xpm"
*frame2.compoundName: "frame_"
*frame2.x: 546
*frame2.y: 370
*frame2.background: "#9ac0cd"
*frame2.shadowType: "shadow_etched_in"
*frame2.shadowThickness: 3

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: frame2
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: 3
*label2.y: 4
*label2.background: "#9ac0cd"
*label2.labelString: " Date Formats:  YYYY-DDDTHH:MM:SS.fff\n               YYYY-MM-DD:HH:MM:SS.fff"
*label2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label2.width: 449
*label2.height: 40

*dlSearchSeparator2.class: separator
*dlSearchSeparator2.static: true
*dlSearchSeparator2.name: dlSearchSeparator2
*dlSearchSeparator2.parent: DLtoDTKsearch
*dlSearchSeparator2.width: 1015
*dlSearchSeparator2.height: 8
*dlSearchSeparator2.isCompound: "true"
*dlSearchSeparator2.compoundIcon: "sep.xpm"
*dlSearchSeparator2.compoundName: "separator_"
*dlSearchSeparator2.x: 26
*dlSearchSeparator2.y: 524
*dlSearchSeparator2.background: "#9ac0cd"
*dlSearchSeparator2.separatorType: "shadow_etched_in"
*dlSearchSeparator2.shadowThickness: 3

*dlSearchScheduleLinkLBL.class: label
*dlSearchScheduleLinkLBL.static: true
*dlSearchScheduleLinkLBL.name: dlSearchScheduleLinkLBL
*dlSearchScheduleLinkLBL.parent: DLtoDTKsearch
*dlSearchScheduleLinkLBL.isCompound: "true"
*dlSearchScheduleLinkLBL.compoundIcon: "label.xpm"
*dlSearchScheduleLinkLBL.compoundName: "label_"
*dlSearchScheduleLinkLBL.x: 65
*dlSearchScheduleLinkLBL.y: 377
*dlSearchScheduleLinkLBL.background: "#9ac0cd"
*dlSearchScheduleLinkLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchScheduleLinkLBL.labelString: "FA Schedule Link:"
*dlSearchScheduleLinkLBL.width: 163
*dlSearchScheduleLinkLBL.height: 23

*dlSearchScheduleLinkTF.class: textField
*dlSearchScheduleLinkTF.static: true
*dlSearchScheduleLinkTF.name: dlSearchScheduleLinkTF
*dlSearchScheduleLinkTF.parent: DLtoDTKsearch
*dlSearchScheduleLinkTF.width: 200
*dlSearchScheduleLinkTF.isCompound: "true"
*dlSearchScheduleLinkTF.compoundIcon: "textfield.xpm"
*dlSearchScheduleLinkTF.compoundName: "text_Field"
*dlSearchScheduleLinkTF.x: 236
*dlSearchScheduleLinkTF.y: 371
*dlSearchScheduleLinkTF.background: "LightSkyBlue3"
*dlSearchScheduleLinkTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*dlSearchScheduleLinkTF.maxLength: 20
*dlSearchScheduleLinkTF.height: 35
*dlSearchScheduleLinkTF.activateCallback.source: public
*dlSearchScheduleLinkTF.activateCallback: XmProcessTraversal
*dlSearchScheduleLinkTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*dlSearchScheduleLinkTF.editable: "true"

*dlSearchRevolutionLBL.class: label
*dlSearchRevolutionLBL.static: true
*dlSearchRevolutionLBL.name: dlSearchRevolutionLBL
*dlSearchRevolutionLBL.parent: DLtoDTKsearch
*dlSearchRevolutionLBL.isCompound: "true"
*dlSearchRevolutionLBL.compoundIcon: "label.xpm"
*dlSearchRevolutionLBL.compoundName: "label_"
*dlSearchRevolutionLBL.x: 58
*dlSearchRevolutionLBL.y: 430
*dlSearchRevolutionLBL.background: "#9ac0cd"
*dlSearchRevolutionLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchRevolutionLBL.labelString: "Revolution:"
*dlSearchRevolutionLBL.width: 95
*dlSearchRevolutionLBL.height: 23

*dlSearchRevFrame.class: frame
*dlSearchRevFrame.static: true
*dlSearchRevFrame.name: dlSearchRevFrame
*dlSearchRevFrame.parent: DLtoDTKsearch
*dlSearchRevFrame.width: 282
*dlSearchRevFrame.height: 46
*dlSearchRevFrame.isCompound: "true"
*dlSearchRevFrame.compoundIcon: "frame.xpm"
*dlSearchRevFrame.compoundName: "frame_"
*dlSearchRevFrame.x: 156
*dlSearchRevFrame.y: 418
*dlSearchRevFrame.shadowType: "shadow_etched_in"
*dlSearchRevFrame.shadowThickness: 2

*dlSearchRevForm.class: form
*dlSearchRevForm.static: true
*dlSearchRevForm.name: dlSearchRevForm
*dlSearchRevForm.parent: dlSearchRevFrame
*dlSearchRevForm.width: 285
*dlSearchRevForm.height: 47
*dlSearchRevForm.resizePolicy: "resize_none"
*dlSearchRevForm.isCompound: "true"
*dlSearchRevForm.compoundIcon: "form.xpm"
*dlSearchRevForm.compoundName: "form_"
*dlSearchRevForm.x: 2
*dlSearchRevForm.y: 2
*dlSearchRevForm.background: "#9ac0cd"
*dlSearchRevForm.shadowType: "shadow_out"
*dlSearchRevForm.shadowThickness: 0

*dlSearchRevStartLBL.class: label
*dlSearchRevStartLBL.static: true
*dlSearchRevStartLBL.name: dlSearchRevStartLBL
*dlSearchRevStartLBL.parent: dlSearchRevForm
*dlSearchRevStartLBL.isCompound: "true"
*dlSearchRevStartLBL.compoundIcon: "label.xpm"
*dlSearchRevStartLBL.compoundName: "label_"
*dlSearchRevStartLBL.x: 3
*dlSearchRevStartLBL.y: 12
*dlSearchRevStartLBL.labelString: "Start:"
*dlSearchRevStartLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchRevStartLBL.width: 54
*dlSearchRevStartLBL.background: "#9ac0cd"
*dlSearchRevStartLBL.height: 22

*dlSearchRevEndLBL.class: label
*dlSearchRevEndLBL.static: true
*dlSearchRevEndLBL.name: dlSearchRevEndLBL
*dlSearchRevEndLBL.parent: dlSearchRevForm
*dlSearchRevEndLBL.isCompound: "true"
*dlSearchRevEndLBL.compoundIcon: "label.xpm"
*dlSearchRevEndLBL.compoundName: "label_"
*dlSearchRevEndLBL.x: 146
*dlSearchRevEndLBL.y: 12
*dlSearchRevEndLBL.labelString: "End:"
*dlSearchRevEndLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchRevEndLBL.width: 44
*dlSearchRevEndLBL.background: "#9ac0cd"
*dlSearchRevEndLBL.height: 22

*dlSearchRevStartText.class: text
*dlSearchRevStartText.static: true
*dlSearchRevStartText.name: dlSearchRevStartText
*dlSearchRevStartText.parent: dlSearchRevForm
*dlSearchRevStartText.width: 70
*dlSearchRevStartText.isCompound: "true"
*dlSearchRevStartText.compoundIcon: "text.xpm"
*dlSearchRevStartText.compoundName: "text_"
*dlSearchRevStartText.x: 59
*dlSearchRevStartText.y: 3
*dlSearchRevStartText.background: "LightSkyBlue3"
*dlSearchRevStartText.height: 36
*dlSearchRevStartText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchRevStartText.columns: 10
*dlSearchRevStartText.text: ""
*dlSearchRevStartText.modifyVerifyCallback.source: public
*dlSearchRevStartText.modifyVerifyCallback: dl_search_rev_modifyVerifyCb
*dlSearchRevStartText.activateCallback.source: public
*dlSearchRevStartText.activateCallback: dl_search_rev_loseFocusCb

*dlSearchRevEndText.class: text
*dlSearchRevEndText.static: true
*dlSearchRevEndText.name: dlSearchRevEndText
*dlSearchRevEndText.parent: dlSearchRevForm
*dlSearchRevEndText.width: 70
*dlSearchRevEndText.isCompound: "true"
*dlSearchRevEndText.compoundIcon: "text.xpm"
*dlSearchRevEndText.compoundName: "text_"
*dlSearchRevEndText.x: 194
*dlSearchRevEndText.y: 3
*dlSearchRevEndText.background: "LightSkyBlue3"
*dlSearchRevEndText.height: 36
*dlSearchRevEndText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchRevEndText.columns: 10
*dlSearchRevEndText.cursorPosition: 0
*dlSearchRevEndText.text: ""
*dlSearchRevEndText.modifyVerifyCallback.source: public
*dlSearchRevEndText.modifyVerifyCallback: dl_search_rev_modifyVerifyCb
*dlSearchRevEndText.activateCallback.source: public
*dlSearchRevEndText.activateCallback: dl_search_rev_loseFocusCb

*dlSearchSequenceLBL.class: label
*dlSearchSequenceLBL.static: true
*dlSearchSequenceLBL.name: dlSearchSequenceLBL
*dlSearchSequenceLBL.parent: DLtoDTKsearch
*dlSearchSequenceLBL.isCompound: "true"
*dlSearchSequenceLBL.compoundIcon: "label.xpm"
*dlSearchSequenceLBL.compoundName: "label_"
*dlSearchSequenceLBL.x: 65
*dlSearchSequenceLBL.y: 478
*dlSearchSequenceLBL.background: "#9ac0cd"
*dlSearchSequenceLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchSequenceLBL.labelString: "Sequence:"
*dlSearchSequenceLBL.width: 88
*dlSearchSequenceLBL.height: 23

*dlSearchSequenceFrame.class: frame
*dlSearchSequenceFrame.static: true
*dlSearchSequenceFrame.name: dlSearchSequenceFrame
*dlSearchSequenceFrame.parent: DLtoDTKsearch
*dlSearchSequenceFrame.width: 282
*dlSearchSequenceFrame.height: 46
*dlSearchSequenceFrame.isCompound: "true"
*dlSearchSequenceFrame.compoundIcon: "frame.xpm"
*dlSearchSequenceFrame.compoundName: "frame_"
*dlSearchSequenceFrame.x: 156
*dlSearchSequenceFrame.y: 466
*dlSearchSequenceFrame.shadowType: "shadow_etched_in"
*dlSearchSequenceFrame.shadowThickness: 2

*dlSearchSequenceForm.class: form
*dlSearchSequenceForm.static: true
*dlSearchSequenceForm.name: dlSearchSequenceForm
*dlSearchSequenceForm.parent: dlSearchSequenceFrame
*dlSearchSequenceForm.width: 285
*dlSearchSequenceForm.height: 47
*dlSearchSequenceForm.resizePolicy: "resize_none"
*dlSearchSequenceForm.isCompound: "true"
*dlSearchSequenceForm.compoundIcon: "form.xpm"
*dlSearchSequenceForm.compoundName: "form_"
*dlSearchSequenceForm.x: 2
*dlSearchSequenceForm.y: 2
*dlSearchSequenceForm.background: "#9ac0cd"
*dlSearchSequenceForm.shadowType: "shadow_out"
*dlSearchSequenceForm.shadowThickness: 0

*dlSearchSequenceStartLBL.class: label
*dlSearchSequenceStartLBL.static: true
*dlSearchSequenceStartLBL.name: dlSearchSequenceStartLBL
*dlSearchSequenceStartLBL.parent: dlSearchSequenceForm
*dlSearchSequenceStartLBL.isCompound: "true"
*dlSearchSequenceStartLBL.compoundIcon: "label.xpm"
*dlSearchSequenceStartLBL.compoundName: "label_"
*dlSearchSequenceStartLBL.x: 2
*dlSearchSequenceStartLBL.y: 11
*dlSearchSequenceStartLBL.labelString: "Start:"
*dlSearchSequenceStartLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchSequenceStartLBL.width: 54
*dlSearchSequenceStartLBL.background: "#9ac0cd"
*dlSearchSequenceStartLBL.height: 22

*dlSearchSequenceEndLBL.class: label
*dlSearchSequenceEndLBL.static: true
*dlSearchSequenceEndLBL.name: dlSearchSequenceEndLBL
*dlSearchSequenceEndLBL.parent: dlSearchSequenceForm
*dlSearchSequenceEndLBL.isCompound: "true"
*dlSearchSequenceEndLBL.compoundIcon: "label.xpm"
*dlSearchSequenceEndLBL.compoundName: "label_"
*dlSearchSequenceEndLBL.x: 145
*dlSearchSequenceEndLBL.y: 11
*dlSearchSequenceEndLBL.labelString: "End:"
*dlSearchSequenceEndLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchSequenceEndLBL.width: 44
*dlSearchSequenceEndLBL.background: "#9ac0cd"
*dlSearchSequenceEndLBL.height: 22

*dlSearchSequenceStartText.class: text
*dlSearchSequenceStartText.static: true
*dlSearchSequenceStartText.name: dlSearchSequenceStartText
*dlSearchSequenceStartText.parent: dlSearchSequenceForm
*dlSearchSequenceStartText.width: 70
*dlSearchSequenceStartText.isCompound: "true"
*dlSearchSequenceStartText.compoundIcon: "text.xpm"
*dlSearchSequenceStartText.compoundName: "text_"
*dlSearchSequenceStartText.x: 58
*dlSearchSequenceStartText.y: 2
*dlSearchSequenceStartText.background: "LightSkyBlue3"
*dlSearchSequenceStartText.height: 36
*dlSearchSequenceStartText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchSequenceStartText.columns: 10
*dlSearchSequenceStartText.text: ""
*dlSearchSequenceStartText.modifyVerifyCallback.source: public
*dlSearchSequenceStartText.modifyVerifyCallback: dl_search_sequence_modifyVerifyCb
*dlSearchSequenceStartText.activateCallback.source: public
*dlSearchSequenceStartText.activateCallback: dl_search_sequence_loseFocusCb

*dlSearchSequenceEndText.class: text
*dlSearchSequenceEndText.static: true
*dlSearchSequenceEndText.name: dlSearchSequenceEndText
*dlSearchSequenceEndText.parent: dlSearchSequenceForm
*dlSearchSequenceEndText.width: 70
*dlSearchSequenceEndText.isCompound: "true"
*dlSearchSequenceEndText.compoundIcon: "text.xpm"
*dlSearchSequenceEndText.compoundName: "text_"
*dlSearchSequenceEndText.x: 193
*dlSearchSequenceEndText.y: 2
*dlSearchSequenceEndText.background: "LightSkyBlue3"
*dlSearchSequenceEndText.height: 36
*dlSearchSequenceEndText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchSequenceEndText.columns: 10
*dlSearchSequenceEndText.cursorPosition: 0
*dlSearchSequenceEndText.text: ""
*dlSearchSequenceEndText.modifyVerifyCallback.source: public
*dlSearchSequenceEndText.modifyVerifyCallback: dl_search_sequence_modifyVerifyCb
*dlSearchSequenceEndText.activateCallback.source: public
*dlSearchSequenceEndText.activateCallback: dl_search_sequence_loseFocusCb

*dlSearchStatusLBL.class: label
*dlSearchStatusLBL.static: true
*dlSearchStatusLBL.name: dlSearchStatusLBL
*dlSearchStatusLBL.parent: DLtoDTKsearch
*dlSearchStatusLBL.isCompound: "true"
*dlSearchStatusLBL.compoundIcon: "label.xpm"
*dlSearchStatusLBL.compoundName: "label_"
*dlSearchStatusLBL.x: 887
*dlSearchStatusLBL.y: 84
*dlSearchStatusLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*dlSearchStatusLBL.labelString: "Downlink Status"
*dlSearchStatusLBL.background: "#9ac0cd"

*dlSearchStatusSW.class: scrolledWindow
*dlSearchStatusSW.static: true
*dlSearchStatusSW.name: dlSearchStatusSW
*dlSearchStatusSW.parent: DLtoDTKsearch
*dlSearchStatusSW.scrollingPolicy: "automatic"
*dlSearchStatusSW.width: 165
*dlSearchStatusSW.height: 230
*dlSearchStatusSW.isCompound: "true"
*dlSearchStatusSW.compoundIcon: "scrlwnd.xpm"
*dlSearchStatusSW.compoundName: "scrolled_Window"
*dlSearchStatusSW.x: 872
*dlSearchStatusSW.y: 112
*dlSearchStatusSW.background: "LightSkyBlue3"
*dlSearchStatusSW.traversalOn: "false"

*statusRC.class: rowColumn
*statusRC.static: true
*statusRC.name: statusRC
*statusRC.parent: dlSearchStatusSW
*statusRC.isCompound: "true"
*statusRC.compoundIcon: "row.xpm"
*statusRC.compoundName: "row_Column"
*statusRC.x: 2
*statusRC.y: 2
*statusRC.width: 134
*statusRC.height: 226
*statusRC.resizeHeight: "true"
*statusRC.resizeWidth: "false"
*statusRC.spacing: 0
*statusRC.marginHeight: 5
*statusRC.traversalOn: "false"

