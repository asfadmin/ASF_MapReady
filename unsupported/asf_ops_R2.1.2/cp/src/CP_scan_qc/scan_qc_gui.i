! UIMX ascii 2.9 key: 8324                                                      

*scanQcDialogShell.class: dialogShell
*scanQcDialogShell.classinc:
*scanQcDialogShell.classspec:
*scanQcDialogShell.classmembers:
*scanQcDialogShell.classconstructor:
*scanQcDialogShell.classdestructor:
*scanQcDialogShell.gbldecl: /*----------------------------------------------------------\
 * SCCS Header\
 * File:selectFrame.i   Rev:1.3.0.0   Date:95/09/25\
 *\
 *---------------------------------------------------------*/\
#include <stdio.h>\
#include "scan_qc_def.h"\
static char sccsid_selectFrame_i[] = "@(#)scan_qc_gui.i	1.13 97/04/30 14:45:06"; \

*scanQcDialogShell.ispecdecl:
*scanQcDialogShell.funcdecl: swidget create_scanQcDialogShell(swidget UxParent)
*scanQcDialogShell.funcname: create_scanQcDialogShell
*scanQcDialogShell.funcdef: "swidget", "<create_scanQcDialogShell>(%)"
*scanQcDialogShell.argdecl: swidget UxParent;
*scanQcDialogShell.arglist: UxParent
*scanQcDialogShell.arglist.UxParent: "swidget", "%UxParent%"
*scanQcDialogShell.icode:
*scanQcDialogShell.fcode: return(rtrn);\

*scanQcDialogShell.auxdecl:
*scanQcDialogShell.name.source: public
*scanQcDialogShell.static: false
*scanQcDialogShell.name: scanQcDialogShell
*scanQcDialogShell.parent: NO_PARENT
*scanQcDialogShell.parentExpression: UxParent
*scanQcDialogShell.x: 0
*scanQcDialogShell.y: 0
*scanQcDialogShell.width: 700
*scanQcDialogShell.height: 880
*scanQcDialogShell.buttonFontList: "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
*scanQcDialogShell.labelFontList: "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
*scanQcDialogShell.textFontList: "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
*scanQcDialogShell.title: "CP_scan_qc"
*scanQcDialogShell.defaultFontList: "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
*scanQcDialogShell.destroyCallback: doExit();
*scanQcDialogShell.isInCompound: "true"
*scanQcDialogShell.isNovice: "true"
*scanQcDialogShell.isCompound: "true"
*scanQcDialogShell.popdownCallback: doExit();

*scanQcMainWindow.class: mainWindow
*scanQcMainWindow.name.source: public
*scanQcMainWindow.static: false
*scanQcMainWindow.name: scanQcMainWindow
*scanQcMainWindow.parent: scanQcDialogShell
*scanQcMainWindow.unitType: "pixels"
*scanQcMainWindow.x: 0
*scanQcMainWindow.y: 0
*scanQcMainWindow.width: 700
*scanQcMainWindow.height: 880

*scanQcMenu.class: rowColumn
*scanQcMenu.static: true
*scanQcMenu.name: scanQcMenu
*scanQcMenu.parent: scanQcMainWindow
*scanQcMenu.rowColumnType: "menu_bar"
*scanQcMenu.menuHelpWidget: "menu5_top_b2"
*scanQcMenu.menuAccelerator: "<KeyUp>F10"

*file.class: rowColumn
*file.static: true
*file.name: file
*file.parent: scanQcMenu
*file.rowColumnType: "menu_pulldown"

*printScanQcWindow.class: pushButtonGadget
*printScanQcWindow.static: true
*printScanQcWindow.name: printScanQcWindow
*printScanQcWindow.parent: file
*printScanQcWindow.labelString: "Print"
*printScanQcWindow.mnemonic: "P"
*printScanQcWindow.activateCallback: print_cb(scanQcDialogShell);

*exitScanQc.class: pushButtonGadget
*exitScanQc.static: true
*exitScanQc.name: exitScanQc
*exitScanQc.parent: file
*exitScanQc.labelString: "Exit"
*exitScanQc.mnemonic: "x"
*exitScanQc.activateCallback: hold_cb();

*help.class: rowColumn
*help.static: true
*help.name: help
*help.parent: scanQcMenu
*help.rowColumnType: "menu_pulldown"

*helpOnOverview.class: pushButtonGadget
*helpOnOverview.static: true
*helpOnOverview.name: helpOnOverview
*helpOnOverview.parent: help
*helpOnOverview.labelString: "Overview"
*helpOnOverview.mnemonic: "O"
*helpOnOverview.activateCallback: help_cb(0);

*helpOnUsingHelp.class: pushButtonGadget
*helpOnUsingHelp.static: true
*helpOnUsingHelp.name: helpOnUsingHelp
*helpOnUsingHelp.parent: help
*helpOnUsingHelp.labelString: "Using Help"
*helpOnUsingHelp.mnemonic: "H"
*helpOnUsingHelp.activateCallback: help_cb(1);

*helpOnProductInfo.class: pushButtonGadget
*helpOnProductInfo.static: true
*helpOnProductInfo.name: helpOnProductInfo
*helpOnProductInfo.parent: help
*helpOnProductInfo.labelString: "Product Information"
*helpOnProductInfo.mnemonic: "P"
*helpOnProductInfo.activateCallback: help_cb(2);

*menu5_top_b1.class: cascadeButton
*menu5_top_b1.static: true
*menu5_top_b1.name: menu5_top_b1
*menu5_top_b1.parent: scanQcMenu
*menu5_top_b1.labelString: "File"
*menu5_top_b1.mnemonic: "F"
*menu5_top_b1.subMenuId: "file"

*menu5_top_b2.class: cascadeButton
*menu5_top_b2.static: true
*menu5_top_b2.name: menu5_top_b2
*menu5_top_b2.parent: scanQcMenu
*menu5_top_b2.labelString: "Help"
*menu5_top_b2.mnemonic: "H"
*menu5_top_b2.subMenuId: "help"

*scanQcBulletinBoard.class: bulletinBoard
*scanQcBulletinBoard.static: true
*scanQcBulletinBoard.name: scanQcBulletinBoard
*scanQcBulletinBoard.parent: scanQcMainWindow
*scanQcBulletinBoard.width: 700

*platformLbl.class: labelGadget
*platformLbl.static: true
*platformLbl.name: platformLbl
*platformLbl.parent: scanQcBulletinBoard
*platformLbl.x: 60
*platformLbl.y: 10
*platformLbl.width: 90
*platformLbl.height: 30
*platformLbl.alignment: "alignment_beginning"
*platformLbl.labelString: "Platform:"

*platformData.class: labelGadget
*platformData.name.source: public
*platformData.static: false
*platformData.name: platformData
*platformData.parent: scanQcBulletinBoard
*platformData.x: 150
*platformData.y: 10
*platformData.width: 100
*platformData.height: 30
*platformData.labelString: ""
*platformData.alignment: "alignment_beginning"
*platformData.marginHeight: 8

*stationLbl.class: labelGadget
*stationLbl.static: true
*stationLbl.name: stationLbl
*stationLbl.parent: scanQcBulletinBoard
*stationLbl.x: 500
*stationLbl.y: 40
*stationLbl.width: 80
*stationLbl.height: 30
*stationLbl.alignment: "alignment_beginning"
*stationLbl.labelString: "Station:"

*stationData.class: labelGadget
*stationData.name.source: public
*stationData.static: false
*stationData.name: stationData
*stationData.parent: scanQcBulletinBoard
*stationData.x: 580
*stationData.y: 40
*stationData.width: 80
*stationData.height: 30
*stationData.labelString: ""
*stationData.alignment: "alignment_beginning"
*stationData.marginHeight: 8

*revolutionLbl.class: labelGadget
*revolutionLbl.static: true
*revolutionLbl.name: revolutionLbl
*revolutionLbl.parent: scanQcBulletinBoard
*revolutionLbl.x: 60
*revolutionLbl.y: 40
*revolutionLbl.width: 90
*revolutionLbl.height: 30
*revolutionLbl.alignment: "alignment_beginning"
*revolutionLbl.labelString: "Revolution:"

*revolutionData.class: labelGadget
*revolutionData.name.source: public
*revolutionData.static: false
*revolutionData.name: revolutionData
*revolutionData.parent: scanQcBulletinBoard
*revolutionData.x: 150
*revolutionData.y: 40
*revolutionData.width: 100
*revolutionData.height: 30
*revolutionData.labelString: ""
*revolutionData.alignment: "alignment_beginning"
*revolutionData.marginHeight: 8

*instrumentModeLbl.class: labelGadget
*instrumentModeLbl.static: true
*instrumentModeLbl.name: instrumentModeLbl
*instrumentModeLbl.parent: scanQcBulletinBoard
*instrumentModeLbl.x: 280
*instrumentModeLbl.y: 10
*instrumentModeLbl.width: 80
*instrumentModeLbl.height: 30
*instrumentModeLbl.alignment: "alignment_beginning"
*instrumentModeLbl.labelString: "Mode:"

*instrumentModeData.class: labelGadget
*instrumentModeData.name.source: public
*instrumentModeData.static: false
*instrumentModeData.name: instrumentModeData
*instrumentModeData.parent: scanQcBulletinBoard
*instrumentModeData.x: 360
*instrumentModeData.y: 10
*instrumentModeData.width: 80
*instrumentModeData.height: 30
*instrumentModeData.labelString: ""
*instrumentModeData.alignment: "alignment_beginning"
*instrumentModeData.marginHeight: 8

*sequenceLbl.class: labelGadget
*sequenceLbl.static: true
*sequenceLbl.name: sequenceLbl
*sequenceLbl.parent: scanQcBulletinBoard
*sequenceLbl.x: 280
*sequenceLbl.y: 40
*sequenceLbl.width: 80
*sequenceLbl.height: 30
*sequenceLbl.alignment: "alignment_beginning"
*sequenceLbl.labelString: "Sequence:"

*sequenceData.class: labelGadget
*sequenceData.name.source: public
*sequenceData.static: false
*sequenceData.name: sequenceData
*sequenceData.parent: scanQcBulletinBoard
*sequenceData.x: 360
*sequenceData.y: 40
*sequenceData.width: 80
*sequenceData.height: 30
*sequenceData.alignment: "alignment_beginning"
*sequenceData.labelString: ""
*sequenceData.marginHeight: 8

*frameSizeData.class: labelGadget
*frameSizeData.name.source: public
*frameSizeData.static: false
*frameSizeData.name: frameSizeData
*frameSizeData.parent: scanQcBulletinBoard
*frameSizeData.x: 150
*frameSizeData.y: 70
*frameSizeData.width: 50
*frameSizeData.height: 30
*frameSizeData.labelString: ""
*frameSizeData.alignment: "alignment_beginning"
*frameSizeData.marginHeight: 8

*segmentNbrLbl.class: labelGadget
*segmentNbrLbl.static: true
*segmentNbrLbl.name: segmentNbrLbl
*segmentNbrLbl.parent: scanQcBulletinBoard
*segmentNbrLbl.x: 60
*segmentNbrLbl.y: 122
*segmentNbrLbl.width: 200
*segmentNbrLbl.height: 30
*segmentNbrLbl.alignment: "alignment_beginning"
*segmentNbrLbl.labelString: "Number of Segments:"

*segmentNbrData.class: labelGadget
*segmentNbrData.name.source: public
*segmentNbrData.static: false
*segmentNbrData.name: segmentNbrData
*segmentNbrData.parent: scanQcBulletinBoard
*segmentNbrData.x: 260
*segmentNbrData.y: 122
*segmentNbrData.width: 60
*segmentNbrData.height: 30
*segmentNbrData.alignment: "alignment_beginning"
*segmentNbrData.labelString: ""
*segmentNbrData.marginHeight: 8

*segemntSeparator.class: separatorGadget
*segemntSeparator.static: true
*segemntSeparator.name: segemntSeparator
*segemntSeparator.parent: scanQcBulletinBoard
*segemntSeparator.x: 10
*segemntSeparator.y: 101
*segemntSeparator.width: 680
*segemntSeparator.height: 10

*frameSeparator.class: separatorGadget
*frameSeparator.static: true
*frameSeparator.name: frameSeparator
*frameSeparator.parent: scanQcBulletinBoard
*frameSeparator.x: 10
*frameSeparator.y: 414
*frameSeparator.width: 680
*frameSeparator.height: 10

*OKSelectFrame.class: pushButtonGadget
*OKSelectFrame.name.source: public
*OKSelectFrame.static: false
*OKSelectFrame.name: OKSelectFrame
*OKSelectFrame.parent: scanQcBulletinBoard
*OKSelectFrame.x: 90
*OKSelectFrame.y: 770
*OKSelectFrame.width: 100
*OKSelectFrame.height: 50
*OKSelectFrame.labelString: "Accept"
*OKSelectFrame.activateCallback: {\
accept_cb ();\
}

*frameScrollBar.class: scrollBar
*frameScrollBar.name.source: public
*frameScrollBar.static: false
*frameScrollBar.name: frameScrollBar
*frameScrollBar.parent: scanQcBulletinBoard
*frameScrollBar.x: 639
*frameScrollBar.y: 533
*frameScrollBar.width: 20
*frameScrollBar.height: 200
*frameScrollBar.shadowThickness: 3
*frameScrollBar.valueChangedCallback: moveFrameScrollBar(); 

*centerTimeLbl.class: labelGadget
*centerTimeLbl.static: true
*centerTimeLbl.name: centerTimeLbl
*centerTimeLbl.parent: scanQcBulletinBoard
*centerTimeLbl.x: 459
*centerTimeLbl.y: 483
*centerTimeLbl.width: 160
*centerTimeLbl.height: 30
*centerTimeLbl.labelString: "Center Time"

*frameCurrentTimeScrollWindow.class: scrolledWindow
*frameCurrentTimeScrollWindow.static: true
*frameCurrentTimeScrollWindow.name: frameCurrentTimeScrollWindow
*frameCurrentTimeScrollWindow.parent: scanQcBulletinBoard
*frameCurrentTimeScrollWindow.scrollingPolicy: "application_defined"
*frameCurrentTimeScrollWindow.x: 459
*frameCurrentTimeScrollWindow.y: 533
*frameCurrentTimeScrollWindow.visualPolicy: "variable"
*frameCurrentTimeScrollWindow.scrollBarDisplayPolicy: "static"
*frameCurrentTimeScrollWindow.shadowThickness: 0
*frameCurrentTimeScrollWindow.height: 200
*frameCurrentTimeScrollWindow.width: 160

*frameCenterTimeScrollList.class: scrolledList
*frameCenterTimeScrollList.name.source: public
*frameCenterTimeScrollList.static: false
*frameCenterTimeScrollList.name: frameCenterTimeScrollList
*frameCenterTimeScrollList.parent: frameCurrentTimeScrollWindow
*frameCenterTimeScrollList.width: 180
*frameCenterTimeScrollList.height: 200
*frameCenterTimeScrollList.shadowThickness: 3
*frameCenterTimeScrollList.visibleItemCount: 10
*frameCenterTimeScrollList.stringDirection: "string_direction_r_to_l"
*frameCenterTimeScrollList.listSizePolicy: "constant"
*frameCenterTimeScrollList.browseSelectionCallback: pickFrameListEntry(timeList);

*frameCenterLonScrollWindow.class: scrolledWindow
*frameCenterLonScrollWindow.static: true
*frameCenterLonScrollWindow.name: frameCenterLonScrollWindow
*frameCenterLonScrollWindow.parent: scanQcBulletinBoard
*frameCenterLonScrollWindow.scrollingPolicy: "application_defined"
*frameCenterLonScrollWindow.x: 339
*frameCenterLonScrollWindow.y: 533
*frameCenterLonScrollWindow.visualPolicy: "variable"
*frameCenterLonScrollWindow.scrollBarDisplayPolicy: "static"
*frameCenterLonScrollWindow.shadowThickness: 0
*frameCenterLonScrollWindow.height: 200
*frameCenterLonScrollWindow.width: 100
*frameCenterLonScrollWindow.stringDirection: "string_direction_r_to_l"

*frameCenterLonScrollList.class: scrolledList
*frameCenterLonScrollList.name.source: public
*frameCenterLonScrollList.static: false
*frameCenterLonScrollList.name: frameCenterLonScrollList
*frameCenterLonScrollList.parent: frameCenterLonScrollWindow
*frameCenterLonScrollList.width: 100
*frameCenterLonScrollList.height: 200
*frameCenterLonScrollList.shadowThickness: 3
*frameCenterLonScrollList.visibleItemCount: 10
*frameCenterLonScrollList.listSizePolicy: "constant"
*frameCenterLonScrollList.browseSelectionCallback: pickFrameListEntry(longitudeList);

*frameCenterLatScrollWindow.class: scrolledWindow
*frameCenterLatScrollWindow.static: true
*frameCenterLatScrollWindow.name: frameCenterLatScrollWindow
*frameCenterLatScrollWindow.parent: scanQcBulletinBoard
*frameCenterLatScrollWindow.scrollingPolicy: "application_defined"
*frameCenterLatScrollWindow.x: 219
*frameCenterLatScrollWindow.y: 533
*frameCenterLatScrollWindow.visualPolicy: "variable"
*frameCenterLatScrollWindow.scrollBarDisplayPolicy: "static"
*frameCenterLatScrollWindow.shadowThickness: 0
*frameCenterLatScrollWindow.height: 200
*frameCenterLatScrollWindow.width: 100
*frameCenterLatScrollWindow.stringDirection: "string_direction_r_to_l"

*frameCenterLatScrollList.class: scrolledList
*frameCenterLatScrollList.name.source: public
*frameCenterLatScrollList.static: false
*frameCenterLatScrollList.name: frameCenterLatScrollList
*frameCenterLatScrollList.parent: frameCenterLatScrollWindow
*frameCenterLatScrollList.width: 100
*frameCenterLatScrollList.height: 200
*frameCenterLatScrollList.shadowThickness: 3
*frameCenterLatScrollList.visibleItemCount: 10
*frameCenterLatScrollList.listSizePolicy: "constant"
*frameCenterLatScrollList.sensitive: "true"
*frameCenterLatScrollList.singleSelectionCallback: pickFrameListEntry(latitudeList);
*frameCenterLatScrollList.browseSelectionCallback: pickFrameListEntry(latitudeList);

*frameFrameScrollWindow.class: scrolledWindow
*frameFrameScrollWindow.static: true
*frameFrameScrollWindow.name: frameFrameScrollWindow
*frameFrameScrollWindow.parent: scanQcBulletinBoard
*frameFrameScrollWindow.scrollingPolicy: "application_defined"
*frameFrameScrollWindow.x: 139
*frameFrameScrollWindow.y: 533
*frameFrameScrollWindow.visualPolicy: "variable"
*frameFrameScrollWindow.scrollBarDisplayPolicy: "static"
*frameFrameScrollWindow.shadowThickness: 0
*frameFrameScrollWindow.height: 200
*frameFrameScrollWindow.width: 60

*frameFrameScrollList.class: scrolledList
*frameFrameScrollList.name.source: public
*frameFrameScrollList.static: false
*frameFrameScrollList.name: frameFrameScrollList
*frameFrameScrollList.parent: frameFrameScrollWindow
*frameFrameScrollList.width: 60
*frameFrameScrollList.height: 200
*frameFrameScrollList.shadowThickness: 3
*frameFrameScrollList.visibleItemCount: 10
*frameFrameScrollList.listSizePolicy: "constant"
*frameFrameScrollList.stringDirection: "string_direction_r_to_l"
*frameFrameScrollList.browseSelectionCallback: pickFrameListEntry(frameList);

*frameSegmentScrollWindow.class: scrolledWindow
*frameSegmentScrollWindow.static: true
*frameSegmentScrollWindow.name: frameSegmentScrollWindow
*frameSegmentScrollWindow.parent: scanQcBulletinBoard
*frameSegmentScrollWindow.scrollingPolicy: "application_defined"
*frameSegmentScrollWindow.x: 59
*frameSegmentScrollWindow.y: 533
*frameSegmentScrollWindow.visualPolicy: "variable"
*frameSegmentScrollWindow.scrollBarDisplayPolicy: "static"
*frameSegmentScrollWindow.shadowThickness: 0
*frameSegmentScrollWindow.height: 200
*frameSegmentScrollWindow.width: 60

*frameSegmentScrollList.class: scrolledList
*frameSegmentScrollList.name.source: public
*frameSegmentScrollList.static: false
*frameSegmentScrollList.name: frameSegmentScrollList
*frameSegmentScrollList.parent: frameSegmentScrollWindow
*frameSegmentScrollList.width: 60
*frameSegmentScrollList.height: 200
*frameSegmentScrollList.shadowThickness: 3
*frameSegmentScrollList.visibleItemCount: 10
*frameSegmentScrollList.listSizePolicy: "constant"
*frameSegmentScrollList.stringDirection: "string_direction_r_to_l"
*frameSegmentScrollList.browseSelectionCallback: pickFrameListEntry(segmentList);

*centerLogDegLbl.class: labelGadget
*centerLogDegLbl.static: true
*centerLogDegLbl.name: centerLogDegLbl
*centerLogDegLbl.parent: scanQcBulletinBoard
*centerLogDegLbl.x: 339
*centerLogDegLbl.y: 503
*centerLogDegLbl.width: 100
*centerLogDegLbl.height: 30
*centerLogDegLbl.labelString: "(deg)"

*centerLatDegLbl.class: labelGadget
*centerLatDegLbl.static: true
*centerLatDegLbl.name: centerLatDegLbl
*centerLatDegLbl.parent: scanQcBulletinBoard
*centerLatDegLbl.x: 219
*centerLatDegLbl.y: 503
*centerLatDegLbl.width: 100
*centerLatDegLbl.height: 30
*centerLatDegLbl.labelString: "(deg)"

*frameNbrLbl3.class: labelGadget
*frameNbrLbl3.static: true
*frameNbrLbl3.name: frameNbrLbl3
*frameNbrLbl3.parent: scanQcBulletinBoard
*frameNbrLbl3.x: 119
*frameNbrLbl3.y: 483
*frameNbrLbl3.width: 100
*frameNbrLbl3.height: 30
*frameNbrLbl3.labelString: "Frame"

*centerLogLbl.class: labelGadget
*centerLogLbl.static: true
*centerLogLbl.name: centerLogLbl
*centerLogLbl.parent: scanQcBulletinBoard
*centerLogLbl.x: 329
*centerLogLbl.y: 483
*centerLogLbl.width: 120
*centerLogLbl.height: 30
*centerLogLbl.labelString: "Center Longitude"

*centerLatLbl.class: labelGadget
*centerLatLbl.static: true
*centerLatLbl.name: centerLatLbl
*centerLatLbl.parent: scanQcBulletinBoard
*centerLatLbl.x: 209
*centerLatLbl.y: 483
*centerLatLbl.width: 120
*centerLatLbl.height: 30
*centerLatLbl.labelString: "Center Latitude"

*centerTimeGMTLbl.class: labelGadget
*centerTimeGMTLbl.static: true
*centerTimeGMTLbl.name: centerTimeGMTLbl
*centerTimeGMTLbl.parent: scanQcBulletinBoard
*centerTimeGMTLbl.x: 489
*centerTimeGMTLbl.y: 503
*centerTimeGMTLbl.width: 100
*centerTimeGMTLbl.height: 30
*centerTimeGMTLbl.labelString: "(GMT)"

*backSelectFrame.class: pushButtonGadget
*backSelectFrame.name.source: public
*backSelectFrame.static: false
*backSelectFrame.name: backSelectFrame
*backSelectFrame.parent: scanQcBulletinBoard
*backSelectFrame.x: 490
*backSelectFrame.y: 770
*backSelectFrame.width: 100
*backSelectFrame.height: 50
*backSelectFrame.labelString: "Reject"
*backSelectFrame.activateCallback: {\
reject_cb();\
}

*cancelSelectFrame.class: pushButtonGadget
*cancelSelectFrame.static: true
*cancelSelectFrame.name: cancelSelectFrame
*cancelSelectFrame.parent: scanQcBulletinBoard
*cancelSelectFrame.x: 290
*cancelSelectFrame.y: 770
*cancelSelectFrame.width: 100
*cancelSelectFrame.height: 50
*cancelSelectFrame.labelString: "Hold"
*cancelSelectFrame.activateCallback: {\
hold_cb();\
}

*commandSeparator.class: separatorGadget
*commandSeparator.static: true
*commandSeparator.name: commandSeparator
*commandSeparator.parent: scanQcBulletinBoard
*commandSeparator.x: 10
*commandSeparator.y: 747
*commandSeparator.width: 680
*commandSeparator.height: 10

*frameTableLbl.class: labelGadget
*frameTableLbl.static: true
*frameTableLbl.name: frameTableLbl
*frameTableLbl.parent: scanQcBulletinBoard
*frameTableLbl.x: 239
*frameTableLbl.y: 443
*frameTableLbl.width: 190
*frameTableLbl.height: 30
*frameTableLbl.labelString: "Frame Table"
*frameTableLbl.fontList: "-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1"
*frameTableLbl.alignment: "alignment_center"

*segmentSegmentScrollWindow.class: scrolledWindow
*segmentSegmentScrollWindow.static: true
*segmentSegmentScrollWindow.name: segmentSegmentScrollWindow
*segmentSegmentScrollWindow.parent: scanQcBulletinBoard
*segmentSegmentScrollWindow.scrollingPolicy: "application_defined"
*segmentSegmentScrollWindow.x: 58
*segmentSegmentScrollWindow.y: 294
*segmentSegmentScrollWindow.width: 60
*segmentSegmentScrollWindow.height: 110
*segmentSegmentScrollWindow.shadowThickness: 0
*segmentSegmentScrollWindow.visualPolicy: "variable"
*segmentSegmentScrollWindow.stringDirection: "string_direction_r_to_l"
*segmentSegmentScrollWindow.sensitive: "true"
*segmentSegmentScrollWindow.scrollBarDisplayPolicy: "static"

*segmentSegmentScrollList.class: scrolledList
*segmentSegmentScrollList.name.source: public
*segmentSegmentScrollList.static: false
*segmentSegmentScrollList.name: segmentSegmentScrollList
*segmentSegmentScrollList.parent: segmentSegmentScrollWindow
*segmentSegmentScrollList.width: 60
*segmentSegmentScrollList.height: 110
*segmentSegmentScrollList.shadowThickness: 3
*segmentSegmentScrollList.visibleItemCount: 5
*segmentSegmentScrollList.listSizePolicy: "constant"
*segmentSegmentScrollList.browseSelectionCallback: pickSegmentListEntry(segmentList);
*segmentSegmentScrollList.selectionPolicy: "browse_select"
*segmentSegmentScrollList.sensitive: "true"

*segmentStartTimeScrollWindow.class: scrolledWindow
*segmentStartTimeScrollWindow.static: true
*segmentStartTimeScrollWindow.name: segmentStartTimeScrollWindow
*segmentStartTimeScrollWindow.parent: scanQcBulletinBoard
*segmentStartTimeScrollWindow.scrollingPolicy: "application_defined"
*segmentStartTimeScrollWindow.x: 298
*segmentStartTimeScrollWindow.y: 294
*segmentStartTimeScrollWindow.width: 150
*segmentStartTimeScrollWindow.height: 110
*segmentStartTimeScrollWindow.visualPolicy: "variable"
*segmentStartTimeScrollWindow.shadowThickness: 0

*segmentStartTimeScrollList.class: scrolledList
*segmentStartTimeScrollList.name.source: public
*segmentStartTimeScrollList.static: false
*segmentStartTimeScrollList.name: segmentStartTimeScrollList
*segmentStartTimeScrollList.parent: segmentStartTimeScrollWindow
*segmentStartTimeScrollList.width: 180
*segmentStartTimeScrollList.height: 110
*segmentStartTimeScrollList.shadowThickness: 3
*segmentStartTimeScrollList.visibleItemCount: 5
*segmentStartTimeScrollList.stringDirection: "string_direction_r_to_l"
*segmentStartTimeScrollList.listSizePolicy: "constant"
*segmentStartTimeScrollList.browseSelectionCallback: pickSegmentListEntry(longitudeList);

*segmentDurationScrollWindow.class: scrolledWindow
*segmentDurationScrollWindow.static: true
*segmentDurationScrollWindow.name: segmentDurationScrollWindow
*segmentDurationScrollWindow.parent: scanQcBulletinBoard
*segmentDurationScrollWindow.scrollingPolicy: "application_defined"
*segmentDurationScrollWindow.x: 138
*segmentDurationScrollWindow.y: 294
*segmentDurationScrollWindow.width: 60
*segmentDurationScrollWindow.height: 110
*segmentDurationScrollWindow.scrollBarDisplayPolicy: "static"
*segmentDurationScrollWindow.shadowThickness: 0
*segmentDurationScrollWindow.visualPolicy: "variable"

*segmentDurationScrollList.class: scrolledList
*segmentDurationScrollList.name.source: public
*segmentDurationScrollList.static: false
*segmentDurationScrollList.name: segmentDurationScrollList
*segmentDurationScrollList.parent: segmentDurationScrollWindow
*segmentDurationScrollList.width: 60
*segmentDurationScrollList.height: 110
*segmentDurationScrollList.shadowThickness: 3
*segmentDurationScrollList.visibleItemCount: 5
*segmentDurationScrollList.stringDirection: "string_direction_r_to_l"
*segmentDurationScrollList.listSizePolicy: "constant"
*segmentDurationScrollList.browseSelectionCallback: pickSegmentListEntry(latitudeList);

*segmentStopTimeScrollWindow.class: scrolledWindow
*segmentStopTimeScrollWindow.static: true
*segmentStopTimeScrollWindow.name: segmentStopTimeScrollWindow
*segmentStopTimeScrollWindow.parent: scanQcBulletinBoard
*segmentStopTimeScrollWindow.scrollingPolicy: "application_defined"
*segmentStopTimeScrollWindow.x: 468
*segmentStopTimeScrollWindow.y: 294
*segmentStopTimeScrollWindow.width: 150
*segmentStopTimeScrollWindow.height: 110
*segmentStopTimeScrollWindow.visualPolicy: "variable"
*segmentStopTimeScrollWindow.shadowThickness: 0

*segmentStopTimeScrollList.class: scrolledList
*segmentStopTimeScrollList.name.source: public
*segmentStopTimeScrollList.static: false
*segmentStopTimeScrollList.name: segmentStopTimeScrollList
*segmentStopTimeScrollList.parent: segmentStopTimeScrollWindow
*segmentStopTimeScrollList.width: 180
*segmentStopTimeScrollList.height: 110
*segmentStopTimeScrollList.shadowThickness: 3
*segmentStopTimeScrollList.visibleItemCount: 5
*segmentStopTimeScrollList.stringDirection: "string_direction_r_to_l"
*segmentStopTimeScrollList.listSizePolicy: "constant"
*segmentStopTimeScrollList.browseSelectionCallback: pickSegmentListEntry(timeList);

*segmentScrollBar.class: scrollBar
*segmentScrollBar.name.source: public
*segmentScrollBar.static: false
*segmentScrollBar.name: segmentScrollBar
*segmentScrollBar.parent: scanQcBulletinBoard
*segmentScrollBar.x: 638
*segmentScrollBar.y: 294
*segmentScrollBar.width: 20
*segmentScrollBar.height: 102
*segmentScrollBar.shadowThickness: 3
*segmentScrollBar.valueChangedCallback: moveSegmentScrollBar();
*segmentScrollBar.sliderSize: 10
*segmentScrollBar.maximum: 50
*segmentScrollBar.pageIncrement: 5

*segmnetLbl2.class: labelGadget
*segmnetLbl2.static: true
*segmnetLbl2.name: segmnetLbl2
*segmnetLbl2.parent: scanQcBulletinBoard
*segmnetLbl2.x: 38
*segmnetLbl2.y: 264
*segmnetLbl2.width: 100
*segmnetLbl2.height: 30
*segmnetLbl2.labelString: "Number"

*frameNbrLbl2.class: labelGadget
*frameNbrLbl2.static: true
*frameNbrLbl2.name: frameNbrLbl2
*frameNbrLbl2.parent: scanQcBulletinBoard
*frameNbrLbl2.x: 198
*frameNbrLbl2.y: 264
*frameNbrLbl2.width: 100
*frameNbrLbl2.height: 30
*frameNbrLbl2.labelString: "Frames"

*durationSecLbl.class: labelGadget
*durationSecLbl.static: true
*durationSecLbl.name: durationSecLbl
*durationSecLbl.parent: scanQcBulletinBoard
*durationSecLbl.x: 113
*durationSecLbl.y: 264
*durationSecLbl.width: 100
*durationSecLbl.height: 30
*durationSecLbl.labelString: "(sec)"

*frameNbrLbl1.class: labelGadget
*frameNbrLbl1.static: true
*frameNbrLbl1.name: frameNbrLbl1
*frameNbrLbl1.parent: scanQcBulletinBoard
*frameNbrLbl1.x: 198
*frameNbrLbl1.y: 244
*frameNbrLbl1.width: 100
*frameNbrLbl1.height: 30
*frameNbrLbl1.labelString: "# of"

*segmentLbl1.class: labelGadget
*segmentLbl1.static: true
*segmentLbl1.name: segmentLbl1
*segmentLbl1.parent: scanQcBulletinBoard
*segmentLbl1.x: 38
*segmentLbl1.y: 244
*segmentLbl1.width: 100
*segmentLbl1.height: 30
*segmentLbl1.labelString: "Segment"

*durationLbl.class: labelGadget
*durationLbl.static: true
*durationLbl.name: durationLbl
*durationLbl.parent: scanQcBulletinBoard
*durationLbl.x: 118
*durationLbl.y: 244
*durationLbl.width: 100
*durationLbl.height: 30
*durationLbl.labelString: "Duration"

*startTimeGMTLbl.class: labelGadget
*startTimeGMTLbl.static: true
*startTimeGMTLbl.name: startTimeGMTLbl
*startTimeGMTLbl.parent: scanQcBulletinBoard
*startTimeGMTLbl.x: 323
*startTimeGMTLbl.y: 264
*startTimeGMTLbl.width: 100
*startTimeGMTLbl.height: 30
*startTimeGMTLbl.labelString: "(GMT)"

*stopTimeLbl.class: labelGadget
*stopTimeLbl.static: true
*stopTimeLbl.name: stopTimeLbl
*stopTimeLbl.parent: scanQcBulletinBoard
*stopTimeLbl.x: 498
*stopTimeLbl.y: 244
*stopTimeLbl.width: 100
*stopTimeLbl.height: 30
*stopTimeLbl.labelString: "Stop Time"

*startTimeLbl.class: labelGadget
*startTimeLbl.static: true
*startTimeLbl.name: startTimeLbl
*startTimeLbl.parent: scanQcBulletinBoard
*startTimeLbl.x: 323
*startTimeLbl.y: 244
*startTimeLbl.width: 100
*startTimeLbl.height: 30
*startTimeLbl.labelString: "Start Time"

*stopTimeGMTLbl.class: labelGadget
*stopTimeGMTLbl.static: true
*stopTimeGMTLbl.name: stopTimeGMTLbl
*stopTimeGMTLbl.parent: scanQcBulletinBoard
*stopTimeGMTLbl.x: 498
*stopTimeGMTLbl.y: 264
*stopTimeGMTLbl.width: 100
*stopTimeGMTLbl.height: 30
*stopTimeGMTLbl.labelString: "(GMT)"

*segmentTableLbl.class: labelGadget
*segmentTableLbl.static: true
*segmentTableLbl.name: segmentTableLbl
*segmentTableLbl.parent: scanQcBulletinBoard
*segmentTableLbl.x: 248
*segmentTableLbl.y: 204
*segmentTableLbl.width: 190
*segmentTableLbl.height: 30
*segmentTableLbl.labelString: "Segment Table"
*segmentTableLbl.fontList: "-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1"
*segmentTableLbl.alignment: "alignment_center"

*segmentLbl3.class: labelGadget
*segmentLbl3.static: true
*segmentLbl3.name: segmentLbl3
*segmentLbl3.parent: scanQcBulletinBoard
*segmentLbl3.x: 39
*segmentLbl3.y: 483
*segmentLbl3.width: 100
*segmentLbl3.height: 30
*segmentLbl3.labelString: "Segment"

*segmnetLbl4.class: labelGadget
*segmnetLbl4.static: true
*segmnetLbl4.name: segmnetLbl4
*segmnetLbl4.parent: scanQcBulletinBoard
*segmnetLbl4.x: 39
*segmnetLbl4.y: 503
*segmnetLbl4.width: 100
*segmnetLbl4.height: 30
*segmnetLbl4.labelString: "Number"

*frameNbrLbl4.class: labelGadget
*frameNbrLbl4.static: true
*frameNbrLbl4.name: frameNbrLbl4
*frameNbrLbl4.parent: scanQcBulletinBoard
*frameNbrLbl4.x: 119
*frameNbrLbl4.y: 503
*frameNbrLbl4.width: 100
*frameNbrLbl4.height: 30
*frameNbrLbl4.labelString: "Number"

*totalDatatakeLbl.class: labelGadget
*totalDatatakeLbl.static: true
*totalDatatakeLbl.name: totalDatatakeLbl
*totalDatatakeLbl.parent: scanQcBulletinBoard
*totalDatatakeLbl.x: 60
*totalDatatakeLbl.y: 147
*totalDatatakeLbl.width: 200
*totalDatatakeLbl.height: 30
*totalDatatakeLbl.alignment: "alignment_beginning"
*totalDatatakeLbl.labelString: "Total Datatake Length (sec):"

*totalDatatakeData.class: labelGadget
*totalDatatakeData.name.source: public
*totalDatatakeData.static: false
*totalDatatakeData.name: totalDatatakeData
*totalDatatakeData.parent: scanQcBulletinBoard
*totalDatatakeData.x: 260
*totalDatatakeData.y: 147
*totalDatatakeData.width: 60
*totalDatatakeData.height: 30
*totalDatatakeData.alignment: "alignment_beginning"
*totalDatatakeData.labelString: ""
*totalDatatakeData.marginHeight: 8

*nominalFrameLbl.class: labelGadget
*nominalFrameLbl.static: true
*nominalFrameLbl.name: nominalFrameLbl
*nominalFrameLbl.parent: scanQcBulletinBoard
*nominalFrameLbl.x: 360
*nominalFrameLbl.y: 122
*nominalFrameLbl.width: 220
*nominalFrameLbl.height: 30
*nominalFrameLbl.alignment: "alignment_beginning"
*nominalFrameLbl.labelString: "Nominal Frame Length (sec):"

*nominalFrameData.class: labelGadget
*nominalFrameData.name.source: public
*nominalFrameData.static: false
*nominalFrameData.name: nominalFrameData
*nominalFrameData.parent: scanQcBulletinBoard
*nominalFrameData.x: 580
*nominalFrameData.y: 122
*nominalFrameData.width: 60
*nominalFrameData.height: 30
*nominalFrameData.alignment: "alignment_beginning"
*nominalFrameData.labelString: ""
*nominalFrameData.marginHeight: 8

*segmentFrameScrollWindow.class: scrolledWindow
*segmentFrameScrollWindow.static: true
*segmentFrameScrollWindow.name: segmentFrameScrollWindow
*segmentFrameScrollWindow.parent: scanQcBulletinBoard
*segmentFrameScrollWindow.scrollingPolicy: "application_defined"
*segmentFrameScrollWindow.x: 218
*segmentFrameScrollWindow.y: 294
*segmentFrameScrollWindow.width: 60
*segmentFrameScrollWindow.height: 110
*segmentFrameScrollWindow.scrollBarDisplayPolicy: "static"
*segmentFrameScrollWindow.shadowThickness: 0
*segmentFrameScrollWindow.visualPolicy: "variable"

*segmentFrameScrollList.class: scrolledList
*segmentFrameScrollList.name.source: public
*segmentFrameScrollList.static: false
*segmentFrameScrollList.name: segmentFrameScrollList
*segmentFrameScrollList.parent: segmentFrameScrollWindow
*segmentFrameScrollList.width: 60
*segmentFrameScrollList.height: 110
*segmentFrameScrollList.shadowThickness: 3
*segmentFrameScrollList.visibleItemCount: 5
*segmentFrameScrollList.stringDirection: "string_direction_r_to_l"
*segmentFrameScrollList.listSizePolicy: "constant"
*segmentFrameScrollList.browseSelectionCallback: pickSegmentListEntry(frameList);

*segemntSeparator1.class: separatorGadget
*segemntSeparator1.static: true
*segemntSeparator1.name: segemntSeparator1
*segemntSeparator1.parent: scanQcBulletinBoard
*segemntSeparator1.x: 10
*segemntSeparator1.y: 184
*segemntSeparator1.width: 680
*segemntSeparator1.height: 10

*nominalSegmentLbl.class: labelGadget
*nominalSegmentLbl.static: true
*nominalSegmentLbl.name: nominalSegmentLbl
*nominalSegmentLbl.parent: scanQcBulletinBoard
*nominalSegmentLbl.x: 360
*nominalSegmentLbl.y: 147
*nominalSegmentLbl.width: 220
*nominalSegmentLbl.height: 30
*nominalSegmentLbl.alignment: "alignment_beginning"
*nominalSegmentLbl.labelString: "Nominal Segment Length (sec):"

*nominalSegmentData.class: labelGadget
*nominalSegmentData.name.source: public
*nominalSegmentData.static: false
*nominalSegmentData.name: nominalSegmentData
*nominalSegmentData.parent: scanQcBulletinBoard
*nominalSegmentData.x: 580
*nominalSegmentData.y: 147
*nominalSegmentData.width: 60
*nominalSegmentData.height: 30
*nominalSegmentData.alignment: "alignment_beginning"
*nominalSegmentData.labelString: ""
*nominalSegmentData.marginHeight: 8

*jobIDLbl.class: labelGadget
*jobIDLbl.static: true
*jobIDLbl.name: jobIDLbl
*jobIDLbl.parent: scanQcBulletinBoard
*jobIDLbl.x: 500
*jobIDLbl.y: 10
*jobIDLbl.width: 80
*jobIDLbl.height: 30
*jobIDLbl.alignment: "alignment_beginning"
*jobIDLbl.labelString: "Job ID:"

*jobIDData.class: labelGadget
*jobIDData.name.source: public
*jobIDData.static: false
*jobIDData.name: jobIDData
*jobIDData.parent: scanQcBulletinBoard
*jobIDData.x: 580
*jobIDData.y: 10
*jobIDData.width: 80
*jobIDData.height: 30
*jobIDData.labelString: ""
*jobIDData.alignment: "alignment_beginning"
*jobIDData.marginHeight: 8

*frameSizeKM.class: labelGadget
*frameSizeKM.static: true
*frameSizeKM.name: frameSizeKM
*frameSizeKM.x: 200
*frameSizeKM.y: 70
*frameSizeKM.width: 40
*frameSizeKM.height: 30
*frameSizeKM.alignment: "alignment_beginning"
*frameSizeKM.labelString: "(km)"
*frameSizeKM.parent: scanQcBulletinBoard

*frameSizeLbl.class: labelGadget
*frameSizeLbl.static: true
*frameSizeLbl.name: frameSizeLbl
*frameSizeLbl.x: 60
*frameSizeLbl.y: 70
*frameSizeLbl.width: 90
*frameSizeLbl.height: 30
*frameSizeLbl.alignment: "alignment_beginning"
*frameSizeLbl.labelString: "Frame Size:"
*frameSizeLbl.parent: scanQcBulletinBoard

