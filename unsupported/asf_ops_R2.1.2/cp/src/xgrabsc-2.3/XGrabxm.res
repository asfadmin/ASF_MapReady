#include "config.h"

XGrab*.title.labelString: X-Windows Screen Grabber
XGrab*.title.FontList:   *-times-medium-r-normal--18-*
XGrab*.title.x: 135

!----------------------------box 1

XGrab*.box1.x: 0
XGrab*.box1.y:  35

XGrab*.inputLbl.x:     5
XGrab*.inputLbl.y:      0
XGrab*.inputLbl.FontList: *-helvetica-medium-o-normal--14-*
XGrab*.inputLbl.labelString: Input Options--------------------------------------------

XGrab*.selectiontype.topAttachment: attach_widget
XGrab*.selectiontype.topWidget: inputLbl
XGrab*.selectiontype.topOffset: 2
XGrab*.selectiontype.orientation: horizontal
XGrab*.selectiontype.numColumns: 2
XGrab*.selectiontype.x: 5

XGrab*.click.labelString: Click on Window
XGrab*.stretch.labelString: Stretch Rectangle
XGrab*.stretch.set: 1
XGrab*.key.labelString: Wait for Control key
XGrab*.root.labelString: Grab Whole Screen

XGrab*.sleeplbl.x: 10
XGrab*.sleeplbl.y:  90
XGrab*.sleeplbl.labelString: Sleep before starting

XGrab*.sleeptime.leftAttachment: attach_widget
XGrab*.sleeptime.leftWidget:      sleeplbl
XGrab*.sleeptime.leftOffset:  5
XGrab*.sleeptime.y:      85
XGrab*.sleeptime.width:  40
XGrab*.sleeptime*value:  DEFAULT_PRESLEEP

XGrab*.psleeplbl.leftAttachment: attach_widget
XGrab*.psleeplbl.leftWidget:	sleeptime
XGrab*.psleeplbl.leftOffset:    10
XGrab*.psleeplbl.y:    90
XGrab*.psleeplbl.labelString: Sleep after selecting

XGrab*.psleeptime.leftAttachment: attach_widget
XGrab*.psleeptime.leftWidget:	psleeplbl
XGrab*.psleeptime.leftOffset:     5
XGrab*.psleeptime.y:     85
XGrab*.psleeptime.width:  40
XGrab*.psleeptime*value: DEFAULT_POSTSLEEP

XGrab*.host.x:   10
XGrab*.host.y:   120
XGrab*.host.labelString: Use Alternate Display:
XGrab*.host.width:  150
XGrab*.host.set: 0

XGrab*.hostText.leftAttachment: attach_widget
XGrab*.hostText.leftWidget:	host
XGrab*.hostText.leftOffset:     5
XGrab*.hostText.y:    115
! xgrab sets DISPLAY value into this widget
! XGrab*.hostText*value: :0


!----------------------------box 2
XGrab*.box2.x:   0
XGrab*.box2.topAttachment: attach_widget
XGrab*.box2.topWidget: box1

XGrab*.outputFormat.x:   5
XGrab*.outputFormat.y:    0
XGrab*.outputFormat.FontList: *-helvetica-medium-o-normal--12-*
XGrab*.outputFormat.labelString: Output Format-------------------------------------------------

XGrab*.formattype.topAttachment: attach_widget
XGrab*.formattype.topWidget: outputFormat
XGrab*.formattype.topOffset: 2
XGrab*.formattype.orientation: horizontal
XGrab*.formattype.numColumns: 2

XGrab*.xwd.labelString:    X Window Dump
XGrab*.ps.labelString:     PostScript
XGrab*.ps.set: 1
XGrab*.puzzle.labelString: Puzzle
XGrab*.xwdxy.labelString:  XWD-xy
XGrab*.xpm.labelString:    Bitmap/XPM
XGrab*.xpm2.labelString:   Bm/XPM2
XGrab*.xpm3.labelString:   Bm/XPM3

!----------------------------box 3
XGrab*.box3.x:   0
XGrab*.box3.topAttachment: attach_widget
XGrab*.box3.topWidget: box2

XGrab*.psOptions.x:   5
XGrab*.psOptions.y:    0
XGrab*.psOptions.FontList: *-helvetica-medium-o-normal--12-*
XGrab*.psOptions.labelString: PostScript Options----------------------------------------------

XGrab*.compress.x:   10
XGrab*.compress.y:    25 
XGrab*.compress.labelString: Compress
XGrab*.compress.set: 1

XGrab*.color.leftAttachment: attach_widget
XGrab*.color.leftWidget: compress
XGrab*.color.leftOffset:    10
XGrab*.color.y:     25
XGrab*.color.labelString: Color Output

XGrab*.binary.leftAttachment: attach_widget
XGrab*.binary.leftWidget: color
XGrab*.binary.leftOffset: 10
XGrab*.binary.y: 25
XGrab*.binary.labelString: Binary

XGrab*.limit.leftAttachment: attach_widget
XGrab*.limit.leftWidget: binary
XGrab*.limit.leftOffset: 10
XGrab*.limit.y: 25
#if defined(NO_PRINTER_MEMORY_CHECKS)
XGrab*.limit.set: 0
#else
XGrab*.limit.set: 1
#endif
XGrab*.limit.labelString: Limit Check

XGrab*.postscripttype.topAttachment: attach_widget
XGrab*.postscripttype.topWidget: compress
XGrab*.postscripttype.topOffset: 2
XGrab*.postscripttype.orientation: horizontal
XGrab*.postscripttype.numColumns: 1
XGrab*.postscripttype.x: 5

XGrab*.portrait.labelString: Portrait
XGrab*.portrait.set: true
XGrab*.landscape.labelString: Landscape
XGrab*.epsi.labelString: PS Figure
XGrab*.onlyEpsi.labelString: Preview Only
XGrab*.pageWidth.x: 10
XGrab*.pageWidth.y:  90
XGrab*.pageWidth.labelString: Paper Width

XGrab*.pageWidthText.leftAttachment: attach_widget
XGrab*.pageWidthText.leftWidget: pageWidth
XGrab*.pageWidthText.leftOffset: 0
XGrab*.pageWidthText.y:  85
XGrab*.pageWidthText.width: 45
XGrab*.pageWidthText*value: PAPER_WIDTH

XGrab*.pageHeight.leftAttachment: attach_widget
XGrab*.pageHeight.leftWidget:	pageWidthText
XGrab*.pageHeight.leftOffset: 2
XGrab*.pageHeight.y:  90
XGrab*.pageHeight.labelString: Height

XGrab*.pageHeightText.leftAttachment: attach_widget
XGrab*.pageHeightText.leftWidget: pageHeight
XGrab*.pageHeightText.leftOffset:  0
XGrab*.pageHeightText.y:  85
XGrab*.pageHeightText.width: 45
XGrab*.pageHeightText*value: PAPER_HEIGHT

XGrab*.horizMargin.leftAttachment: attach_widget
XGrab*.horizMargin.leftWidget: pageHeightText
XGrab*.horizMargin.leftOffset:  2
XGrab*.horizMargin.y:  90
XGrab*.horizMargin.labelString: Margin Width

XGrab*.horizMarginText.leftAttachment: attach_widget
XGrab*.horizMarginText.leftWidget: horizMargin
XGrab*.horizMarginText.leftOffset:  0
XGrab*.horizMarginText.y:  85
XGrab*.horizMarginText.width: 45
XGrab*.horizMarginText*value: HORIZ_MARGIN

XGrab*.vertMargin.leftAttachment: attach_widget
XGrab*.vertMargin.leftWidget: horizMarginText
XGrab*.vertMargin.leftOffset:  2
XGrab*.vertMargin.y:  90
XGrab*.vertMargin.labelString:  Height

XGrab*.vertMarginText.leftAttachment: attach_widget
XGrab*.vertMarginText.leftWidget: vertMargin
XGrab*.vertMarginText.leftOffset:  0
XGrab*.vertMarginText.y:  85
XGrab*.vertMarginText.width: 45
XGrab*.vertMarginText*value: VERT_MARGIN

!----------------------------box 4
XGrab*.box4.x: 0
XGrab*.box4.topAttachment: attach_widget
XGrab*.box4.topWidget: box3

XGrab*.prOptions.x:   5
XGrab*.prOptions.y:    0
XGrab*.prOptions.FontList: *-helvetica-medium-o-normal--12-*
XGrab*.prOptions.labelString: Image Processing Options-----

XGrab*.borders.x:   10
XGrab*.borders.y:    20
XGrab*.borders.labelString: Include Borders
XGrab*.borders.width:          105
XGrab*.borders.set:            1

XGrab*.reverse.x:   10
XGrab*.reverse.y:    45
XGrab*.reverse.labelString: Reverse Colors
XGrab*.reverse.width:           105

XGrab*.brightnessLbl.x:   10
XGrab*.brightnessLbl.y:    75
XGrab*.brightnessLbl.labelString: Brightness

XGrab*.brightnessText.x:   85
XGrab*.brightnessText.y:    70
XGrab*.brightnessText.width: 40
XGrab*.brightnessText*value: 100

!----------------------------box 5
XGrab*.box5.leftAttachment: attach_widget
XGrab*.box5.leftWidget: box4
XGrab*.box5.leftOffset: 5
XGrab*.box5.topAttachment: attach_widget
XGrab*.box5.topWidget: box3

XGrab*.outputLbl.x:     5
XGrab*.outputLbl.y:      0
XGrab*.outputLbl.FontList: *-helvetica-medium-o-normal--12-*
XGrab*.outputLbl.labelString: Output Options------------------------

XGrab*.outputtype.topAttachment: attach_widget
XGrab*.outputtype.topWidget: outputLbl
XGrab*.outputtype.topOffset: 2
XGrab*.outputtype.orientation: vertical
XGrab*.outputtype.numColumns: 1

XGrab*.file.labelString: To File:
XGrab*.file.set: 1
XGrab*.file.marginHeight: 2
XGrab*.printer.labelString: To Command:
XGrab*.printer.marginHeight: 4

XGrab*.fileText.leftAttachment: attach_widget
XGrab*.fileText.leftWidget: outputtype
XGrab*.fileText.leftOffset: 5
XGrab*.fileText.topAttachment: attach_widget
XGrab*.fileText.topWidget: outputLbl
XGrab*.fileText.topOffset: 2
XGrab*.fileText.width: 110
XGrab*.fileText*value: DEFAULT_FILENAME

XGrab*.printerText.leftAttachment: attach_widget
XGrab*.printerText.leftWidget: outputtype
XGrab*.printerText.leftOffset: 5
XGrab*.printerText.topAttachment: attach_widget
XGrab*.printerText.topWidget: fileText
XGrab*.printerText.topOffset: 2
XGrab*.printerText.width: 110
XGrab*.printerText*value: DEFAULT_COMMAND



XGrab*.converttype.topAttachment: attach_widget
XGrab*.converttype.topWidget: box5
XGrab*.converttype.topOffset: 15
XGrab*.converttype.orientation: horizontal
XGrab*.converttype.numColumns: 2
XGrab*.converttype.x: 5

XGrab*.nodither.labelString: No Dither
XGrab*.nodither.set: true
XGrab*.dither.labelString: Dither
XGrab*.halftone.labelString: Halftone
XGrab*.mapbw.labelString: Map to B/W
XGrab*.mdither.labelString: M-Dither

!----------------------------box 6
XGrab*.box6.x:   0
XGrab*.box6.topAttachment: attach_widget
XGrab*.box6.topWidget: converttype


XGrab*.splat.labelString: ------------------------------------------------------------

XGrab*.OK.x:   110
XGrab*.OK.y:     25
XGrab*.OK.width:            80
XGrab*.OK.FontList: *-helvetica-bold-r-normal--14-*
XGrab*.OK.labelString: OK

XGrab*.Dismiss.x:   265
XGrab*.Dismiss.y:     25
XGrab*.Dismiss.width:            80
XGrab*.Dismiss.FontList: *-helvetica-bold-r-normal--14-*
XGrab*.Dismiss.labelString: Dismiss


!----------------------------- pervasive resources
XGrab*FontList: *-helvetica-medium-r-normal--12-*
XGrab*XmText.FontList: *-helvetica-medium-r-normal--12-*
XGrab*XmToggleButton.FontList: *-helvetica-medium-r-normal--12-*
XGrab*XmLabel.FontList: *-helvetica-medium-r-normal--12-*
XGrab*XmLabel.borderWidth: 0
XGrab*XmToggleButton.borderWidth: 0
XGrab*XmForm.borderWidth: 0
XGrab*XmPushButton.shadowThickness: 3

XGrab*XmToggleButton.marginHeight: 0
XGrab*XmRowColumn*adjustMargin: false

XGrab*XmForm.background: #a3d1f2
XGrab*XmRowColumn.background: #a3d1f2
XGrab*XmLabel.background: #a3d1f2
XGrab*XmText*background: ivory
XGrab*XmToggleButton.background: #a3d1f2
!XGrab*XmPushButton.background: #d3f1ff

XGrab*selstyle.foreground: navy
XGrab*outputXmFormat.foreground: navy
XGrab*psOptions.foreground: navy
XGrab*prOptions.foreground: navy
XGrab*outputLbl.foreground: navy
XGrab*foreground: navy
XGrab*background: #d3f1ff

XGrab*selectColor: navy
XGrab*highlightColor: navy
XGrab*highlightThickness: 2

! ========================================================================
! *
! * Name - XGrabxm.ad
! *
! * Version:	1.2
! *
! * ccsid:	@(#)XGrabxm.res	1.2 -06/28/93 09:13:40
! * from: 	ccs/s.XGrabxm.res
! * date: 	06/28/93 09:14:48
! *
! * Copyright (c) 1991-3 Bruce Schuchardt.
! * Read the file cpyright.h for full copyright inXmFormation.
! *
! *
! * Description: Application Defaults file for xgrabxm
! *
! *========================================================================

