#include "config.h"

XGrab*.title.label: X-Windows Screen Grabber
XGrab*.title.font:   *-times-medium-r-normal--18-*
XGrab*.title.horizDistance: 105

!----------------------------box 1

XGrab*.box1.horizDistance: 0
XGrab*.box1.vertDistance:  35

XGrab*.inputLbl.horizDistance:     5
XGrab*.inputLbl.vertDistance:      0
XGrab*.inputLbl.font: *-helvetica-medium-o-normal--14-*
XGrab*.inputLbl.label: Input Options-------------------------------------

XGrab*.click.horizDistance:   10
XGrab*.click.vertDistance:    25
XGrab*.click.label: Click on Window

XGrab*.stretch.horizDistance:   135
XGrab*.stretch.vertDistance:     25
XGrab*.stretch.label: Stretch Rectangle
XGrab*.stretch.state: 1

XGrab*.key.horizDistance:   265
XGrab*.key.vertDistance:     25
XGrab*.key.label: Wait for Control key

XGrab*.root.horizDistance: 10
XGrab*.root.vertDistance:  55
XGrab*.root.label: Grab Whole Screen

XGrab*.sleeplbl.horizDistance: 10
XGrab*.sleeplbl.vertDistance:  85
XGrab*.sleeplbl.label: Sleep before starting

XGrab*.sleeptime.fromHoriz:      sleeplbl
XGrab*.sleeptime.horizDistance:     5
XGrab*.sleeptime.vertDistance:     85
XGrab*.sleeptime.width:            30
XGrab*.sleeptime*string: DEFAULT_PRESLEEP

XGrab*.psleeplbl.fromHoriz:	sleeptime
XGrab*.psleeplbl.horizDistance:   10
XGrab*.psleeplbl.vertDistance:    85
XGrab*.psleeplbl.label: Sleep after selecting

XGrab*.psleeptime.fromHoriz:	psleeplbl
XGrab*.psleeptime.horizDistance:     5
XGrab*.psleeptime.vertDistance:     85
XGrab*.psleeptime.width:            30
XGrab*.psleeptime*string: DEFAULT_POSTSLEEP

XGrab*.host.horizDistance:   10
XGrab*.host.vertDistance:   115
XGrab*.host.label: Use Alternate Display:
XGrab*.host.shapeStyle: oval
XGrab*.host.width:	    140
XGrab*.host.state: 0

XGrab*.hostText.fromHoriz:	host
XGrab*.hostText.horizDistance:     5
XGrab*.hostText.vertDistance:    115
! xgrab sets DISPLAY value into this widget
! XGrab*.hostText*string: :0


!----------------------------box 2
XGrab*.box2.horizDistance:   0
XGrab*.box2.vertDistance:  180

XGrab*.outputFormat.horizDistance:   5
XGrab*.outputFormat.vertDistance:    0
XGrab*.outputFormat.font: *-helvetica-medium-o-normal--12-*
XGrab*.outputFormat.label: Output Format------------------------------------------

XGrab*.ps.horizDistance:    10
XGrab*.ps.vertDistance:     25
XGrab*.ps.label: PostScript
XGrab*.ps.state: 1

XGrab*.puzzle.horizDistance:    87
XGrab*.puzzle.vertDistance:     25
XGrab*.puzzle.label: Puzzle

XGrab*.xwd.horizDistance:   142
XGrab*.xwd.vertDistance:    25
XGrab*.xwd.label: X Window Dump

XGrab*.xyxwd.horizDistance:    252
XGrab*.xyxwd.vertDistance:     25
XGrab*.xyxwd.label: XWD/xy

XGrab*.xpm.horizDistance:    10
XGrab*.xpm.vertDistance:     55
XGrab*.xpm.label: Bitmap/XPM

XGrab*.xpm2.horizDistance:    95
XGrab*.xpm2.vertDistance:     55
XGrab*.xpm2.label: Bm/XPM2

XGrab*.xpm3.horizDistance:   165
XGrab*.xpm3.vertDistance:     55
XGrab*.xpm3.label: Bm/XPM3

!----------------------------box 3
XGrab*.box3.horizDistance:   0
XGrab*.box3.vertDistance:  270

XGrab*.psOptions.horizDistance:   5
XGrab*.psOptions.vertDistance:    0
XGrab*.psOptions.font: *-helvetica-medium-o-normal--12-*
XGrab*.psOptions.label: PostScript Options---------------------------------------

XGrab*.compress.horizDistance:   10
XGrab*.compress.vertDistance:    25 
XGrab*.compress.shapeStyle: oval
XGrab*.compress.label: Compress
XGrab*.compress.state: 1

XGrab*.color.fromHoriz: compress
XGrab*.color.horizDistance:    10
XGrab*.color.vertDistance:     25
XGrab*.color.shapeStyle: oval
XGrab*.color.label: Color Output

XGrab*.binary.fromHoriz: color
XGrab*.binary.horizDistance: 10
XGrab*.binary.vertDistance: 25
XGrab*.binary.shapeStyle: oval
XGrab*.binary.label: Binary

XGrab*.limit.fromHoriz: binary
XGrab*.limit.horizDistance: 10
XGrab*.limit.vertDistance: 25
XGrab*.limit.shapeStyle: oval
#if defined(NO_PRINTER_MEMORY_CHECKS)
XGrab*.limit.state: 0
#else
XGrab*.limit.state: 1
#endif
XGrab*.limit.label: Limit Check

XGrab*.landscape.horizDistance: 10
XGrab*.landscape.vertDistance: 55
XGrab*.landscape.width: 80
XGrab*.landscape.label: Landscape

XGrab*.epsi.fromHoriz: landscape
XGrab*.epsi.horizDistance:    10
XGrab*.epsi.vertDistance:     55
XGrab*.epsi.label: PS Figure

XGrab*.onlyEpsi.fromHoriz: epsi
XGrab*.onlyEpsi.horizDistance:    10
XGrab*.onlyEpsi.vertDistance:     55
XGrab*.onlyEpsi.label: Preview Only

XGrab*.pageWidth.horizDistance: 10
XGrab*.pageWidth.vertDistance:  85
XGrab*.pageWidth.label: Paper Width

XGrab*.pageWidthText.fromHoriz: pageWidth
XGrab*.pageWidthText.horizDistance: 0
XGrab*.pageWidthText.vertDistance:  85
XGrab*.pageWidthText.width: 35
XGrab*.pageWidthText*string: PAPER_WIDTH

XGrab*.pageHeight.fromHoriz:	pageWidthText
XGrab*.pageHeight.horizDistance: 2
XGrab*.pageHeight.vertDistance:  85
XGrab*.pageHeight.label: Height

XGrab*.pageHeightText.fromHoriz: pageHeight
XGrab*.pageHeightText.horizDistance:  0
XGrab*.pageHeightText.vertDistance:  85
XGrab*.pageHeightText.width: 35
XGrab*.pageHeightText*string: PAPER_HEIGHT

XGrab*.horizMargin.fromHoriz: pageHeightText
XGrab*.horizMargin.horizDistance:  2
XGrab*.horizMargin.vertDistance:  85
XGrab*.horizMargin.label: Margin Width

XGrab*.horizMarginText.fromHoriz: horizMargin
XGrab*.horizMarginText.horizDistance:  0
XGrab*.horizMarginText.vertDistance:  85
XGrab*.horizMarginText.width: 35
XGrab*.horizMarginText*string: HORIZ_MARGIN

XGrab*.vertMargin.fromHoriz: horizMarginText
XGrab*.vertMargin.horizDistance:  2
XGrab*.vertMargin.vertDistance:  85
XGrab*.vertMargin.label:  Height

XGrab*.vertMarginText.fromHoriz: vertMargin
XGrab*.vertMarginText.horizDistance:  0
XGrab*.vertMarginText.vertDistance:  85
XGrab*.vertMarginText.width: 35
XGrab*.vertMarginText*string: VERT_MARGIN

!----------------------------box 4
XGrab*.box4.horizDistance: 0
XGrab*.box4.vertDistance: 385

XGrab*.prOptions.horizDistance:   5
XGrab*.prOptions.vertDistance:    0
XGrab*.prOptions.font: *-helvetica-medium-o-normal--12-*
XGrab*.prOptions.label: Image Processing Options-----

XGrab*.borders.horizDistance:   10
XGrab*.borders.vertDistance:    25
XGrab*.borders.shapeStyle: oval
XGrab*.borders.label: Include Borders
XGrab*.borders.width:          105
XGrab*.borders.state:            1

XGrab*.reverse.horizDistance:   10
XGrab*.reverse.vertDistance:    55
XGrab*.reverse.shapeStyle: oval
XGrab*.reverse.label: Reverse Colors
XGrab*.reverse.width:           105

XGrab*.brightnessLbl.horizDistance:   10
XGrab*.brightnessLbl.vertDistance:    85
XGrab*.brightnessLbl.label: Brightness

XGrab*.brightnessText.horizDistance:   85
XGrab*.brightnessText.vertDistance:    85
XGrab*.brightnessText.width:           30
XGrab*.brightnessText*string: 100

XGrab*.dither.horizDistance:   10
XGrab*.dither.vertDistance:   115
XGrab*.dither.label: Dither

XGrab*.mdither.horizDistance:   62
XGrab*.mdither.vertDistance:   115
XGrab*.mdither.label: M-Dither

XGrab*.halftone.horizDistance:  132
XGrab*.halftone.vertDistance:   115
XGrab*.halftone.label: Halftone

XGrab*.mapbw.horizDistance:   10
XGrab*.mapbw.vertDistance:    140
XGrab*.mapbw.label: Map to B/W


!----------------------------box 5
XGrab*.box5.fromHoriz: box4
XGrab*.box5.horizDistance: 5
XGrab*.box5.vertDistance:  385

XGrab*.outputLbl.horizDistance:     5
XGrab*.outputLbl.vertDistance:      0
XGrab*.outputLbl.font: *-helvetica-medium-o-normal--12-*
XGrab*.outputLbl.label: Output Options--------------

XGrab*.file.horizDistance:   10
XGrab*.file.vertDistance:    25
XGrab*.file.label: To File:
XGrab*.file.state: 1

XGrab*.fileText.horizDistance:   100
XGrab*.fileText.vertDistance:    25
XGrab*.fileText.width: 100
XGrab*.fileText*string: DEFAULT_FILENAME

XGrab*.printer.horizDistance:    10
XGrab*.printer.vertDistance:     55
XGrab*.printer.label: To Command:

XGrab*.printerText.horizDistance:    100
XGrab*.printerText.vertDistance:     55
XGrab*.printerText.width: 100
XGrab*.printerText*string: DEFAULT_COMMAND


!----------------------------box 6
XGrab*.box6.horizDistance:   0
XGrab*.box6.vertDistance:  547


XGrab*.splat.label: -----------------------------------------------------

XGrab*.OK.horizDistance:   110
XGrab*.OK.vertDistance:     25
XGrab*.OK.width:            80
XGrab*.OK.font: *-helvetica-bold-r-normal--14-*
XGrab*.OK.label: OK

XGrab*.Dismiss.horizDistance:   265
XGrab*.Dismiss.vertDistance:     25
XGrab*.Dismiss.width:            80
XGrab*.Dismiss.font: *-helvetica-bold-r-normal--14-*
XGrab*.Dismiss.label: Dismiss


!----------------------------- pervasive resources
XGrab*font: *-helvetica-medium-r-normal--12-*
XGrab*Label.borderWidth: 0
XGrab*Toggle.borderWidth: 1
XGrab*Form.borderWidth: 0
XGrab*Toggle.shapeStyle: rectangle
XGrab*Command.shapeStyle: oval
XGrab*Text.editType: edit

XGrab*Form.title.right: chainRight
XGrab*Form.box5.right: chainRight
XGrab*Form.box5.Text.right: chainRight
XGrab*Form.box6.right: chainRight
XGrab*Form.box6.Dismiss.left: chainRight
XGrab*Form.box6.Dismiss.right: chainRight

XGrab*Form.Form.*.left: chainLeft
XGrab*Form.Form.*.right: chainLeft
XGrab*Form.Form.*.top: chainTop
XGrab*Form.Form.*.bottom: chainTop

XGrab*Form.background: #a3d1f2
XGrab*Label.background: #a3d1f2
!XGrab*Text*background: #d3f1ff
XGrab*Toggle.background: ivory
!XGrab*Command.background: #d3f1ff

XGrab*selstyle.foreground: navy
XGrab*outputFormat.foreground: navy
XGrab*psOptions.foreground: navy
XGrab*prOptions.foreground: navy
XGrab*outputLbl.foreground: navy
XGrab*foreground: navy
XGrab*background: #d3f1ff

! ========================================================================
! *
! * Name - XGrabxaw.res
! *
! * Version:	1.2
! *
! * ccsid:	@(#)XGrabxaw.res	1.2 - 06/28/93 09:13:39
! * from: 	ccs/s.XGrabxaw.res
! * date: 	06/28/93 09:14:48
! *
! * Copyright (c) 1991-2 Bruce Schuchardt.
! * Read the file cpyright.h for full copyright information.
! *
! *
! * Description: Application Defaults file for xgrab
! *
! *========================================================================

