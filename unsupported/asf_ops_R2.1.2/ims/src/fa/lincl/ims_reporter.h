/*****************************************************************************
**
**
** File:    ims_reporter.h
**
** Function: Include file for ims_reporter
**
** Author: Dan Crichton
**
** Date:    1/9/96
**
**
*****************************************************************************/

#ifndef IMS_REPORTER_H

#define IMS_REPORTER_H

static char *sccsReporter = "@(#)ims_reporter.h	5.4 03/13/97";

/*****************************************************************************
** The following contain definitions for the dialog (X1, Y1) - (X2, Y2) 
** positions for each of the widgets used in ims_reporter.
*****************************************************************************/

/*
** Main Window Definitions
*/

#define IMS_FA_MW_X1	0
#define IMS_FA_MW_Y1	0
#define	IMS_FA_MW_X2	700
#define	IMS_FA_MW_Y2	550

/*
** Main Window Buttons
*/

#define IMS_FA_MW_EXIT_X1	240
#define IMS_FA_MW_EXIT_Y1	450

#define IMS_FA_MW_GEN_X1	65
#define IMS_FA_MW_GEN_Y1	350

#define IMS_FA_MW_RESEND_X1	415
#define IMS_FA_MW_RESEND_Y1	350

#define IMS_FA_MW_VIEWH_X1	65
#define IMS_FA_MW_VIEWH_Y1	400

#define IMS_FA_MW_VIEWA_X1	415
#define IMS_FA_MW_VIEWA_Y1	400

/*
** Copyright location for main window.
*/

#define IMS_FA_MW_COPY1_X1	220
#define IMS_FA_MW_COPY1_Y1	500

#define IMS_FA_MW_COPY2_X1	235
#define IMS_FA_MW_COPY2_Y1	520

/*
** Title location for main window.
*/

#define IMS_FA_MW_TITLE1_X1	100
#define IMS_FA_MW_TITLE1_Y1	20

/*
** Picture location for main window.
*/

#define IMS_FA_MW_PIC_X1	75
#define IMS_FA_MW_PIC_Y1	95

/*
** Generation Main Window
*/

#define IMS_FA_GEN_X1	0
#define IMS_FA_GEN_Y1	0
#define	IMS_FA_GEN_X2	450
#define	IMS_FA_GEN_Y2	450


/*
** Main Window Job Information Widget
*/

#define IMS_FA_JI_X1	0
#define IMS_FA_JI_Y1	240
#define	IMS_FA_JI_X2	420
#define IMS_FA_JI_Y2	140

/*
** Main Window Job Information Status
*/

#define IMS_FA_JS1_X1	10	
#define IMS_FA_JS2_X1	65
#define IMS_FA_JS3_X1	164
#define IMS_FA_JS4_X1	342
#define IMS_FA_JS_Y1	225

/*
** Main Window Submit Button
*/

#define IMS_FA_MWSUBMIT_X1	50
#define IMS_FA_MWSUBMIT_Y1	170

/*
** Main Window Quit Button
*/

#define IMS_FA_MWQUIT_X1	280
#define IMS_FA_MWQUIT_Y1	170

/*
** Main Window Scale
*/

#define IMS_FA_SCALE_X1	150
#define IMS_FA_SCALE_Y1	110

/*
** Main Window Title
*/

#define IMS_FA_MWTITLE_X1	130
#define IMS_FA_MWTITLE_Y1	30

/*
** Acquisition Dialog
*/

#define IMS_FA_ACQ_X1	40
#define IMS_FA_ACQ_Y1	40
#define IMS_FA_ACQ_X2	400
#define IMS_FA_ACQ_Y2	230

/*
** Acquisition Dialog Submit Button
*/

#define IMS_FA_AQSUBMIT_X1	50
#define IMS_FA_AQSUBMIT_Y1	200

/*
** Acquisition Dialog Quit Button
*/
#define IMS_FA_AQQUIT_X1	240
#define IMS_FA_AQQUIT_Y1	200

/*
** Acquisition Dialog Start Date
*/
#define IMS_FA_AQSTART_X1	50
#define IMS_FA_AQSTART_Y1	80

/*
** Acquisition Dialog End Date
*/
#define IMS_FA_AQEND_X1		50 
#define IMS_FA_AQEND_Y1		120

/*
** Acquisition Dialog Pass Number Text Widget
** Note: the start/end dates and rev should not
** be on the same acquisition dialog box.
*/

#define IMS_FA_AQPASS_X1	50
#define IMS_FA_AQPASS_Y1	100	

/*
** Acquisition Dialog Title
*/

#define IMS_FA_AQTITLE_X1	50
#define IMS_FA_AQTITLE_Y1	20

/*
** Single Tape Dialog Box
*/

#define IMS_FA_STAPE_X1		40
#define IMS_FA_STAPE_Y1		100
#define IMS_FA_STAPE_X2		400
#define IMS_FA_STAPE_Y2		320

/*
** Single Tape Available List Window Headers
*/
#define IMS_FA_STA_TITLE_X1	65
#define IMS_FA_STA_TITLE_Y1	60


/*
** Single Tape Available List Window Headers
*/
#define IMS_FA_STA_STATUS1_X1	10
#define IMS_FA_STA_STATUS2_X1	120
#define IMS_FA_STA_STATUS3_X1	225

#define IMS_FA_STA_STATUS_Y1	100


/*
** Single Tape Available List Window
*/

#define IMS_FA_ST_TA_X1		10
#define IMS_FA_ST_TA_Y1		140
#define IMS_FA_ST_TA_X2		350
#define IMS_FA_ST_TA_Y2		120


/*
** Single Tape Submit Button
*/

#define IMS_FA_ST_SUBMIT_X1	50
#define IMS_FA_ST_SUBMIT_Y1	280

/*
** Single Tape Quit Button
*/

#define IMS_FA_ST_QUIT_X1	240
#define IMS_FA_ST_QUIT_Y1	280


/*
** Single Tape Title
*/

#define IMS_FA_ST_TITLE_X1	50
#define IMS_FA_ST_TITLE_Y1	20


/*
** Build Schedule ID
*/

#define IMS_FA_BS_X1	40
#define IMS_FA_BS_Y1	40
#define IMS_FA_BS_X2	400
#define IMS_FA_BS_Y2	170

/*
** Schedule ID Submit Button
*/

#define IMS_FA_BS_SUBMIT_X1	50
#define IMS_FA_BS_SUBMIT_Y1	140

/*
** Schedule ID Quit Button
*/

#define IMS_FA_BS_QUIT_X1	240
#define IMS_FA_BS_QUIT_Y1	140

/*
** Schedule ID Rev Text 
*/

#define IMS_FA_BS_TEXT_X1	40
#define IMS_FA_BS_TEXT_Y1	70

/*
** Schedule ID Seq Text
*/

#define IMS_FA_BS_TEXT2_X1	40
#define IMS_FA_BS_TEXT2_Y1	105

/*
** Schedule ID Title
*/

#define IMS_FA_BS_TITLE_X1	50
#define IMS_FA_BS_TITLE_Y1	20

/*
** Multiple Shipping Dialog
*/

#define IMS_FA_MS_X1	40
#define IMS_FA_MS_Y1	40
#define IMS_FA_MS_X2	450	
#define IMS_FA_MS_Y2	590

/*
** Tapes Shipped List Window for Multiple Shipping Dialog
*/

#define IMS_FA_MS_TAPES_X1	0
#define IMS_FA_MS_TAPES_Y1	70
#define IMS_FA_MS_TAPES_X2	300
#define IMS_FA_MS_TAPES_Y2	80


/*
** Tapes Shipped List Window Status
*/

#define IMS_FA_MS_STATUS1_X1	10
#define IMS_FA_MS_STATUS2_X1	210

#define IMS_FA_MS_STATUS_Y1		45

/*
** Multiple Shipping Dialog Remove Button
*/

#define IMS_FA_MS_REMOVE_X1	345
#define IMS_FA_MS_REMOVE_Y1	90

/*
** Multiple Shipping Dialog Shipping Date
*/ 

#define IMS_FA_MS_DATE_X1	100
#define IMS_FA_MS_DATE_Y1	450

/*
** Multiple Shipping Dialog Tapes Available List
*/

#define IMS_FA_MS_TA_X1	0
#define IMS_FA_MS_TA_Y1	230
#define IMS_FA_MS_TA_X2	320
#define IMS_FA_MS_TA_Y2	190

/*
** Multiple Shipping Dialog Tape Available Title
*/

#define IMS_FA_TA_TITLE_X1	40
#define IMS_FA_TA_TITLE_Y1	170

/*
** Multiple Shipping Dialog Tapes Status 
*/

#define IMS_FA_TA_STATUS1_X1	10
#define IMS_FA_TA_STATUS2_X1	120
#define IMS_FA_TA_STATUS3_X1	225


#define IMS_FA_TA_STATUS_Y1		200

/*
** Multiple Shipping Dialog Add Button
*/

#define IMS_FA_MS_ADD_X1	345
#define IMS_FA_MS_ADD_Y1	330

/*
** Multiple Shipping Dialog Submit Button
*/

#define IMS_FA_MS_SUBMIT_X1	50
#define IMS_FA_MS_SUBMIT_Y1	540

/*
** Multiple Shipping Dialog Quit Button
*/

#define IMS_FA_MS_QUIT_X1	240
#define IMS_FA_MS_QUIT_Y1	540

/*
** Multiple Shipping Dialog Title
*/

#define IMS_FA_MS_TITLE_X1	40
#define IMS_FA_MS_TITLE_Y1	15

/*
** Logon Box
*/

#define IMS_FA_LOGON_X1		312
#define IMS_FA_LOGON_Y1		284
#define IMS_FA_LOGON_X2		375
#define IMS_FA_LOGON_Y2		250

/*
** Logon Submit Button
*/
#define IMS_FA_LG_SUBMIT_X1	50
#define IMS_FA_LG_SUBMIT_Y1	160

/*
** Logon Quit Button
*/

#define IMS_FA_LG_QUIT_X1	240
#define IMS_FA_LG_QUIT_Y1	160

/*
** Logon User Text Field
*/

#define IMS_FA_LG_USER_X1	40
#define IMS_FA_LG_USER_Y1	30

/*
** Logon Password
*/

#define IMS_FA_LG_PASS_X1	40
#define IMS_FA_LG_PASS_Y1	60

/*
** Logon Server
*/

#define IMS_FA_LG_SERVER_X1	40
#define IMS_FA_LG_SERVER_Y1	90

/*
** Logon Database
*/
#define IMS_FA_LG_DATABASE_X1	40
#define IMS_FA_LG_DATABASE_Y1	120

/*
** Logon Copyright notice line 1
*/
#define IMS_FA_LG_COPY1_X1		25
#define IMS_FA_LG_COPY1_Y1		200


/*
** Logon copyright notice line 2
*/
#define IMS_FA_LG_COPY2_X1		45
#define IMS_FA_LG_COPY2_Y1		215


/*
** View Acqusition Screen Sizes
*/

#define IMS_FA_VAQ_X2		500
#define IMS_FA_VAQ_Y2		340

#define IMS_FA_VAQ_ACQLAB_X1	10
#define IMS_FA_VAQ_ACQLAB_Y1	20

#define IMS_FA_VAQ_ACQRADIO_X1	10
#define IMS_FA_VAQ_ACQRADIO_Y1	40

#define IMS_FA_VAQ_PLATFORM_X1	10
#define IMS_FA_VAQ_PLATFORM_Y1	85

#define IMS_FA_VAQ_DATELAB_X1	10
#define IMS_FA_VAQ_DATELAB_Y1	130

#define IMS_FA_VAQ_DATERADIO_X1	10
#define IMS_FA_VAQ_DATERADIO_Y1	150

#define IMS_FA_VAQ_DATEFROM_X1	130
#define IMS_FA_VAQ_DATEFROM_Y1	150

#define IMS_FA_VAQ_DATETOLAB_X1	290
#define IMS_FA_VAQ_DATETOLAB_Y1	157

#define IMS_FA_VAQ_DATETO_X1	315	
#define IMS_FA_VAQ_DATETO_Y1	150

#define IMS_FA_VAQ_REVLAB_X1	10
#define IMS_FA_VAQ_REVLAB_Y1	200

#define IMS_FA_VAQ_REVRADIO_X1	10
#define IMS_FA_VAQ_REVRADIO_Y1	220

#define IMS_FA_VAQ_REVFROM_X1	130
#define IMS_FA_VAQ_REVFROM_Y1	220

#define IMS_FA_VAQ_REVTOLAB_X1	210
#define IMS_FA_VAQ_REVTOLAB_Y1	227

#define IMS_FA_VAQ_REVTO_X1		235
#define IMS_FA_VAQ_REVTO_Y1		220

#define IMS_FA_VAQ_QUERY_X1		110
#define IMS_FA_VAQ_QUERY_Y1		290

#define IMS_FA_VAQ_CANCEL_X1	240
#define IMS_FA_VAQ_CANCEL_Y1	290

/*
** View Acquisition Query Window
*/

#define IMS_FA_VAL_X2       760
#define IMS_FA_VAL_Y2       400

/*
** View History Dialog Settings
*/

#define IMS_FA_HIS_X2		500
#define IMS_FA_HIS_Y2		280

#define IMS_FA_HIS_REPORT_X1	0
#define IMS_FA_HIS_REPORT_Y1	25

#define IMS_FA_HIS_DATELAB_X1	10
#define IMS_FA_HIS_DATELAB_Y1	70

#define IMS_FA_HIS_DATERADIO_X1	10
#define IMS_FA_HIS_DATERADIO_Y1	90

#define IMS_FA_HIS_DATEFROM_X1	135
#define IMS_FA_HIS_DATEFROM_Y1	90

#define IMS_FA_HIS_DATETOLAB_X1	290
#define IMS_FA_HIS_DATETOLAB_Y1	97

#define IMS_FA_HIS_DATETO_X1	315	
#define IMS_FA_HIS_DATETO_Y1	90

#define IMS_FA_HIS_NAMELAB_X1	10
#define IMS_FA_HIS_NAMELAB_Y1	150

#define IMS_FA_HIS_NAMERADIO_X1	10
#define IMS_FA_HIS_NAMERADIO_Y1	170

#define IMS_FA_HIS_NAMEFROM_X1	140
#define IMS_FA_HIS_NAMEFROM_Y1	170

#define IMS_FA_HIS_QUERY_X1		110
#define IMS_FA_HIS_QUERY_Y1		240

#define IMS_FA_HIS_CANCEL_X1	240
#define IMS_FA_HIS_CANCEL_Y1	240

/*
** Define View History List Window
*/

#define IMS_FA_HISL_X2       760
#define IMS_FA_HISL_Y2       400

/*
** Define View History List Window
*/

#define IMS_FA_RESEND_X2       760
#define IMS_FA_RESEND_Y2       400

#endif
