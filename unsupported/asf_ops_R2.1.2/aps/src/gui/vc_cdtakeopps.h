
/*******************************************************************************
       vc_cdtakeopps.h
       This header file is included by vc_cdtakeopps.c

*******************************************************************************/

#ifndef	_VC_CDTAKEOPPS_INCLUDED
#define	_VC_CDTAKEOPPS_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	Widget	UxCreateDatatakeOpps;
	Widget	Uxlabel37;
	Widget	UxscrolledWindowList3;
	Widget	UxscrolledList_sites;
	Widget	UxTF_COMMENTS;
	Widget	UxTF_DARID;
	Widget	Uxlabel53;
	Widget	UxTF_QUICKLOOK;
	Widget	Uxlabel5;
	Widget	UxTF_SITENAME;
	Widget	Uxlabel54;
	Widget	UxTF_SHAPE;
	Widget	Uxlabel55;
	Widget	Uxlabel56;
	Widget	UxrowColumn3;
	Widget	UxtoggleButton_DARSites;
	Widget	UxtoggleButton_HypoSites;
	Widget	Uxlabel60;
	Widget	UxscrolledWindowText2;
	Widget	UxscrolledText_create_dtkopps;
	Widget	Uxseparator5;
	Widget	Uxlabel52;
	Widget	UxpushButton_cdtakeopps_quit;
	Widget	UxpushButton_create_dtkopps;
	Widget	Uxlabel67;
	Widget	Uxform_quad;
	Widget	UxTF_NW_LON;
	Widget	Uxlabel38;
	Widget	UxTF_NE_LON;
	Widget	Uxlabel45;
	Widget	Uxlabel_NE;
	Widget	UxTF_SW_LON;
	Widget	Uxlabel47;
	Widget	Uxlabel48;
	Widget	UxTF_SE_LAT;
	Widget	UxTF_SE_LON;
	Widget	Uxlabel50;
	Widget	Uxlabel51;
	Widget	Uxlabel_NW;
	Widget	UxTF_NE_LAT;
	Widget	UxTF_NW_LAT;
	Widget	UxTF_SW_LAT;
	Widget	Uxform_circle;
	Widget	UxTF_center_lon;
	Widget	UxTF_center_lat;
	Widget	Uxlabel1;
	Widget	Uxlabel2;
	Widget	Uxlabel3;
	Widget	UxtextField_radius;
	Widget	Uxlabel4;
	Widget	UxmenuCreateHypoSite;
	Widget	Uxmenu1_p1;
	Widget	UxmenuCreateHypoSite_Circle;
	Widget	UxmenuCreateHypoSite_Quad;
	Widget	Uxmenu1_top_b1;
	Widget	Uxform_dtk_info;
	Widget	Uxlabel57;
	Widget	Uxlabel59;
	Widget	UxsubMenu_cdtk_sensor;
	Widget	UxsubMenu_cdtk_ensor_SAR;
	Widget	UxoptionMenu_cdtk_sensor;
	Widget	Uxrc_CoverageType1;
	Widget	UxtoggleButton_Ascending;
	Widget	UxtoggleButton_Descending;
	Widget	UxTF_start_rev;
	Widget	UxTF_total_revs;
	Widget	Uxlabel65;
	Widget	Uxlabel68;
	Widget	UxsubMenu_cdtk_sat;
	Widget	UxsubMenu_cdtk_sat_ERS;
	Widget	UxoptionMenu_cdtk_sat;
	Widget	Uxlabel117;
	Widget	UxTF_dtkopps_total_days;
	Widget	Uxlabel119;
	Widget	UxTF_dtkopps_end;
	Widget	Uxlabel121;
	Widget	Uxlabel120;
	Widget	UxTF_dtkopps_start;
	Widget	Uxlabel66;
	Widget	Uxlabel118;
	Widget	UxTF_stop_rev;
	Widget	UxpushButton_cancel_create_site;
	Widget	UxpushButton_delete_site;
	Widget	UxpushButton_done_create_site;
	Widget	UxpushButton_Refresh;
	Widget	Uxseparator10;
	swidget	UxUxParent;
} _UxCCreateDatatakeOpps;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCreateDatatakeOpps  *UxCreateDatatakeOppsContext;
#define CreateDatatakeOpps      UxCreateDatatakeOppsContext->UxCreateDatatakeOpps
#define label37                 UxCreateDatatakeOppsContext->Uxlabel37
#define scrolledWindowList3     UxCreateDatatakeOppsContext->UxscrolledWindowList3
#define scrolledList_sites      UxCreateDatatakeOppsContext->UxscrolledList_sites
#define TF_COMMENTS             UxCreateDatatakeOppsContext->UxTF_COMMENTS
#define TF_DARID                UxCreateDatatakeOppsContext->UxTF_DARID
#define label53                 UxCreateDatatakeOppsContext->Uxlabel53
#define TF_QUICKLOOK            UxCreateDatatakeOppsContext->UxTF_QUICKLOOK
#define label5                  UxCreateDatatakeOppsContext->Uxlabel5
#define TF_SITENAME             UxCreateDatatakeOppsContext->UxTF_SITENAME
#define label54                 UxCreateDatatakeOppsContext->Uxlabel54
#define TF_SHAPE                UxCreateDatatakeOppsContext->UxTF_SHAPE
#define label55                 UxCreateDatatakeOppsContext->Uxlabel55
#define label56                 UxCreateDatatakeOppsContext->Uxlabel56
#define rowColumn3              UxCreateDatatakeOppsContext->UxrowColumn3
#define toggleButton_DARSites   UxCreateDatatakeOppsContext->UxtoggleButton_DARSites
#define toggleButton_HypoSites  UxCreateDatatakeOppsContext->UxtoggleButton_HypoSites
#define label60                 UxCreateDatatakeOppsContext->Uxlabel60
#define scrolledWindowText2     UxCreateDatatakeOppsContext->UxscrolledWindowText2
#define scrolledText_create_dtkopps UxCreateDatatakeOppsContext->UxscrolledText_create_dtkopps
#define separator5              UxCreateDatatakeOppsContext->Uxseparator5
#define label52                 UxCreateDatatakeOppsContext->Uxlabel52
#define pushButton_cdtakeopps_quit UxCreateDatatakeOppsContext->UxpushButton_cdtakeopps_quit
#define pushButton_create_dtkopps UxCreateDatatakeOppsContext->UxpushButton_create_dtkopps
#define label67                 UxCreateDatatakeOppsContext->Uxlabel67
#define form_quad               UxCreateDatatakeOppsContext->Uxform_quad
#define TF_NW_LON               UxCreateDatatakeOppsContext->UxTF_NW_LON
#define label38                 UxCreateDatatakeOppsContext->Uxlabel38
#define TF_NE_LON               UxCreateDatatakeOppsContext->UxTF_NE_LON
#define label45                 UxCreateDatatakeOppsContext->Uxlabel45
#define label_NE                UxCreateDatatakeOppsContext->Uxlabel_NE
#define TF_SW_LON               UxCreateDatatakeOppsContext->UxTF_SW_LON
#define label47                 UxCreateDatatakeOppsContext->Uxlabel47
#define label48                 UxCreateDatatakeOppsContext->Uxlabel48
#define TF_SE_LAT               UxCreateDatatakeOppsContext->UxTF_SE_LAT
#define TF_SE_LON               UxCreateDatatakeOppsContext->UxTF_SE_LON
#define label50                 UxCreateDatatakeOppsContext->Uxlabel50
#define label51                 UxCreateDatatakeOppsContext->Uxlabel51
#define label_NW                UxCreateDatatakeOppsContext->Uxlabel_NW
#define TF_NE_LAT               UxCreateDatatakeOppsContext->UxTF_NE_LAT
#define TF_NW_LAT               UxCreateDatatakeOppsContext->UxTF_NW_LAT
#define TF_SW_LAT               UxCreateDatatakeOppsContext->UxTF_SW_LAT
#define form_circle             UxCreateDatatakeOppsContext->Uxform_circle
#define TF_center_lon           UxCreateDatatakeOppsContext->UxTF_center_lon
#define TF_center_lat           UxCreateDatatakeOppsContext->UxTF_center_lat
#define label1                  UxCreateDatatakeOppsContext->Uxlabel1
#define label2                  UxCreateDatatakeOppsContext->Uxlabel2
#define label3                  UxCreateDatatakeOppsContext->Uxlabel3
#define textField_radius        UxCreateDatatakeOppsContext->UxtextField_radius
#define label4                  UxCreateDatatakeOppsContext->Uxlabel4
#define menuCreateHypoSite      UxCreateDatatakeOppsContext->UxmenuCreateHypoSite
#define menu1_p1                UxCreateDatatakeOppsContext->Uxmenu1_p1
#define menuCreateHypoSite_Circle UxCreateDatatakeOppsContext->UxmenuCreateHypoSite_Circle
#define menuCreateHypoSite_Quad UxCreateDatatakeOppsContext->UxmenuCreateHypoSite_Quad
#define menu1_top_b1            UxCreateDatatakeOppsContext->Uxmenu1_top_b1
#define form_dtk_info           UxCreateDatatakeOppsContext->Uxform_dtk_info
#define label57                 UxCreateDatatakeOppsContext->Uxlabel57
#define label59                 UxCreateDatatakeOppsContext->Uxlabel59
#define subMenu_cdtk_sensor     UxCreateDatatakeOppsContext->UxsubMenu_cdtk_sensor
#define subMenu_cdtk_ensor_SAR  UxCreateDatatakeOppsContext->UxsubMenu_cdtk_ensor_SAR
#define optionMenu_cdtk_sensor  UxCreateDatatakeOppsContext->UxoptionMenu_cdtk_sensor
#define rc_CoverageType1        UxCreateDatatakeOppsContext->Uxrc_CoverageType1
#define toggleButton_Ascending  UxCreateDatatakeOppsContext->UxtoggleButton_Ascending
#define toggleButton_Descending UxCreateDatatakeOppsContext->UxtoggleButton_Descending
#define TF_start_rev            UxCreateDatatakeOppsContext->UxTF_start_rev
#define TF_total_revs           UxCreateDatatakeOppsContext->UxTF_total_revs
#define label65                 UxCreateDatatakeOppsContext->Uxlabel65
#define label68                 UxCreateDatatakeOppsContext->Uxlabel68
#define subMenu_cdtk_sat        UxCreateDatatakeOppsContext->UxsubMenu_cdtk_sat
#define subMenu_cdtk_sat_ERS    UxCreateDatatakeOppsContext->UxsubMenu_cdtk_sat_ERS
#define optionMenu_cdtk_sat     UxCreateDatatakeOppsContext->UxoptionMenu_cdtk_sat
#define label117                UxCreateDatatakeOppsContext->Uxlabel117
#define TF_dtkopps_total_days   UxCreateDatatakeOppsContext->UxTF_dtkopps_total_days
#define label119                UxCreateDatatakeOppsContext->Uxlabel119
#define TF_dtkopps_end          UxCreateDatatakeOppsContext->UxTF_dtkopps_end
#define label121                UxCreateDatatakeOppsContext->Uxlabel121
#define label120                UxCreateDatatakeOppsContext->Uxlabel120
#define TF_dtkopps_start        UxCreateDatatakeOppsContext->UxTF_dtkopps_start
#define label66                 UxCreateDatatakeOppsContext->Uxlabel66
#define label118                UxCreateDatatakeOppsContext->Uxlabel118
#define TF_stop_rev             UxCreateDatatakeOppsContext->UxTF_stop_rev
#define pushButton_cancel_create_site UxCreateDatatakeOppsContext->UxpushButton_cancel_create_site
#define pushButton_delete_site  UxCreateDatatakeOppsContext->UxpushButton_delete_site
#define pushButton_done_create_site UxCreateDatatakeOppsContext->UxpushButton_done_create_site
#define pushButton_Refresh      UxCreateDatatakeOppsContext->UxpushButton_Refresh
#define separator10             UxCreateDatatakeOppsContext->Uxseparator10
#define UxParent                UxCreateDatatakeOppsContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CreateDatatakeOpps( swidget _UxUxParent );

#endif	/* _VC_CDTAKEOPPS_INCLUDED */
