/*****************************************************************************
**
** File:        ims_pmfTool.h
**
** Function:    Include file for ims_pmfTool constants
**
** Author:      Dan Crichton
**
** Date:        3/13/96
**
*****************************************************************************/

#ifndef IMS_PMFTOOL_H
#define IMS_PMFTOOL_H

static char *sccsPmfTool = "@(#)ims_pmfTool.h	1.4 11/24/97";

/*
** Main Window Definitions
*/

#define IMS_OT_MW_HT	800
#define	IMS_OT_MW_WID	700

typedef struct ims_ot_widget_list
{
	Widget widget;
	struct ims_ot_widget_list *next;
} IMS_OT_WIDGET_LIST;

typedef struct 
{
	Widget main_w;
	Widget msg_w;
	Widget work_w;
	Widget menubar_w;
	Widget open_w;
	Widget save_w;
	IMS_OT_WIDGET_LIST *headList;
	IMS_OT_WIDGET_LIST *tailList;
	XColor mandPixel, bgPixel, fgPixel;
} IMS_OT_WIDGETS;

typedef struct
{
	Widget widget;
	char *description;
	char *keyword;
	short int keyword_idx;
	short int significance;
	short int max_len;
	short int data_type;
} IMS_OT_KEYWORD;

typedef struct
{
	char *username;
	char *password;
	char *program;
	char *database;
	char *server;
	char *accountId;
	char *saveFile;
} IMS_OT_USERSPEC;

typedef struct
{
	DBCHAR item_name[IMS_COL60_LEN+1];
	DBSMALLINT item_id;
}	IMS_OT_CATALOG_ITEM;

extern IMS_OT_WIDGETS ims_otWidgets;
extern IMS_MSG_STRUCT *ims_ot_glbl_msg;
extern IMS_OT_USERSPEC ims_ot_userSpec;
extern IMS_ODL_TREE *ims_ot_tree, *ims_otFocusTree;
extern IMS_CLNT_EVENT ims_ot_request;
extern int data_type_count;
extern IMS_OT_CATALOG_ITEM data_type[IMS_COL10_LEN+1];
extern int significance_count;
extern IMS_OT_CATALOG_ITEM significance[IMS_COL10_LEN+1];


int ims_otBldMenuBar(IMS_MSG_STRUCT *, Widget);
int ims_otBldMsgWindow(IMS_MSG_STRUCT *, Widget);
void ims_otDatasetSelect(Widget);

#endif  /* !IMS_PMFTOOL_H */
