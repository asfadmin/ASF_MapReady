#ifndef PPS_COLUMNLIST_H
#define PPS_COLUMNLIST_H

#include "gui_common.h"

typedef enum
{
	PPS_TEXT_COLUMN_LIST,
	PPS_INT_COLUMN_LIST,
	PPS_FLOAT_COLUMN_LIST

} PPSColumnDataTypeE;

typedef struct _PPSColumnListSpec
{
	char				label[PPS_STRING_LEN];
	int					width;
	PPSColumnDataTypeE	dataType;

} PPSColumnListSpec;


/*-----------------------------------------------*/
/* constructor: create a column list             */
/*                                               */
/*    Note: don't worry about space allocated    */
/*          for columnClientData.  It will be    */
/*          freed when the widget is destroyed.  */
/*-----------------------------------------------*/
Widget ppsCreateColumnList(Widget               parent,
                           String               name,
                           PPSColumnListSpec    *columnListSpecs,
                           int                  numColumnLists,
                           int                  visibleItems);

/*-----------------------------------------------*/
/* replace all the items in the column list      */
/*                                               */
/* varargs:                                      */
/*    Widget columnListWidget;                   */
/*                 // returned from create call  */
/*    int    rows; // number of rows             */
/*    int    columns; // number of columns       */
/*    void*  array_of_1st_list;                  */
/*    void*  array_of_2nd_list;                  */
/*    ........................                   */
/*    void*  array_of_last_list;                 */
/*-----------------------------------------------*/
void ppsColumnListReplaceAllItems();

/*-----------------------------------------------*/
/* add items at the end of the column list       */
/*                                               */
/* varargs:                                      */
/*    Widget columnListWidget;                   */
/*                 // returned from create call  */
/*    int    rows; // number of rows             */
/*    int    columns; // number of columns       */
/*    void*  array_of_1st_list;                  */
/*    void*  array_of_2nd_list;                  */
/*    ........................                   */
/*    void*  array_of_last_list;                 */
/*-----------------------------------------------*/
void ppsColumnListAddItems(
						Widget		columnListWidget,
						int			rows,
						int			columns,
						void**		valueArrays,
						int*		textSizes);

short
ppsColumnListRecalcWidth(Widget columnList);

int
ppsColumnListGetItemCount(Widget columnList);

void
ppsColumnListDeleteAllItems(Widget columnList);

extern Widget		drawArea;

#endif
