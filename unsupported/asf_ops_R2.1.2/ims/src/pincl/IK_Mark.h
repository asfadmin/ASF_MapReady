/*
* Name: IK_Mark.h
 *
 * Description: This file defines data structures and manifest
 * constants for IC_Mark*() routines. Why IC?
 *
 * Notes:
 *
 *---------------------------------------------------------------------------
 *
 * RCS information:
 *
 * $RCSfile$
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0  1995/11/06  13:03:54  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:42:34  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.1  1995/02/13  22:36:18  ims
 * COPIED FROM 4.3.1.1 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.1  1994/08/26  11:59:12  ims
 * COPIED FROM REVISION 4.0.1.1.
 *
 * Revision 4.0.1.1  1994/06/08  17:11:46  ims
 * COPIED FROM REVISION 3.1.1.1.
 *
 * Revision 3.1.1.1  1994/04/18  13:49:56  winter
 * Extensive code cleanup by Eric Winter.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.4  1993/08/13  19:20:46  sudarsan
 * Modified IK_IMAGE_AVAIL to be IK_BROWSE_AVAIL - to handle
 * availablity of ftp browse availability in addition to
 * image availability. Also created defines for NO_BROWSE,
 * INTEGRATED, & FTP.
 *
 * Revision 1.3  1993/01/06  19:25:41  honce
 * IK_Itemstatus type deleted.  AIX compiler would not allow bit fields to
 * use enums.  #define's created to take care of problem and they work on
 * all compilers.  So far, ...
 *
 * Revision 1.2  1992/10/26  18:30:12  honce
 * Added IK_SELECTED as an IK_MARK
 *
 * Revision 1.1  1992/09/10  15:24:34  honce
 * Initial revision
 *
*/

/*****************************************************************************/

#ifndef __IK_MARK_H__
#define __IK_MARK_H__

/*****************************************************************************/

/* IK_Mark structures are used for the current status of granule or
   dataset record. */
typedef enum IK_Mark {

  IK_ALL_FIELDS = 0,       /* Use all status fields */
#define IK_DONT_CARE (0)   /* Don't care about mark "state" */

  IK_VISIBLE = 1,          /* record can be displayed */
#define IK_TOGGLE (1)      /* Toggle from current 1/0 state */

  IK_BROWSE_AVAIL = 2,            /* browse int/ftp on client */
#define IK_NOBROWSE_AVAIL   (0)   /* both not avail */
#define IK_INTEGRATED_AVAIL (1)   /* browse image on client */ 
#define IK_FTPB_AVAIL       (2)   /* browse image on client */ 

  IK_FOR_DELETE = 4,     /* record has been deleted */
  IK_FOR_COVERAGE = 8,   /* user wants coverage map */
  IK_FOR_DETAIL = 16,    /* user wants detail information */

  IK_FOR_ORDER = 32,      /* user wants to order */
#define IK_NO_ORDER (0)   /* user doesn't want granule */
#define IK_WANTS    (1)   /* record is of interest */

  IK_FOR_BROWSE = 64,       /* user wants browse */
#define IK_NO_BROWSE  (0)   /* No Browse available/ wanted */
#define IK_INTEGRATED (1)   /* integrated browse available/ wanted */
#define IK_FTP        (2)   /* ftp browse available/ wanted */

  IK_SELECTED = 128   /* user wants to operate */

} IK_Mark;

typedef unsigned IK_ItemStatus;

/*****************************************************************************/

#endif   /* !__IK_MARK_H__ */
