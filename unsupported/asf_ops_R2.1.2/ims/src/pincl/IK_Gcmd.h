/*
 * Name: IK_Gcmd.h
 *
 * Description: This file includes some definitions related to talking
 * to the GCMD (Global Change Master Directory).
 *
 * Notes:
 *
 *-------------------------------------------------------------------------
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
 * Revision 5.0  1995/11/06  13:03:40  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:42:12  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.1  1995/02/13  22:35:07  ims
 * COPIED FROM 4.3.1.1 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.1  1994/08/26  11:49:35  ims
 * COPIED FROM REVISION 4.0.1.1.
 *
 * Revision 4.0.1.1  1994/06/08  17:06:43  ims
 * COPIED FROM REVISION 3.1.1.1..
 *
 * Revision 3.1.1.1  1994/04/18  13:27:45  winter
 * Extensive code cleanup by Eric Winter.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.1.2  1994/02/07  14:58:03  winter
 * Changed IK_MDTupple_Sep to IK_MDTuple_Sep.
 *
 * Revision 3.0.1.1  1994/02/07  14:56:39  winter
 * Placeholder revision for code cleanup by Eric Winter and Pat Ryan.
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.4  1993/02/03  13:30:15  sylvain
 * added ifdef's for the non-normalized geographic coverage values. You can
 * get the normalized values by compiling IK_Gcmd.o with USE_NORMALIZED_COVERAGE
 * defined on the compile line.
 *
 * Revision 1.3  1992/11/24  18:41:29  sylvain
 * added prototype for CloseMDSocket ()
 *
 * Revision 1.2  1992/10/18  22:34:05  sylvain
 * took out the get brief sql stmt
 *
 * Revision 1.1  1992/10/16  18:04:56  sylvain
 * Initial revision
 *
*/

/*****************************************************************************/

#ifndef __IK_GCMD_H__
#define __IK_GCMD_H__

/*****************************************************************************/

/* The majority of the following was borrowed from the MD small client
 * that was used to text the GCMD. */

/*****************************************************************************/

/* #include directives */

/* Standard headers */
#include <stdio.h>

/* Third-party library headers */
#include "mdcodes.h"

/* IMS headers */
#include "inet.h"
 
/*****************************************************************************/

/* Function prototypes */

/* GCMD functions that I use. */
extern int Network_Request(char*, char*, int, int);
extern void chgupper (char*);
extern int CompressMultiSpaces (char*);
extern void CloseMDSocket (int);

/*****************************************************************************/

/* #define directives */

#define IK_MDBUF_SZ    (4096)
#define IK_MDQUERY_SZ  (512)
#define IK_DIF_SZ      (60)
#define IK_MDTuple_Sep "\n"
#define IK_MDField_Sep "~"

#ifdef USE_NORMALIZED_COVERAGE
#define IK_MDCoverageField_Sep IK_MDField_Sep
#else
#define IK_MDCoverageField_Sep "~NSEW,"
#endif

/*****************************************************************************/

/* The concatenation of entry_title1 and entry_title2, are dm_entry
 * the data manager gdbm. The concatenation of start_date, start_time,
 * and start_zone is md_start_time in the data manager.  The
 * concatenation of stop_date, stop_time. and stop_zone is
 * md_stop_time to the data manger. */

/* NOTE: Why are #define directives used within the structure
   definition? */

typedef struct IK_Gcmd {
  char *de_id;
  char *entry_title1;
  char *entry_title2;
  char *start_date;
  char *start_time;
  char *start_zone;
  char *stop_date;
  char *stop_time;
  char *stop_zone;
#ifdef USE_NORMALIZED_COVERAGE
  char *min_lat;
  char *max_lat;
  char *min_long;
  char *max_long;
#else
  char *min_lat;
  char *max_long;
  char *max_lat;
  char *min_long;
#endif
#define IK_NUMSELECTRESULTS (13)   /* the number of results returned
				      by the select */
  char *summary_dif;
  char *attribute_dif;
  char *data_center_dif;
  char *personnel_dif;
  char *references_dif;
#define IK_NUMDIFRESULTS (5)   /* The number of dif results */
  struct IK_Gcmd *next_results;
} IK_Gcmd;

/*****************************************************************************/

/* Public readable header */
#define IK_PUBHEADER_SZ (IK_NUMSELECTRESULTS + IK_NUMDIFRESULTS + 1)
#define IK_MDHEADER_SZ IK_PUBHEADER_SZ

/*****************************************************************************/

#endif   /* ! __IK_GCMD_H__ */
