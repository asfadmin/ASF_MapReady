/*
 * Name: IK_Ingest.h
 *
 * Description: This file contains function prototypes for Data
 * Manager ingestion routines.
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
 * Revision 5.0.1.1  1995/11/06  13:45:43  ims
 * COPIED FROM 4.5.1.1 FOR GUI_PROMO_5_0_951106.
 *
 * Revision 4.5.1.1  1995/09/25  20:33:40  jong
 * Added IK_IngestPackage() to the prototype list.
 *
 * Revision 4.5  1995/07/27  18:42:28  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:20:34  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:49:43  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.2  1994/08/15  19:31:41  ryan
 * Changed prototype for IK_DirIngest to match GCMD changes.
 *
 * Revision 4.0.1.1  1994/08/15  17:56:40  ryan
 * new branch
 *
 * Revision 4.0  1994/06/08  17:10:51  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.2  1994/05/03  20:11:39  ims
 * Promotion from BUILD_94MAY03.
 *
 * Revision 3.1.1.3  1994/04/28  14:49:30  winter
 * Added #include <stdio.h> to remove error in odldef.h. For reference
 * to the reader, odldef.h requires previous #include <stdio.h>.
 *
 * Revision 3.1.1.2  1994/04/28  11:20:07  winter
 * Added #ifdef BRIDGE_CONFIG test to #include IK_BridgeExt.h.
 * THIS REVISION IS A MERGER OF 3.1.1.1 AND 3.0.1.1.
 *
 * Revision 3.1.1.1  1994/04/18  13:49:02  winter
 * Extensive code cleanup by Eric Winter.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.16  1993/06/28  20:58:14  sylvain
 * updated the prototype for IK_Sequence2List to match the function :-)
 *
 * Revision 1.15  1993/04/30  19:57:54  sylvain
 * added a fourth arg to IK_MDDataReceived to tell it to search gdbm if
 * the information wasn't found in memory.
 *
 * Revision 1.14  1992/11/19  15:53:43  sylvain
 * added IK_MDMarkIngested
 *
 * Revision 1.13  1992/11/16  23:08:54  sylvain
 * added IK_MDDataIngested() prototype, a function to tell you if the
 * GCMD results have already been ingested or not.
 *
 * Revision 1.12  1992/11/10  20:35:09  honce
 * IK_InBrowseAck() and IK_InPRequestAck() changed to
 * IK_BrowseAckIngest() and IK_PRequestAckIngest() to match c code.
 *
 * Revision 1.11  1992/11/08  20:34:36  honce
 * Added macro IK_ALIGN() to figure out memory alignment.
 *
 * Revision 1.10  1992/10/27  14:36:33  sylvain
 * fixed prototype for IK_IngestImage()
 *
 * Revision 1.9  1992/10/21  15:44:46  sylvain
 * added prototype for IK_DirIngest
 *
 * Revision 1.8  1992/10/06  20:06:47  honce
 * Removed prototype for IK_DirInit(), moved it to IK_Dataset.h.
 * Removed prototype for IK_InvInit(), a phantom function.
 *
 * Revision 1.7  1992/10/05  16:43:04  honce
 * Added prototypes for IK_IngestGranules() and IK_IngestDataset().
 *
 * Revision 1.6  1992/10/02  18:35:00  sylvain
 * added 2 includes so you only have to include this file to use an
 * ingestion routine.
 *
 * Revision 1.5  1992/10/01  18:11:39  sylvain
 * moved the CPP define over to here from IK_Ims.h
 *
 * Revision 1.4  1992/09/30  22:29:10  sylvain
 * added prototypes for IK_IngestValids and IK_IngestErrnos
 *
 * Revision 1.3  1992/09/30  22:22:01  honce
 * Added IK_MEMBUFSIZ & IK_MEMWASTE.  Removed misc. include of IK_Granule.h.
 *
 * Revision 1.2  1992/09/22  20:53:59  honce
 * Added function prototype for IK_Sequence2List()
 *
 * Revision 1.1  1992/09/10  15:24:34  honce
 * Initial revision
 *
*/

/*****************************************************************************/

#ifndef __IK_INGEST_H__
#define __IK_INGEST_H__

/*****************************************************************************/

/* #include directives */
#include <stdio.h>   /* Needed for odldef.h. */

/* Third-party library headers */
#include "odldef.h"
#include "odlinter.h"

/* IMS headers */
#ifdef BRIDGE_CONFIG
#include "IK_BridgeExt.h"
#endif
#include "IK_DataDic.h"
#include "IK_Dataset.h"
#include "IK_Gcmd.h"

/*****************************************************************************/

/* #define directives */

/* IK_MEMBUFSIZ is the minimum amount of memory to allocate when
   getting more memory. This means less reallocates and should save on
   performance. */
#define IK_MEMBUFSIZ (4 * 1024)

/* IK_MEMWASTE is the threshold on the amount of memory to waste. This
   number could save 1 realloc(). */
#define	IK_MEMWASTE (512)

#define IK_ALIGN(n) (sizeof(void *) * ((((n) + 1) / sizeof(void *)) + 1))

/*****************************************************************************/

/* Function prototypes */

extern int IK_IngestValids(char *);
extern int IK_IngestErrnos(char *);
extern int IK_DirIngest(VALUE, VALUE, VALUE, IK_Gcmd *);
extern int IK_MDDataReceived (char *, char *, char *, int);
extern int IK_DeleteMDList(void);
extern int IK_GCMDMarkIngested(char *, char *);
extern int IK_InsertMD (char *, char *,  char *);

extern int IK_BrowseAckIngest(AGGREGATE);
extern int IK_PRequestAckIngest(AGGREGATE);
extern int IK_IngestImage(const AGGREGATE, char *, size_t);
extern int IK_InvSearchIngest(AGGREGATE);

extern int IK_InvResultIngest(AGGREGATE);
extern int IK_IngestGranules(AGGREGATE);
extern int IK_IngestDataset(IK_ArchiveTag, IK_DatasetTag, AGGREGATE, int);

extern char *IK_MapImageFilename(IK_SortKey *, char *);

extern char **IK_Sequence2List(int *, PARAMETER, unsigned int);
extern int IK_IngestPackage(AGGREGATE);
/*---------------------------------------------------------------------------*/

/* The following definitions are needed by IK_Msg.c, but will probably
   be needed by other people as well. */

#ifndef CPP
#ifdef vms
#define CPP "cc/preprocess_only"
#else
#define CPP "/lib/cpp -P -C"	/* cpp, without line control info and
				   retaining comment info from the
				   file */
#endif   /* vms */
#endif   /* !CPP */

/*****************************************************************************/

#endif   /* !__IK_INGEST_H__ */
