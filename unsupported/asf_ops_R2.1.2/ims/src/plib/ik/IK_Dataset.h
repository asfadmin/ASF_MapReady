
/*
 * Name: IK_Dataset.h
 *
 * Description: This file contains definitions for data structures and
 * manifest constants for IK_Dataset data types, and function
 * prototypes for related functions (which are scattered across several
 * source files).
 *
 * Notes:
 *
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
 * Revision 5.0.1.2  1995/11/28  17:00:51  shiva
 * changed errorMsg from string to sequence string: char **errorMsg;
 *
 * Revision 5.0.1.1  1995/11/13  22:18:11  shiva
 * Place holder for 5.0.1 branch.
 *
 * Revision 5.0  1995/11/06  13:03:31  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.5  1995/09/13  21:53:42  shiva
 * Increased VALID_ACCOUNTS_MAX_NUM from 5 to 10 until a more permanent
 * solution is found such as making this value dynamic based on how
 * many the DAACs send.
 *
 * Revision 4.5.1.4  1995/08/24  15:19:01  shiva
 * Added a new field to the Dataset structure to hold the number of
 * VALID_ACCOUNTs sent.
 *
 * Revision 4.5.1.3  1995/08/23  19:11:58  shiva
 * Fixed errors.  Now VALID_ACCOUNT successfully ingests into the dataset
 * structure dsP.
 *
 * Revision 4.5.1.2  1995/08/21  21:19:11  shiva
 * First cut of the billing enhancement changes.
 *
 * Revision 4.5.1.1  1995/08/21  15:17:24  shiva
 * Place holder for the branch.
 *
 * Revision 4.5  1995/07/27  18:41:57  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:19:49  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:49:14  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0  1994/06/08  16:56:06  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.2.1.2  1994/05/04  19:12:09  winter
 * COPIED FROM REVISION 3.1.1.3.
 *
 * Revision 3.1.1.3  1994/05/02  11:49:45  winter
 * Changed char *org_center to char org_center[IK_ORG_CENTER_SZ] in the
 * IK_Dataset structure.
 *
 * Revision 3.1.1.2  1994/04/28  18:45:16  winter
 * Added char *org_center to IK_Dataset structure. This data is needed by
 * the GCMD, if the data exists.
 *
 * Revision 3.1.1.1  1994/04/28  10:46:28  winter
 * THIS REVISION IS A COPY OF REVISION 3.0.1.4, WHICH IS A MERGER OF
 * 3.0.1.3 AND 3.0.2.1. ALL FURTHER DEVELOPMENT SHOULD BE BASED ON THIS
 * REVISION.
 *
 * Revision 3.0.1.4  1994/04/28  10:44:21  winter
 * Added #ifdef BRIDGE to allow char * argument to IK_DirInit().
 * THIS REVISION MERGES 3.0.1.3 AND 3.0.2.1..
 *
 * Revision 3.0.1.3  1994/04/18  12:54:58  winter
 * More code cleanup by Eric Winter.
 *
 * Revision 3.0.1.2  1994/03/21  22:38:43  winter
 * Added char **browse_desc member (Browse Product Description) to the
 * definition of the IK_Dataset structure.
 *
 * Revision 3.0.1.1  1994/03/21  17:13:52  winter
 * Added a lot of comments. Removed conditional rcsid variable. Added
 * #include IK_DataDic.h. Moved IK_DatasetStatus data structure definition
 * to before the IK_Dataset structure definition. Removed #ifdef FUTURE_FEATURE
 * clause, and fixed resulting IC_GetDatasetKeys() prototype. Added argument
 * names to function prototypes, from their definitions. Sorted prototypes by
 * source file and alphabetically.
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.6  1993/04/22  13:49:04  shermer
 * IK_Dataset.h is included in IX_protos.h for
 *   IX_browse.c   	IX_cover.c	  IX_brstats.c
 *
 * Added:
 *
 *  #include "IK_Ims.h"
 *   analysis: IMS Shared User File defines tag size constant: IK_DATASET_TAG_SZ
 *
 * Revision 1.5  1993/01/06  19:22:29  honce
 * type IK_ItemStatus changed to unsigned. Private data moved to IK_DatastP.h.
 *
 * Revision 1.4  1992/10/06  20:06:20  honce
 * Added prototypes for IK_DirInit() and IK_DirClose()
 *
 * Revision 1.3  1992/10/06  19:10:02  honce
 * Added prototype for IC_ResetDatasetStatus()
 *
 * Revision 1.2  1992/09/14  21:19:33  honce
 * Corrected prototype for IK_StripDatasetKey().
 *
 * Revision 1.1  1992/09/10  15:24:34  honce
 * Initial revision
 *
*/

/*****************************************************************************/

/* Prevent this header file from being #included more than once per
** source file. */

#ifndef __IK_DATASET_H__
#define __IK_DATASET_H__

/*****************************************************************************/

/* #include directives */

/*---------------------------------------------------------------------------*/

/* IMS header files */
#include "IK_Coverag.h"
#include "IK_DataDic.h"
#include "IK_Ims.h"
#include "IK_Mark.h"

/*****************************************************************************/

/* #define directives */

/*---------------------------------------------------------------------------*/

/* This macro constant defines the length of a dataset key. Note that
*since * this is a macro, it can be placed _before_ the definition of
*the * IK_DatasetTag structure. The IK_ArchiveTag type is defined in
*IK_Ims.h. */
#define IK_DATASET_KEY_SZ (sizeof(IK_ArchiveTag) + sizeof(IK_DatasetTag))
#define ACCOUNT_NUMBER_MAX_LEN (80+1)
#define VALID_ACCOUNTS_MAX_NUM (10)
#define BALANCE_STRING_MAX_LEN (16+1)
 
/*****************************************************************************/

/* Data type definitions */

/*---------------------------------------------------------------------------*/

/* Define the dataset tag data type (just a string). The size is
   defined in IK_Ims.h. */
typedef unsigned char IK_DatasetTag[IK_DATASET_TAG_SZ];

/*---------------------------------------------------------------------------*/
/* Define the dataset status structure. */

struct IK_DatasetStatus {
  unsigned is_visible : 1; /* Should dataset be shown? */
};
typedef struct IK_DatasetStatus IK_DatasetStatus;

/*---------------------------------------------------------------------------*/
/* Defining the Valid Account structure. */

typedef struct ValidAccount {
  char accountNumber[ACCOUNT_NUMBER_MAX_LEN];
  char accountBalance[BALANCE_STRING_MAX_LEN];
  char **errorMsg;
} ValidAccounts;

/*---------------------------------------------------------------------------*/

/* Define the IK_Dataset structure. */
struct IK_Dataset {

  /* IMS's Data Center tag */
  IK_ArchiveTag archive_tag;

  /* IMS's Dataset tag */
  IK_DatasetTag dataset_tag;

  /* IMS's GCMD Entry tag */
  char md_entry[IK_MD_ENTRY_ID_SZ];

  /* Inventory search message id */
  char srch_msg_id[IK_MESSAGE_ID_SZ];

  /* Dataset's coverage from GCMD */
  IK_Coverage area;

  /* Any dataset restrictions */
  char **restriction;

  /* Dataset comments */
  char **comment;

  /* Dataset browse description */
  char **browse_desc;

  /* Fields not mapped during search */
  char **unmapped_field;

  /* Start time from GCMD */
  char *md_start_time;

  /* Stop time from GCMD */
  char *md_stop_time;

  /* Title from GCMD DIF on dataset */
  char *md_title;

  /* Brief from GCMD DIF on dataset */
  char *md_brief;

  /* Dataset attributes from GCMD DIF */
  char *md_attribute;

  /* Bibliographic references from GCMD DIF */
  char *md_reference;

  /* Personnel w/respect to DIF */
  char *md_personnel;

  /* Associated data center information */
  char *md_archive_info;

  /* Originating data center for dataset */
  char org_center[IK_ORG_CENTER_SZ];

  /* Number of granules from dataset */
  unsigned int ngranules;

  /* Dataset status */
  IK_DatasetStatus marked;

  /* Valid Accounts for the dataset */
  ValidAccounts val_accts[VALID_ACCOUNTS_MAX_NUM];

  /* The real number of VALID_ACCOUNT groups sent for this dataset*/
  int ValAcctCount; 
  
};
typedef struct IK_Dataset IK_Dataset;

/*****************************************************************************/

/* Dataset-related function prototypes */

/* NOTE - Functions defined in IC source files really should not have
   prototypes in an IK header like this one! */

/* These functions are defined in IC_ClrDset.c. */
extern int IC_ResetDatasetStatus(void);

/* These functions are defined in IC_GetKeys.c. */
extern int IC_GetDatasetKeys(IK_SortKey **list);

/* These functions are defined in IK_Dataset.c. */
extern int IK_DirClose(void);
#ifdef BRIDGE_CONFIG
extern int IK_DirInit(char *);
#else
extern int IK_DirInit(void);
#endif
extern IK_SortKey *IK_FirstDatasetKey(void);
extern int IK_GetDataset(IK_SortKey *key, IK_Dataset **ds);
extern IK_SortKey *IK_NextDatasetKey(void);
extern int IK_PutDataset(IK_Dataset *ds);

/* These functions are defined in IK_Key.c. */
extern void IK_StripDatasetKey(IK_ArchiveTag archive, IK_DatasetTag dataset,
 IK_SortKey *key);
extern IK_SortKey *IK_BuildDatasetKey(IK_ArchiveTag archive,
 IK_DatasetTag dataset);

/* These functions are defined in IK_Map.c. */
extern void *IK_MapDatasetIdent(void *ident, unsigned int size);

/* These functions are defined in IK_Mark.c. */
extern int IK_MarkDataset(IK_SortKey *key, IK_Mark field, unsigned mode);
extern int IK_StatDataset(IK_SortKey *key, IK_DatasetStatus *status);

/*****************************************************************************/

#endif	/* !__IK_DATASET_H__ */
