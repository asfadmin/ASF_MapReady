/*
 * Name: IK_DataDic.h
 *
 *
 * Description: This file contains data structures and manifest
 * constants for atomic items used in the Gaea application.
 * 
 * Notes:
 * 
 * 1) Higher level items are defined in separate files.
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
 * Revision 5.0.1.2  1995/11/06  13:12:09  ims
 * COPIED FROM 4.5.1.5 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.5  1995/11/03  21:39:34  iredell
 * changed IK_SENSOR_ID_SZ and IK_SOURCE_ID_SZ to 81
 *
 * Revision 4.5.1.3  1995/10/25  18:55:42  dandi
 * Changed the maximum size of IMAGE_ID to 50. SMR 3209.
 *
 * Revision 4.5.1.2  1995/08/31  19:50:13  shiva
 * Defined IK_BILL_ID_SZ (80 +1) as the size of billing_id.
 *
 * Revision 4.5.1.1  1995/08/31  19:48:06  shiva
 * Place holder for 4.5.1 branch.
 *
 * Revision 4.5  1995/07/27  18:41:54  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:19:47  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:49:12  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.2  1994/07/15  16:02:26  heather
 * changed IK_MD_ENTRY_ID_SZ to 32 from 31
 *
 * Revision 4.0.1.1  1994/07/12  19:02:30  dandi
 * Included a new definition for "Additional Info" for Product Request
 *
 * Revision 4.0  1994/06/08  16:54:19  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.2.1.1  1994/05/04  19:06:49  winter
 * COPIED FROM REVISION 3.0.1.7.
 *
 * Revision 3.0.1.7  1994/05/02  11:47:41  winter
 * Added #define IK_ORG_CENTER_SZ (31+1) to define the size of the new
 * org_center member in the IK_Dataset structure.
 *
 * Revision 3.0.1.6  1994/04/25  15:10:23  winter
 * Merged changes from 3.0.1.2->3.0.1.2.1.1 with revision 3.0.1.5. These are
 * changes that Ranga made in order to support product request modifications.
 *
 * Revision 3.0.1.5  1994/04/25  13:31:34  ryan
 * integrated Hu's 3.0.2.1 changes.
 *
 * Revision 3.0.1.4  1994/04/25  13:29:07  ryan
 * LOTS of changes in field lengths as per Bob Harberts docs.
 *
 * Revision 3.0.1.3  1994/04/18  12:52:41  winter
 * Extensive code cleanup by Eric Winter. Alphabetized data dictionary
 * entries. Obsolete code deleted.
 *
 * Revision 3.0.1.2  1994/04/12  14:55:15  winter
 * Leonid changed IK_BROWSE_PRODUCT_DESCRIPTION_SZ from 61 to 81.
 *
 * Revision 3.0.1.1  1994/03/21  22:37:00  winter
 * Added IK_BROWSE_PRODUCT_DESCRIPTION_SZ 81.
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.9  1992/11/05  16:23:21  honce
 * Added IK_DIF_TITLE_SZ. Changed IK_PACKAGE_ID_SZ to IK_GRANULE_ID_SZ.
 * Changed IK_GRANULE_ID_SZ to 51.
 *
 * Revision 1.8  1992/10/21  18:34:45  honce
 * Added IK_GRANULE_LIMIT_DEFAULT
 *
 * Revision 1.7  1992/10/15  21:23:13  honce
 * Added IK_SCREEN_ID_SZ & IK_FIELD_ID_SZ for help subsystem.
 *
 * Revision 1.6  1992/09/29  15:37:51  honce
 * Added IK_MAX_FIELD_SZ.
 *
 * Revision 1.5  1992/09/22  20:27:20  honce
 * typedef IK_ArchiveTag, and prototype IK_MapArchiveIdent() moved to IK_Ims.h
 *
 * Revision 1.4  1992/09/18  18:20:23  honce
 * IK_LONGITUDE_SZ was 9 should have been 10.
 *
 * Revision 1.3  1992/09/11  14:40:11  honce
 * Moved NARCHIVES and NDATASETS to IK_Ims.h where other config'ed stuff is
 *
 * Revision 1.2  1992/09/10  15:40:06  ims
 * Log keyword was not correct.
 *
*/

/*****************************************************************************/

#ifndef __IK_DATADIC_H__
#define __IK_DATADIC_H__

/*****************************************************************************/

/*---------------------------------------------------------------*/
/*--------------------------  May tweek -------------------------*/
/*---------------------------------------------------------------*/

/* IK_NPOLYSIDES is the number of sides on a location Polygon. */
#ifndef IK_NPOLYSIDES
#define IK_NPOLYSIDES (4)
#endif

/*****************************************************************************/

/*---------------------------------------------------------------*/
/*------------  Tweeking below here could hurt you  -------------*/
/*---------------------------------------------------------------*/

/* Define the IK_SortKey structure, which is used everywhere for
   sorting stuff. */

struct IK_SortKey {

  /* key is a gdbm key. */
  void *key;

  /* key_sz is the key size in bytes. */
  unsigned int key_sz;

  /* datum is the sorted key. */
  void *datum;

  /* datum_sz is the sorted key size. */
  unsigned int datum_sz;

  /* User application object 1 */
  void *usrapp1;

};
typedef struct IK_SortKey IK_SortKey;

/*****************************************************************************/

/* Now for some real data dictionary stuff, variable
   attributes. Everything includes a space for NULL / Max length of
   ...  Any additions to this list should be inserted in alphabetical
   order to facilitate visual scanning. */

#define IK_BILL_ID_SZ		(80 + 1) /* Billing ID (Account Number) */
#define IK_ADD_INFO_SZ		(60 + 1)/* Additional Ordering Info. */
#define IK_ADDRESS_SZ           (32*3+1)/* street address (formerly STREET) */
#define IK_AFFILIATION_SZ            (60+1) /* person's affiliation */
#define IK_ARCHIVE_ACRONYM_SZ        (10+1) /* Data Center acronym */
#define IK_AUTHENTICATION_KEY_SZ     (16+1) /* user-supplied key */
#define IK_AUTHENTICATOR_SZ          (16+1) /* encrypted authenticator */
#define IK_BROWSE_PRODUCT_DESCRIPTION_SZ (80+1) /* per line */
#define IK_CAMPAIGN_ID_SZ            (80+1) /* Campaign or Project Name */
#define IK_CATEGORY_SZ               (7+1)  /* user category */
#define IK_CITY_ID_SZ                (30+1) /* City Name */
#define IK_COMMENT_SZ                (60+1) /* per line */
#define IK_DATASET_ID_SZ             (80+1)
#define IK_DATE_SZ                   (10+1) /* 1992-09-09 */
#define IK_DAY_OF_YEAR               (5+1)  /* 09-09 */
#define IK_DIF_TITLE_SZ              (160+1) /* GCMD DIF title */
#define IK_EMAIL_SZ               (128+1) /* person's internet email address */
#define IK_ERR_MSG_SZ                (60+1) /* UI error messages */
#define IK_FAX_NUMBER_SZ             (22+1) /* person's FAX number */
#define IK_FIELD_ID_SZ               (30+1) /* field name used w/ help */
#define IK_GRANULE_ID_SZ             (50+1) /* Granule ID */
#define IK_GRANULE_LIMIT_DEFAULT     (100)  /* # of granules/dataset */
#define IK_GRANULE_LIMIT_SZ          (10+1) /* number of granules/dataset */
#define IK_IMAGE_ID_SZ               (50+1) /* Image ID */
#define IK_IMAGE_SIZE_SZ             (10+1) /* yes, size of image size */
#define IK_INITIAL_USER_KEY_SZ       (12+1) /* host logon key */
#define IK_LATITUDE_SZ               (9)
#define IK_LONGITUDE_SZ              (10)
#define IK_MAX_FIELD_SZ              (128+1)
#define IK_MD_ENTRY_ID_SZ            (31+1) /* Master Directory Entry ID */
#define IK_MEDIA_FORMAT_SZ           (30+1) /* Distributed Media Format name */
#define IK_MEDIA_ID_SZ               (30+1)   /* Distributed Media Name */
#define IK_MESSAGE_ID_SZ             (30+1) /* Communication Message ID */
#define IK_NAME_SZ                   (60+1) /* person's name */
#define IK_NOTICE_LINE_SZ           (60+1) /* notice lines on welcome screen */
#define IK_ORG_CENTER_SZ             (31+1) /* orig. center length for GCMD */
#define IK_ORGANIZATION_SZ           (60+1) /* organization name */
#define IK_PACKAGE_ID_SZ  (IK_GRANULE_ID_SZ) /* Package Name */
#define IK_PACKAGE_SIZE_SZ           (10+1) /* yes, size of package size */
#define IK_PARAMETER_ID_SZ           (80+1) /* parameter */
#define IK_PHONE_NUMBER_SZ           (22+1) /* person's phone number */
#define IK_PROCESS_LEVEL_SZ          (3)    /* Inventory Processing Level ID */
#define IK_PR_OPTION_ID_SZ           (30+1) /* Prod. Reqst. Processing Option*/
#define IK_RESTRICTION_SZ            (60+1) /* per line */
#define IK_SCREEN_ID_SZ              (30+1) /* screen name used w/ help */
#define IK_SENSOR_ID_SZ              (80+1) /* Sensor Name */
#define IK_SOURCE_ID_SZ              (80+1) /* Source Name */
#define IK_STATE_SZ                  (20+1) /* USPostal code */
#define IK_STATUS_CODE_COMMENT_SZ    (60+1) /* per line */
#define IK_STREET_SZ                 (32+1) /* per line */
#define IK_TIME_STAMP_SZ (IK_DATE_SZ + IK_TIME_SZ)
#define IK_TIME_SZ                   (9+1)  /* 16:39:45Z */
#define IK_TYPE_SZ                   (15+1) /* user affiliation type */
#define IK_ZIP_CODE_SZ               (15+1) /* USPostal code */

/*****************************************************************************/

#endif   /* !__IK_DATADIC_H__ */
