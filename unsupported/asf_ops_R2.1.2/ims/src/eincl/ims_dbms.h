/******************************************************************************
**
** File:        ims_dbms.h
**
** Function:    Include files required to build interface using target DBMS.
**
** Date:        10/4/90
**
** Modified:    2/24/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the __STDC__ conditional statement to enable the use of
**              ANSI function prototypes for Sybase calls.
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              11/6/96 - S. Hardman - R2.1
**              Added ctpublic.h for CT-Library inplementation.
**
** Note:        Includes files for building Sybase only.
**
******************************************************************************/

#ifndef _IMS_DBMS_H
#define _IMS_DBMS_H

static char *sccsDbms = "@(#)ims_dbms.h	5.2  03/13/97";

#include <sybfront.h>
#include <ctpublic.h>

#ifdef __STDC__
#define COMPILE_STYLE ANSI_C_COMPILE
#endif  /* __STDC__ */

#include <sybdb.h>

#endif	/* !_IMS_DBMS_H */
