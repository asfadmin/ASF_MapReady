/******************************************************************************
**
** File:        ims_util.h
**
** Function:    This file includes function prototypes for the
**              ims_util.c file.
**
** Date:        9/23/94
**
******************************************************************************/

#ifndef _IMS_UTIL_H
#define _IMS_UTIL_H

static char *sccsUtil = "@(#)ims_util.h	5.1  03/16/96";

/*
** Function Prototypes for the ims_util.c module.
*/
char *ims_truncStr (char *);
char *ims_trim (char []);
char *ims_saveStr (char *);
char *ims_timeStamp (void);
char *ims_extractFileName (char *);
int ims_extractPath (char *, char *);
void ims_concatFilePath (char *, char *, char *);
int ims_isInteger (char *);
int ims_isReal (char *);
void ims_formatQuotedString (char *, char *);
char *ims_removeQuotes (char *);
int ims_isNullStr (char *);
int ims_bCmp (unsigned char *, unsigned char *, int);
char *ims_toUpper (char *);
char *ims_toLower (char *);
long ims_strIndex (char [], char []);
char *ims_ltoa (long);
int ims_isBlankString (char *);
int ims_nano2msecs (long);

#endif	/* !_IMS_UTIL_H */
