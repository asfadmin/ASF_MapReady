/******************************************************************************
**
** File:        ims_getInput.h
**
** Function:    This file includes function prototypes for the
**              ims_getInput.c file.
**
** Date:        9/23/94
**
******************************************************************************/

#ifndef _IMS_GETINPUT_H
#define _IMS_GETINPUT_H

static char *sccsGetInput = "@(#)ims_getInput.h	5.1  16 Mar 1996";

/*
** Function Prototypes for the ims_getInput.c module.
*/
char *ims_getString (int, char *, int, char *);
char *ims_getPassword (char *);

#endif	/* !_IMS_GETINPUT_H */
