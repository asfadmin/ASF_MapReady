/******************************************************************************
**
** File:        ims_signal.h
**
** Function:    This file includes prototypes for the ims_signal.c module.
**
** Date:        8/17/94
**
******************************************************************************/

#ifndef _IMS_SIGNAL_H
#define _IMS_SIGNAL_H

static char *sccsSignal = "@(#)ims_signal.h	5.1  03/17/96";

/*
** Function Prototypes for the ims_signal.c module.
*/
int ims_setWrapup (int (*) (int));
int ims_blockSigs (void);
int ims_unBlockSigs (void);
char *ims_sigMsg (int);

#endif	/* !_IMS_SIGNAL_H */
