static char *sccs = "@(#)ims_srvMutex.c	5.1  03/18/96";
/******************************************************************************
**
** File:        ims_srvMutex.c
**
** Function:    This file contains functions for locking and unlocking
**              the mutex along with changing priority of the thread.
**
** Author:      Hoshyar Sayah
**
** Date:        10/90
**
** Modified:    8/19/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/9/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
*****************************************************************************/

#include <stdio.h>

#include <ims_dbms.h>
#include <ospublic.h>
#include <oserror.h>

#include <ims_const.h>
#include <ims_msg.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_ftsSrv.h.
** They are listed here for reference.
**
**  int ims_changePriority (SRV_PROC *, int);
**  int ims_lockMutexWait (SRV_OBJID *);
**	int ims_lockMutexNoWait (SRV_OBJID *);
**  int ims_unlockMutex (SRV_OBJID *);
*/

/******************************************************************************
**
** ims_changePriority ()
**
** Change the client thread priority by the delta value.
**
******************************************************************************/

int ims_changePriority (
	SRV_PROC *srvproc,
	int delta)
{
	if (srv_setpri (srvproc, SRV_C_DELTAPRI, (CS_INT) delta) == CS_FAIL)
	{
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_lockMutexWait ()
**
******************************************************************************/

int ims_lockMutexWait (
	SRV_OBJID *mutex)
{
	CS_INT info;

	if (srv_lockmutex (*mutex, SRV_M_WAIT, &info) == CS_FAIL)
	{
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_lockMutexNoWait ()
**
******************************************************************************/

int ims_lockMutexNoWait (
	SRV_OBJID *mutex)
{
	CS_INT info;
	int gotIt;
	int counter;

	gotIt = IMS_FALSE;
	counter = 0;

	while (! gotIt)
	{
		if (srv_lockmutex (*mutex, SRV_M_NOWAIT, &info) == CS_FAIL)
		{
			if (info == (CS_INT) NULL)
			{
				return (IMS_FATAL);
			}
			(CS_VOID) srv_yield ();

			/*
			** Cannot wait any longer
			*/
			counter++;
			if (counter == 200)
			{
				return (IMS_FATAL); 
			}
		}
		else
		{
			gotIt = IMS_TRUE;
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_unlockMutex ()
**
******************************************************************************/

int ims_unlockMutex (
	SRV_OBJID *mutex)
{
	if (srv_unlockmutex (*mutex) == CS_FAIL)
	{
		return (IMS_FATAL);
	}

	return (IMS_OK);
}
