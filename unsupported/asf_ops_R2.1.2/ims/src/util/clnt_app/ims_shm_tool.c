static char *sccs = "@(#)ims_shm_tool.c	5.1  03/17/96";
/******************************************************************************
**
** File:	ims_shm_tool.c
**
** Function: Shared memory tool. 
**
** Author: Dan Crichton	
**
** Date:	6/21/95
**
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
** These are needed by ipc.h           
*/

typedef unsigned long   ulong;
typedef unsigned short   ushort;

#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/wait.h>

#include <ims_shm.h>

void main(int argc, char *argv[])
{

	(void) ims_dumpPoolTable();
}

