static char *sccs = "@(#)ims_passlog_reader.c	1.2  04/25/97";
/******************************************************************************
**
** File:        ims_passlog_reader.c
**
** Function:    A utility to extract lock counts and lock times from
**              an 11 Meter Passlog file.
**
** Author:      Vance Heron
**
** Date:        7/9/96
** Modification: D. Ting
**               R2.1 Added status 4/25/97
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
******************************************************************************/

#include <stdio.h>
#include <math.h>

#include <ims_passlog.h>

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (
	int argc,
	char **argv)
{
int stat, lock_cnt, unlock_cnt;
char fname[100];
char start_tm_str[40], end_tm_str[40],
     lock_strt_str[40], lock_end_str[40];
int  status;/*R2.1*/
char msg[20];/*R2.1*/

int chan;

/* default values */
   start_tm_str[0] = end_tm_str[0] = '\0';
   chan = -1;

/* calling params ... fname, chan, start_time, end_time */
   switch (argc)
   {
   case 5:
      strcpy(end_tm_str, argv[4]);
      /* FALL THROUGH */
   case 4:
      strcpy(start_tm_str, argv[3]);
      /* FALL THROUGH */
   case 3:
      chan=atoi(argv[2]);
      /* FALL THROUGH */
   case 2:
      strcpy(fname,argv[1]);
      break;
   default:	/* argc < 2 || argc > 5 */
      printf ("usage: %s <filename> [[[chan] start_tm] end_tm] \n",argv[0]);
      exit(0);
   }

   stat = ims11m_counts(fname, chan, start_tm_str, end_tm_str, 
      &lock_cnt, &unlock_cnt, &status );

   stat = ims11m_times(fname, chan, start_tm_str, end_tm_str, 
      lock_strt_str, lock_end_str);

   switch (status)
	 {
	 case IMS_DLCOMP:
				strcpy(msg, "Downlink complete");
        break;

   case IMS_DLPART:
				strcpy(msg, "Downlink partial");
        break;

   case IMS_DLFAIL:
				strcpy(msg, "Downlink fail");
        break;

   default:
				strcpy(msg, "Downlink unknown");
	 }

   (void) printf ("Lock Count '%d',  Unlock Count '%d', Status '%s'\n",
      lock_cnt, unlock_cnt, msg); /*R2.1*/
   (void) printf ("Lock Start '%s',  Lock End '%s'\n",
			lock_strt_str, lock_end_str);
}
