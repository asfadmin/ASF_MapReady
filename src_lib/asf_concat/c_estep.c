/******************************************************************************
FUNCTION:	c_estep

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0      	    T.Butzer		Original Development
  2.0      12/87    B.Ailts		Used raw 'C' types
					Placed brigde routines in a seperate
					file
					Renamed from estep to c_estep
  2.1      04/88    D.Holleran	 	Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg
  2.2	   05/88    B.Ailts		Replaced errh with c_errmsg
					Replace CHKSTAT with 'if (status...'
  2.3      02/91    B. Ailts		Standardized error messages
  7.0	    4/95    T. Logan (ASF)      Removed TAE dependencies

					
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT			LAS

ALGORITHM 
Do 
   Do for number of files
      Do for number of bands
          Process a line
While not done
Do for number of files
   If READ access and CONVERSIOn
      Wait for any outstanding I/Os

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "locinc.h"

#define NLINES gd->gbl_nlines

lasErr FUNCTION c_estep(struct GDESC **gdesc)
{
register struct GDESC *gd;

int band_cnt;
int done;
register int i;
int status;

gd = *gdesc;

band_cnt = 0;
	
/* clear all done flags */
for (i=0; i < gd->nfiles; i++)
	gd->fdarray[i]->flags &= ~EDONE;

done = gd->nfiles;

do 
	{
	for (i=0; i < gd->nfiles; i++)
		{
		if (band_cnt < gd->fdarray[i]->nbands)
			{
			status = process_it(gd->fdarray[i],
			 gd->line_cnt, band_cnt, gd);
			if (status != E_SUCC)
			   return((lasErr)status);
			}
		else
                 {
			if (!(gd->fdarray[i]->flags & EDONE))
				{
				done--;
				gd->fdarray[i]->flags |= EDONE;
				}
                 }
		}
		band_cnt++;
	} 
while (done != 0);

gd->line_cnt++;

/* wait for any outstanding i/o's */
/*

for (i=0; i < gd->nfiles; i++)
	{
	if (!((gd->fdarray[i]->acc == IREAD)
		 && (gd->fdarray[i]->flags & CONVERSION)))
		{
		if (gd->fdarray[i]->compres != TRUE)
                {
		status = i_wait(&(gd->fdarray[i]->ifcb));
		if (status != SUCCESS)
			if ((gd->line_cnt + NLINES) 
			     < gd->fdarray[i]->nl)
			   {
			   printf(errtxt,"Xiwait error encountered for file %s",
			          gd->fdarray[i]->fname);
			   c_errmsg(errtxt,"estep-xiwait",NON_FATAL);
			   return(E_FAIL);
			   }
                }
		}
	}
*/   /*ASF*/
return(E_SUCC);
}
