/******************************************************************************
FUNCTION:	c_egroup

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0		    T.Butzer		Original development

  2.0	   12/87    B.Ailts		Placed bridge routines in a seperate 
					file
					Used raw 'C' types
					Renamed egroup to c_egroup

  2.1	   04/88    D.Hollaren		Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg

  2.2	   05/88    B.Ailts		Replace errh with c_errmsg
 					Removed eerror.h
  2.3	   08/88    B.Ailts		Checked status of malloc call
					Added check when max_line_size is 
					greater than the vms i/o limit of 65535
  2.4	   09/88    B.Ailts		Fix problem with downward conversion of
					data when writing to disk
  2.5      09/88    B.Ailts		Fix problem with vms erroring out when
					converting an image with a large number
					of samples to 4 byte data
  2.6      01/90    B.Davis		Check for vmsmaxbytes separately, it
					used to get changed only inside of a
					check for maxbytes.
  2.7      01/90    B.Ailts 		Updated check for mvsmaxbytes to divide
					by two if UPDATE access.
  2.8      02/91    B. Ailts		Standardized error messages


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT			LAS

ALGORITHM 
Allocate the space needed to hold the group descriptor structure
Initialize the group descriptor members
Calculate the maximum line size needed
Allocate the buffer space needed

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "las.h"
#include "locinc.h"

lasErr FUNCTION c_egroup(struct FDESC *fdarray[], struct GDESC **gdesc, unsigned char *buffer, int *buffersz)
{
short i = 0;			/* counter				*/
struct GDESC *gd;		/* pointer to the group descriptor	*/
int maxbytes = 0;		/* maximum number of bytes for one line	*/
int nbytes;			/* number of bytes for one line		*/
int status;			/* status of function calls		*/
int total_bands = 0;		/* total number of bands to be grouped	*/
int maxnbuf = 0;
#ifdef vms
int vmsmaxbytes = 0;		/* vms maximum number of bytes for one line*/
int vmsnbytes;			/* vms number of bytes for one line	*/
#endif

char errtxt[ERRLEN];		/* error message buffer			*/

*gdesc = (struct GDESC *)MALLOC(sizeof(struct GDESC));
gd = *gdesc;
gd->nl = fdarray[0]->nl;
gd->nfiles = 0;
gd->thebuf = buffer;
gd->thebufsz = *buffersz;
gd->the_nxt_space = buffer;
gd->gbl_nlines = 0;
while (fdarray[i] != 0)
   {
   gd->fdarray[i] = fdarray[i];
   gd->nfiles++;
   if (fdarray[i]->nl != gd->nl)
      {
      sprintf(errtxt,"Number of lines are not the same for %s as group %d",
	      fdarray[i]->fname, fdarray[i]->nl);
      c_errmsg(errtxt,"egroup-oflines",NON_FATAL);
      return(E_FAIL);
      }
   total_bands = total_bands + fdarray[i]->nbands;
/* i can't figure out why i did this *TONY* */
/*
   if (fdarray[i]->acc == IREAD)
	nbytes = fdarray[i]->linsiz;
   else
	{
	if (fdarray[i]->cnvlsz != 0)
		nbytes = fdarray[i]->cnvlsz;
	else
		nbytes = fdarray[i]->linsiz;
	}
*/
/* this should be sufficient */
#ifdef vms
   nbytes = fdarray[i]->linsiz;
   if (fdarray[i]->cnvlsz != 0)
      vmsnbytes = fdarray[i]->cnvlsz;
   else
      vmsnbytes = fdarray[i]->linsiz;
   if (nbytes > maxbytes)
      maxbytes = nbytes;
   if (vmsnbytes > vmsmaxbytes)				/* change 2.6	*/
      vmsmaxbytes = vmsnbytes;
   if (fdarray[i]->nbuf > maxnbuf)
      maxnbuf = fdarray[i]->nbuf;

   /*The VMS machines cannot read or write more than bytes at one time, but if
     if the mode is update only half the lines will be written at once
   ---------------------------------------------------------------------------*/
   if (fdarray[i]->acc == IUPDATE)
      vmsmaxbytes /= 2;
#else
   nbytes = fdarray[i]->linsiz;
   if (nbytes > maxbytes)
      maxbytes = nbytes;
   if (fdarray[i]->nbuf > maxnbuf)
      maxnbuf = fdarray[i]->nbuf;
#endif
   i++;
   }
gd->gbl_nlines = gd->thebufsz / (maxbytes * total_bands);
/* test for restrictions on vms */
/* tae xireads and xiwrits call qio's */
/* these can transfer only 65535 bytes in one operation the xi's should */
/* do mutiple qio's but they don't */ 
#ifdef vms
if ((gd->gbl_nlines * vmsmaxbytes) > 65535)
   {
   gd->gbl_nlines = 65535 / vmsmaxbytes;
   if (gd->gbl_nlines < 1)
      {
      c_errmsg("Line size is beyond the 65535 byte i/o limit of VMS",
	       "egroup-vmsi/o",NON_FATAL);

      return(E_FAIL);
      }
   }
#endif
for (i=0; fdarray[i] !=0; i++)
        {
	status = allocate_buffer(fdarray[i],gd);
        if (status != E_SUCC)
	   return((lasErr)status);
         }  /*  for i until fdarray[i] == 0  */

gd->fdarray[i] = 0;
gd->line_cnt = 0;
return(E_SUCC);
}
