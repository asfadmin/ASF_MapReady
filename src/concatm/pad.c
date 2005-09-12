/*******************************************************************************
NAME:      	 PAD

PURPOSE:   	To pad the output image with the fill value

PROGRAM HISTORY:
PROGRAMMER	  DATE		REASON
----------	  ----		------
B. Wilsey      Aug   1984	Original development
D. Akkerman    Aug   1987       PR #4247 modified to allow for single band 
				 specification of a multi-band images
K. Zanter      Sept. l987       NEWLAS (conversion to 'C')
B. Ailts       July  1988       LAS5.0 conversion -- added -AUTO subcommand
T. Mittan      Jan   1992       Improved performance by only pixcopying to a 
				buffer once
T. Logan       Apr   1995       Removed TAE dependencies (ASF) --
				  removal of TAE PARBLK variables
				  removal of timer message routines
				  removal of c_clean calls

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:        LAS		                

ALGORITHM DESCRIPTION:
Initialization 
Switch on dtype -- EBYTE, EWORD, ELONG, or EREAL
   Type cast mask value to a temporary pointer
   Set correct data type size
Open image for writing
Allocate space for image I/O buffer
Set up file group
Do initial read
Set up and start timed processing message
    For each line
       For each band
          Fill buffer with mask value
       Write each image 
Close images
Free buffer
Return

ALGORITHM REFERENCES:     none
*******************************************************************************/
#include "asf.h"


#include "las.h"
#include "locinc.h"
#include "concat.h"

void pad(hostout,nl,ns,totbnd,dtype,mask)
   char  *hostout; 		 /* pointer to output host file descriptor    */
   float *mask;			 /* pointer to mask value		      */
   int  *dtype;		 /* pointer to data type of output image      */
   int  *nl;			 /* pointer to number of lines in output image*/
   int  *ns;			 /* pointer to number of samples per line     */
   int  *totbnd;		 /* pointer to number of bands in output image*/
{
float	tmask;			 /* tempory buffer to mask value	      */
int	access = IWRITE;	 /* image I/O access type		      */
int	band;			 /* band counter			      */
int    curent;		         /* current line			      */
int	*lmask;			 /* LONG mask value			      */
int	one = 1;		 /* value of one		      	      */
int	size;			 /* number of bytes per sample		      */
/*int    status;*/			 /* function return status code		      */
int    opt[3];
short	*smask;			 /* SHORT mask value			      */
unsigned char	*bmask;		 /* BYTE mask value			      */
char    *thebuf;  		 /* pointer to image I/O buffer		      */

/*  Initialize the pointer to the temporary mask value
------------------------------------------------------*/
bmask = (unsigned char *)&tmask;
smask = (short *)&tmask;
lmask = (int *)&tmask;

/* setup based on data type 
---------------------------*/
switch (*dtype)
   {
   case EBYTE:  *bmask = (unsigned char) *mask;
		size = 1;
		break;
   case EWORD:  *smask = (short) *mask;
		size = 2;
		break;
   case ELONG:  *lmask = (int) *mask;
		size = 4;
		break;
   case EREAL:  size = 4;
		tmask = *mask; 
		break;
   }
/* allocate space for image I/O buffer
--------------------------------------*/
thebuf = (char *)calloc(*ns,(size * sizeof(char)));
if ( thebuf == NULL)
   {
   c_errmsg("Error allocating dynamic memory","concat-alloc",NON_FATAL);
   /* c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
   }

/* open output image to full size
---------------------------------*/
opt[0] = opt [1] = opt[2] = 0;
fdesc[1] = 0;
c_eopenr(&fdesc[0],hostout,totbnd,&one,&one,nl,ns,
   	          &access,dtype,opt,&one);
/* if (status != E_SUCC) c_clean(vblk,"concat",hostout,&one); */ /*ASF*/

/* Fill the buffer with the proper value
--------------------------------------*/
c_pxfill((unsigned char *)thebuf,dtype,ns,(unsigned char *)&tmask);

/* process padded image 
------------------------*/
for (curent = 1; curent <= *nl; curent++)
    for (band = 1; band <= *totbnd; band++)
    if (c_ewrite(&fdesc[0],&band,&curent,(unsigned char *)thebuf,&one) != *ns)
       {
       c_eclose(&fdesc[0]);
       /* c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
       }

/* close image
--------------*/
if (c_eclose(&fdesc[0]) != E_SUCC)
   {
   c_errmsg("Error closing file","concat-close",NON_FATAL);
   c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
   /* c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
   }

/* free buffer
--------------*/
free(thebuf);

return;
}
