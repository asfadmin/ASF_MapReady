/******************************************************************************
FUNCTION:	c_ewrite

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	            T.Butzer		Original Development
  2.0	   12/87    B. Ailts		Placed bridge routinue in a separate 
					file
					Changed include file specifications
					Renamed fron ewrite to c_ewrite
  2.1      04/88    D.Hollaren		Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg
  2.2      05/88    B. Ailts		Replaced eerh with c_errmsg
					Deleted eerror.h
  2.3	   08/88    L. Huewe		Added argument for number of lines to
					be read at one time
  2.4      09/88    L. Huewe		Fixed the calling sequence to i_read
  2.5	   10/88    B. Ailts		Do not set cbufptr unless needed
  2.6      12/88    B. Ailts		Output an error message if values are
					clipped
  2.7 	    2/89    B. Ailts		Replaced the 'ceil' function -- did not
					work properly
  2.8       5/90    B. Ailts            Fixed a problem on VMS with the calc. of
					the index variable
  2.9      10/90    T. Baltzer          Rewrite so IUPDATE mode can convert data
  3.0      02/91    B. Ailts		Standardized error messages
  3.1      05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  3.2	   10/92    T. Mittan		Added compression option
  3.3	   4/93	    T. Mittan		Corrected problem when multiple lines
					are written in compressed mode.
	    1/94    K.Gacke		Modified to read/write dal images.  TAE
					images can be read, but all images are
					written as a BSQ DAL image.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
   
ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"


#include <unistd.h>
#include "las.h"
#include "locinc.h"
#include "pixman.h"


int FUNCTION write_dal(struct FDESC *fd, int *eband, int *eline, const unsigned char *euserbuf, int *nlines);


int FUNCTION c_ewrite(struct FDESC **fdesc,int * eband, int *eline, const unsigned char *euserbuf,int * nlines)
{
struct FDESC *fd;
int count1;
int line;
int status;
int i,j;
int nwrites;
int numlines;
int index;
int lsize;
int one = 1;
int conv_type;

static short big_first = TRUE;
static short small_first = TRUE;

char errtxt[ERRLEN];
unsigned char *cbufptr;
const unsigned char *ubufptr;

fd = *fdesc;
if (fd->dal)
    {
    status = write_dal(fd, eband, eline, euserbuf, nlines);
    return(status);
    }
/*
* Process Compress Image
*/
line = *eline + fd->sl - 1;
if (fd->flags & CONVERSION) 
    lsize = fd->cnvlsz;
else
    lsize = fd->linsiz;
	
nwrites = 1; 
numlines = *nlines; 
index = 0;
#ifdef vms 
numlines = MAXBUF / lsize;
nwrites = *nlines / numlines; 
if (*nlines % numlines != 0)
    ++nwrites;
#endif
for (i=0; i < nwrites; i++)
    {
#ifdef vms
    index = i * (numlines * fd->ns * datasize[fd->dtype]);
    line = i * numlines + (*eline + fd->sl - 1);
    if (i == (nwrites - 1))
	numlines = *nlines - (numlines * i);
#endif

    if ((fd->flags & CONVERSION) || (fd->flags & MOVEDATA) ||
	(fd->acc == IUPDATE))
        {
    	if (fd->acc == IUPDATE)
    	    { 
	    strcpy(errtxt,
		"Image update access not allowed in Compression mode");
	    c_errmsg(errtxt,"ewrite",NON_FATAL);
	    return(E_FAIL);
	    }
	else
    	    { 
	    cbufptr = fd->conv_buf;
	    }
	ubufptr = euserbuf+index;
	for (j=0; j < numlines; j++)
	    {
	    if (fd->flags & CONVERSION)
	        {
	        status = c_pxconv(fd->dtype, fd->conv_type, ubufptr,
			 	  cbufptr, fd->ns);
		if (status != E_SUCC)
		    {
		    if (((status & TOOBIG) == TOOBIG) && (big_first))
		        {
		        sprintf(errtxt,"%s%s","Pixel values were larger than",
			        " maximum allowed by output data type");
		        c_errmsg(errtxt,"ewrite-pixel",NON_FATAL);
		        big_first = FALSE;
		        }
		    else if (((status & TOOSMALL) == TOOSMALL) && 
		 	     (small_first))
		        {
		        sprintf(errtxt,"%s%s","Pixel values were smaller than",
			        " minimum allowed by output data type");
		        c_errmsg(errtxt,"ewrite-pixel",NON_FATAL);
		        small_first = FALSE;
		        }
		    }
	        }
	    else
	        {
	        c_pxcopy(ubufptr,cbufptr,fd->dtype,fd->ns);
	        }
            if ( fd->compres == TRUE)
               {
               fseek(fd->fp,0,2);
               fd->f_pointer[((line + j - 1)*fd->nbands)+ (*eband - 1)] = 
				ftell(fd->fp);
               if (fd->conv_type == 0)
	    	    conv_type = fd->dtype;
	       else
		    conv_type = fd->conv_type; 
               encode_76(cbufptr,fd->tempbuf,fd->ns, &count1,
                         datasize[conv_type]);
               fwrite(&count1,sizeof(int), one,fd->fp);
               fwrite(fd->tempbuf,sizeof(char),count1,fd->fp);
               }
	    cbufptr += lsize;
	    ubufptr += (fd->ns * datasize[fd->dtype]);
	    }
	}
    else
        {
        if ( fd->compres == TRUE)
           {
	   for (j=0; j < numlines; j++)
	     {
             index += j * (fd->ns * datasize[fd->dtype]);
             fseek(fd->fp,0,2);
             fd->f_pointer[((line + j - 1) * fd->nbands) + (*eband -1)] = 
				ftell(fd->fp);
             if (fd->conv_type == 0)
	  	conv_type = fd->dtype;
	     else
		conv_type = fd->conv_type; 
             encode_76(&euserbuf[index],fd->tempbuf,fd->ns, &count1,
                        datasize[conv_type]);
             fwrite(&count1,sizeof(int), one,fd->fp);
             fwrite(fd->tempbuf,sizeof(char),count1,fd->fp);
	     }
           }
        }
#ifdef DEBUG
printf("write line = %d nlines = %d\n", line, *nlines);
#endif
    }
return(fd->ns * (*nlines));
}

int FUNCTION write_dal(struct FDESC *fd, int *eband, int *eline, const unsigned char *euserbuf, int *nlines)
{
static short big_first = TRUE;
static short small_first = TRUE;
int dsize;
int line;
int lsize;
int nbyte;
int status;
unsigned int newpos;
unsigned char *cbufptr;
const unsigned char *ubufptr;
char errtxt[ERRLEN];

#ifdef DEBUG
printf("write line = %d nlines = %d\n", line, *nlines);
#endif
lsize = (fd->flags & CONVERSION) ? fd->cnvlsz : fd->linsiz;
dsize = (fd->flags & CONVERSION) ? datasize[fd->conv_type]:datasize[fd->dtype];
if ((fd->flags & CONVERSION) || (fd->flags & MOVEDATA) || (fd->acc == IUPDATE))
    {
    ubufptr = euserbuf;
    for (line=0; line<*nlines; line++)
	{
		cbufptr = (unsigned char *)ubufptr;
        if (fd->flags & CONVERSION)
	    {
	    cbufptr = fd->conv_buf;
	    status=c_pxconv(fd->dtype,fd->conv_type,ubufptr,cbufptr,fd->ns);
	    if (status != E_SUCC)
		{
		if (((status & TOOBIG) == TOOBIG) && (big_first))
		    {
		    sprintf(errtxt,"%s%s","Pixel values were larger than",
			        " maximum allowed by output data type");
		    c_errmsg(errtxt,"ewrite-pixel",NON_FATAL);
		    big_first = FALSE;
		    }
		else if (((status & TOOSMALL) == TOOSMALL) && (small_first))
		    {
		    sprintf(errtxt,"%s%s","Pixel values were smaller than",
			        " minimum allowed by output data type");
		    c_errmsg(errtxt,"ewrite-pixel",NON_FATAL);
		    small_first = FALSE;
		    }
		}
	    }
/*
*  Need to seek in case a window was specified.
*/
        newpos = ((*eband - 1) * fd->img_nl * lsize) +
	        ((*eline + fd->sl + line - 2) * lsize) + 
	        ((fd->ss - 1) * dsize);
	if (lseek(fd->dalfd,newpos,0) < 0)
	    {
	    perror("c_ewrite-seek");
            c_errmsg("error seeking dal file","ewrite",NON_FATAL);
            return(E_FAIL);
	    }
	nbyte = fd->ns * dsize;
	status = write(fd->dalfd,(const char *)cbufptr,nbyte);
	if (status < nbyte)
            {
	    perror("c_ewrite-write");
            c_errmsg("error writing dal file","ewrite",NON_FATAL);
            return(E_FAIL);
            }
	ubufptr += (fd->ns * fd->dtype);
	}
    }
else
    {
    ubufptr = euserbuf;
    newpos = ((*eband - 1) * fd->img_nl * lsize) +
	    ((*eline + fd->sl - 2) * lsize) + 
	    ((fd->ss - 1) * datasize[fd->dtype]);
    if (lseek(fd->dalfd,newpos,0) < 0)
	{
	perror("c_ewrite-seek");
        c_errmsg("error seeking dal file","ewrite",NON_FATAL);
        return(E_FAIL);
	}
    nbyte = lsize * *nlines;
    status = write(fd->dalfd,(const char *)ubufptr,nbyte);
    if (status < nbyte)
        {
	perror("c_ewrite-write");
        c_errmsg("error writing dal file","ewrite",NON_FATAL);
        return(E_FAIL);
        }
    }
return(fd->ns * (*nlines));
}
