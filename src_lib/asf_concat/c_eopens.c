/******************************************************************************
FUNCTION:	c_eopens

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0		    T.Butzer		Original development
  2.0	   12/87    B.Ailts		Placed bridge routines in seperate file
					Renamed from eopens to c_eopens
  2.1	   04/88    D.Hollaren		Replaced newlas.h with las.h
  2.2      05/88    B.Ailts		Replaced errh with c_errmsg
					Changed the calling sequence of the
					DDR routines -- one structure
					Added ddr.h
					Removed eerror.h
					Removed CHKSTAT
  2.3	   08/88    B.Ailts	        Now lets a user specify more bands than
					the image has as long as the band no.
					is within the range of bands
  2.4	   09/88    B.Ailts		filesize is now only checked for READ 
					access  -- not for UPDATE access
  2.5	   01/89    B.Ailts		Now calculates filesize correctly in
					all cases
  2.6      08/89    B.Ailts             Now if zero is specified for bands and
					access = IREAD or IUPDATE all the bands
 				        are specified
  2.7      10/89    B.Davis		Placed the call to lused inside of a
					check for read access.  We don't
					want to change the operating system
					time stamp of the ddr disk file when
					all we are doing is reading an image
					for input.  (for Catalog Manager).
  2.8      09/91    B.Ailts             Standardized error messages
  2.9      05/92    T. Mittan           Check entire filesize not just
                                        requested filesize
  3	   10/92    T.Mittan		Added compression option
  4	   11/93    T.Mittan		Modified compression option to be 
					set by a LAS global	
            1/94    K.Gacke             Modified to read/write dal images. TAE
                                        images can be read, but all images are
                                        written as a BSQ DAL image.



COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT			LAS

ALGORITHM 
If acess equals read
   Initialize the file descriptor members
   Open the image
   Get the ddr information and place in the fdesc structure
   Check the window specifications
   Check the band specifications
Else write access
   Check the band specification
   Check the options argument
   Initialize the DDR fields
   Retrieve the system of the system and place in the DDR system field
   Fill in the DDR fields that are needed
   Write the band indepenant DDR fields
   Initialize the band depenant fields
   Write the band depentant fields
Write the last used date and time to the DDR

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"



#include "las.h"
#include "locinc.h"

int datasize[] = {0,1,2,4,4,8};


lasErr FUNCTION c_eopens(struct FDESC **fdesc,char *fname,int bands[],
	int *sl,int *ss,int *nl,int *ns,int offset[],
	int *acc,int *dtype,int *nbuf,int opt[])
{
struct DDR ddr;
register struct FDESC *fd;

int dflag;
int filesize;
int i;				/* counter -- current band		*/
int nbands;
int stati;
/*int linsiz;*/

char errtxt[ERRLEN];
/*char system[DDSYLN];*/

*fdesc = (struct FDESC *)calloc(1,sizeof(struct FDESC));
if (*fdesc == NULL)
   {
   c_errmsg("Error allocating dynamic memory","eopens-alloc",NON_FATAL);
   return(E_FAIL);
   }
fd = *fdesc;	/* used to simplify expressions */
fd->nl = fd->img_nl = *nl;
fd->ns = fd->img_ns = *ns;
fd->sl = *sl;
fd->ss = *ss;
fd->offset = offset;
fd->acc = *acc;
fd->method = STEP;
fd->dtype = *dtype;
fd->nbuf = *nbuf;
fd->linsiz = (*ns * datasize[*dtype] + 511) & ~0x1ff;
(void)strcpy(fd->fname, fname);
fd->flags = 0; /* clear flags */
fd->cnvlsz = 0; /* set conversion line size 0 */ 

if ((fd->acc == IREAD) || (fd->acc == IUPDATE))
	{	
		
/*  Compare the system that the data is stored on and the system that will
    process the data.
--------------------------------------------------------------------------*/
        stati = c_comsys(fname,&dflag);
	if (stati == E_FAIL)
	   {
	   c_errmsg("Error returned from comsys","eopens-call",NON_FATAL);
	   return(E_FAIL);
	   }
	c_getddr(fname, &ddr);
	if ((dflag == 3) || 
            ((dflag == 1) && (ddr.dtype == EREAL)) ||
	    ((dflag == 2) && (ddr.dtype != EBYTE)))
	   {
	   c_errmsg("Data type of image is not compatible with the processor",
	      "eopens-dtype",NON_FATAL);
	   return(E_FAIL);
	   }

	
        /*linsiz = (ddr.ns * datasize[ddr.dtype] + 511) & ~0x1ff;*/

	/* if a zero was specified for bands all the bands are specified
        ----------------------------------------------------------------*/
	nbands = 0;
        if (bands[0] == 0)
	   {
           for (i = 1; i <= ddr.nbands; i++)
              {
              fd->bands[nbands] = i;
              nbands++;
    	      }
	   }
        else
	   {
	   while (bands[nbands] != 0)
	      {
	      if (bands[nbands] > ddr.nbands)
	         {
	         sprintf(errtxt,"More bands specified than exist for %s",
	                 fd->fname);
	         c_errmsg(errtxt,"eopens-nbands",NON_FATAL);
	         return(E_FAIL);
	         }
	      fd->bands[nbands] = bands[nbands];
	      nbands++;
	      }
	   }
        
	fd->nbands = nbands;

	stati = do_open(fd);	
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from doopen","eopens-call",NON_FATAL);
	   return(E_FAIL);
	   }

	if (*dtype == 0)
		{
		*dtype = ddr.dtype;
		fd->dtype = ddr.dtype;
		}

	if (ddr.dtype != fd->dtype)
		{
		fd->flags |= CONVERSION;
		fd->conv_type = ddr.dtype;
		if ((fd->tae) || (fd->compres))
		    fd->cnvlsz = (ddr.ns * datasize[ddr.dtype] + 511) & ~0x1ff;
		else if (fd->dal)
		    fd->cnvlsz = ddr.ns * datasize[ddr.dtype];
		}
	if (nbands == 0)
		for(i=1; i<=ddr.nbands; i++)
			fd->bands[i-1] = i;

	if (*nl == 0)
	 	{
		*nl = ddr.nl;
		fd->nl = *nl;
	 	}

	if (*ns == 0)
	 	{
		*ns = ddr.ns;
		fd->ns = *ns;
	 	}
        if ((fd->tae) || (fd->compres))
            {
            fd->linsiz = (ddr.ns * datasize[ddr.dtype] + 511) & ~0x1ff;
            /*linsiz = (ddr.ns * datasize[ddr.dtype] + 511) & ~0x1ff;*/
            }
        else if (fd->dal)
            {
            fd->linsiz = ddr.ns * datasize[ddr.dtype];
            /*linsiz = ddr.ns * datasize[ddr.dtype];*/
            fd->img_nl = ddr.nl;
            fd->img_ns = ddr.ns;
	    }
		
	if (check_window(fd, &ddr) != E_SUCC)
	   {
	   sprintf(errtxt,"Invalid window for file %s",fd->fname);
	   c_errmsg(errtxt,"eopens-invalwind",NON_FATAL);
	   return(E_FAIL);
	   }
	}  /*  if (fd->acc == IREAD) || (fd->acc == IUPDATE)  */
else
	{  /*  else fd->acc == IWRITE  */
        fd->compres = fd->tae = FALSE;
        fd->dal = TRUE;
	nbands = bands[0];
	fd->nbands = nbands;
	
	if (nbands < 1)
	   {
	   sprintf(errtxt, "No Bands Specified for file %s",
	           fd->fname);
	   c_errmsg(errtxt,"eopens-nobands",NON_FATAL);
	   return(E_FAIL);
	   }
	else
		{
		for (i=0; i<nbands; i++)
			fd->bands[i] = i + 1;
		}
        
        fd->compres = FALSE;

		if (opt[0] != 0)
		    if ((opt[0] > 0) && (opt[0] < NDTYPES))	
			{
		        if (opt[0] != fd->dtype)
                            {
			    fd->flags |= CONVERSION;
			    fd->conv_type = opt[0];
                            if ((fd->tae) || (fd->compres))
                                fd->cnvlsz = (*ns * 
					datasize[opt[0]] +511) & ~0x1ff;
                            else if (fd->dal)
                                fd->cnvlsz = *ns * datasize[opt[0]];
                            }
		        }
		else
	           {
	           c_errmsg("Invalid option specified","eopens-option",
		            NON_FATAL);
	           return(E_FAIL);
	           }
	if (fd->dal)
	    {
            fd->linsiz = *ns * datasize[*dtype];
	    filesize = fd->linsiz * fd->nl * fd->nbands;
	    if (opt[0] != 0 && opt[0] != 5)
		if (opt[0] != fd->dtype)
		    filesize = fd->cnvlsz * fd->nl * fd->nbands;
	    }
	else
	    {
	    filesize = 11 * 512 + fd->linsiz * fd->nl * fd->nbands;
	    if (opt[0] != 0 && opt[0] != 5)
		if (opt[0] != fd->dtype)
		    filesize = 11 * 512 + fd->cnvlsz * fd->nl * fd->nbands;
	    }
	stati = c_dkcre(fd->fname, filesize);
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from dkcre","eopens-call",NON_FATAL);
	   return(E_FAIL);
	   }
	
	stati = do_open(fd);	
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from doopen","eopens-call", NON_FATAL);
	   return(E_FAIL);
	   }

	c_intddr(&ddr);
	ddr.nl = fd->nl;
	ddr.ns = fd->ns;
	ddr.nbands = fd->nbands;
	if (fd->flags & CONVERSION)
		ddr.dtype = fd->conv_type;
	else
		ddr.dtype = fd->dtype;

	stati = c_putddr(fname, &ddr);	
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from putddr","eopens-call", NON_FATAL);
	   return(E_FAIL);
	   }
 }

#ifdef DEBUG_IMAGEIO
pfd(fd);
#endif
if ((fd->tae) || (fd->compres))
    {
    fd->linsiz = (ddr.ns * datasize[*dtype] + 511) & ~0x1ff;
    fd->file_linsiz = (ddr.ns * datasize[ddr.dtype] + 511) & ~0x1ff;
    }
else if (fd->dal)
    {
    fd->linsiz = ddr.ns * datasize[*dtype];
    fd->file_linsiz = ddr.ns * datasize[ddr.dtype];
    }
return(E_SUCC);
}

#ifdef DEBUG_IMAGEIO

FUNCTION pfd(fd)

	struct FDESC *fd;
{
	printf("nl=%d\n", fd->nl);
	printf("ns=%d\n", fd->ns);
	printf("sl=%d\n", fd->sl);
	printf("offset=%x\n", fd->offset);
	printf("acc=%d\n", fd->acc);
	printf("method=%d\n", fd->method);
	printf("dtype=%d\n", fd->dtype);
	printf("nbuf=%d\n", fd->nbuf);
	printf("linsiz=%d\n", fd->linsiz);

	printf("conv_type=%d\n", fd->conv_type);
	printf("cnvlsz=%d\n", fd->cnvlsz);
	printf("file = %s\n", fd->fname);
	printf("nbands=%d\n", fd->nbands);
	printf("flags=%x\n", fd->flags);
}
#endif

