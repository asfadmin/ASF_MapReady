/******************************************************************************
FUNCTION:	c_eopenr

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	   	    T.Butzer		Original development
  2.0      12/87    B.Ailts		Placed bridge routines into a seperate
					file
					Used raw 'C' types
  2.1	   04/88    D.Hollaren		Replaced newlas.h with las.h
  2.2	   05/88    B.Ailts		Replaced errh with c_errmsg
					Changed the calling sequence of the 
					DDR calls -- one structure
  2.3	   08/88    L. Huewe		Added argument for number of lines to
					be read at one time
  2.4      09/88    L. Huewe		Fixed the calling sequence to i_read
  2.5      09/88    B.Ailts		filesize is now checked only for READ
					access -- not for UPDATE access
  2.6      10/88    B.Ailts		Added 'if' statement to not allocate
					conversion buffer if it is not needed
  2.7      10/89    B.Davis		Placed the call to lused inside of a
					check for read access.  We don't
					want to change the operating system
					time stamp of the ddr disk file when
					all we are doing is reading an image
					for input.  (for Catalog Manager).
  2.8      02/91    B.Ailts		Standardized error messages
  2.9      05/92    T. Mittan           Check entire filesize not just
                                        requested filesize
  3	   10/92    T.Mittan		Added compression option
  4	   11/93    T. Mittan		Modifiied compression option to
					be set with a LAS global.
            1/94    K.Gacke             Modified to read/write dal images. TAE
                                        images can be read, but all images are
                                        written as a BSQ DAL image.
  7.0	    4/95    T.Logan	  	Removed TAE dependencies

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT			LAS

ALGORITHM 
If acess equals read
   Initialize the file descriptor members
   Compare the system of image and the system of the CPU
   Open the image
   Get the ddr information and place in the fdesc structure
   Check the window specifications
   Check the band specifications
   Check for dat type conversion
Else write access
   Check the band specification
   Check the options argument
   Initialize the DDR fields
   Fill in the DDR fields that are needed
   Write the band indepenant DDR fields
   Initialize the band depenant fields
   Write the band depentant fields
If access == UPDATE or flags & CONVERSION or flags & MOVEDATA
   Allocate the conversion buffer space
Write the last used date and time to the DDR

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#define COMPRES1 "COMPRES"

#include "las.h"
#include "locinc.h"

int compres_opt;	/*ASF*/

lasErr FUNCTION c_eopenr(struct FDESC **fdesc,char *fname,int *nbands,
		int *sl,int *ss,int *nl,int *ns,
		int *acc,int *dtype,int opt[],int *maxline)
{
struct DDR ddr;
register struct FDESC *fd;

int dflag;
int filesize=0;
int i;
/*int linsiz=0;*/
int stati;

char errtxt[ERRLEN];
/*char system[DDSYLN];*/

*fdesc = (struct FDESC *)calloc(1,sizeof(struct FDESC));
if (*fdesc == NULL)
   {
   c_errmsg("Error alocatting dynamic memory",
           "eopenr-alloc",NON_FATAL);
   return(E_FAIL);
   }
fd = *fdesc;	/* used to simplify expressions */
fd->nl = fd->img_nl = *nl;
fd->ns = fd->img_ns = *ns;
fd->sl = *sl;
fd->ss = *ss;
fd->acc = *acc;
fd->method = RANDOM;
fd->dtype = *dtype;
fd->nbuf = 0;
(void)strcpy(fd->fname, fname);
fd->flags = 0; /* clear flags */
fd->cnvlsz = 0;

fd->nbands = *nbands;
	
if ((fd->acc == IREAD) || (fd->acc == IUPDATE))
	{	
/*  Compare the system that the data is stored on and the system that will
    process the data.
--------------------------------------------------------------------------*/
	stati = c_getddr(fname, &ddr);
	if ((dflag == 3) || 
            ((dflag == 1) && (ddr.dtype == EREAL)) ||
	    ((dflag == 2) && (ddr.dtype != EBYTE)))
	   {
	   c_errmsg("Data type of image is not compatible with the processor",
	      "eopenr-dtype",NON_FATAL);
	   return(E_FAIL);
	   }


	stati = do_open(fd);	
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from doopen",
	           "eopenr-call",NON_FATAL);
	   return(E_FAIL);
	   }

	/*  moved this statement up a few lines *****
	stati = c_getddr(fname, &ddr);
	***************************/
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from getddr","eopenr-call",
			NON_FATAL);
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

	if (*nbands == 0)
		{
		*nbands = ddr.nbands;
		for(i=1; i<=ddr.nbands; i++)
			fd->bands[i-1] = i;
		fd->nbands = *nbands;
		}

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
	   sprintf(errtxt,"Error returned from check_window");
	   c_errmsg(errtxt,"eopenr-call",NON_FATAL);
	   return(E_FAIL);
	   }

	if (fd->nbands > ddr.nbands)
	   {
	   sprintf(errtxt,"More bands specified than exist for %s",
		   fd->fname);
	   c_errmsg(errtxt,"eopenr-nbands",NON_FATAL);
	   return(E_FAIL);
	   }
	if (fd->compres == TRUE)
	   {
	   fd->mns = ddr.ns;
           if (fd->acc == IUPDATE)
              {
              c_errmsg("Compresed image can not be opened for update access"
                     ,"eopenr-update",NON_FATAL);
	      fclose(fd->fp);
              return(E_FAIL);
              }
           }

	if ((*ns * datasize[*dtype]) != fd->linsiz)
	    fd->flags |= MOVEDATA;

	if ((*ns != ddr.ns) && (*maxline > 1))
	    fd->flags |= MOVEDATA;


	}  /*  if (fd->acc == IREAD) || (fd->acc == IUPDATE)  */
else
	{
        fd->compres = fd->tae = FALSE;
        fd->dal = TRUE;
	if (*nbands < 1)
	   {
	   sprintf(errtxt,"No bands specified for file %s",fd->fname);
	   c_errmsg(errtxt,"eopenr-nobands",NON_FATAL);
	   return(E_FAIL);
	   }
	fd->linsiz = (*ns * datasize[*dtype] + 511) & ~0x1ff;

          if (opt[1] == COMPRES)
             {
             fd->compres = TRUE;
	     fd->dal = FALSE;
             fd->tempbuf = (unsigned char *) calloc(((int)(fd->linsiz *2))
			    ,sizeof(unsigned char));
             fd->f_pointer = (int *) calloc ((fd->nl * fd->nbands), 
			    sizeof(int));
             }
	  else
          if (opt[1] == NO_COMPRES)
             {
             fd->compres = FALSE;
             }
	  else
          if (compres_opt == NO_COMPRES)
             {
             fd->compres = FALSE;
             }
	  else
          if (compres_opt == COMPRES)
             {
             fd->compres = TRUE;
	     fd->dal = FALSE;
             fd->tempbuf = (unsigned char *) calloc(((int)(fd->linsiz *2))
                            ,sizeof(unsigned char));
             fd->f_pointer = (int *) calloc ((fd->nl * fd->nbands),
                            sizeof(int));
             }

		if ((opt[0] > 0) && (opt[0] < NDTYPES))	
		    {
		    if (opt[0] != fd->dtype)
			{
			fd->flags |= CONVERSION;
			fd->conv_type = opt[0];
                        if ((fd->tae) || (fd->compres))
                            fd->cnvlsz = (*ns * datasize[opt[0]] +511) & ~0x1ff;
                        else
                            fd->cnvlsz = *ns * datasize[opt[0]];
			}
		    }
		else
		if (opt[0] != 0)
		   {
		   c_errmsg("Invalid option specified","eopenr-option",
			    NON_FATAL);
		   return(E_FAIL);
		   }
	if (fd->dal)
	    {
	    fd->linsiz = *ns * datasize[*dtype];
	    filesize = fd->linsiz * fd->nl * fd->nbands;
	    if (opt[0] != 0)
		if (opt[0] != fd->dtype)
		    filesize = fd->cnvlsz * fd->nl * fd->nbands;
	    }
	else
	    {
	    filesize = 512 + fd->linsiz * fd->nl * fd->nbands;
	    if (opt[0] != 0)
		if (opt[0] != fd->dtype)
		    filesize = 512 + fd->cnvlsz * fd->nl * fd->nbands;
	    }
	c_dkcre(fd->fname, filesize);
		
	stati = do_open(fd);	
	if (stati != E_SUCC)
           {
	   c_errmsg("Error returned from doopen",
		   "eopenr-call",NON_FATAL);
	   return((lasErr)stati);
           }

	c_intddr(&ddr);	
	ddr.nl = fd->nl;
	ddr.ns = fd->ns;
	ddr.nbands = fd->nbands;
	if (fd->flags & CONVERSION)
	   {
	   ddr.dtype = fd->conv_type;
	   /*linsiz = fd->cnvlsz;*/
	   }
	else
	   {
	   ddr.dtype = fd->dtype;
	   /*linsiz = fd->linsiz;*/
	   }

	stati = c_putddr(fname, &ddr);
	if (stati != E_SUCC)
	   {
	   c_errmsg("Error returned from putddr","eopenr-call",
			NON_FATAL);
	   return(E_FAIL);
	   }

	if ((*ns * datasize[*dtype]) != fd->linsiz)
	    fd->flags |= MOVEDATA;
	}

if ((fd->flags & CONVERSION) || (fd->flags & MOVEDATA) || (fd->acc == IUPDATE))
   {
   if (fd->cnvlsz == 0)
	fd->cnvlsz = fd->linsiz;
   fd->conv_buf = (unsigned char *) calloc(1, ((ddr.ns * datasize[ddr.dtype]
					       + 511) & ~0x1ff) * (*maxline));
   if (fd->conv_buf == NULL)
       {
       c_errmsg("Error allocating buffer space","eopenr-allocbuf",NON_FATAL);
       return(E_FAIL);
       }
   }  /*  if conversion or movedata or update  */

return(E_SUCC);
}

