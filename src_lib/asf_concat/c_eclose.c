/******************************************************************************
FUNCTION:	c_eclose

PURPOSE		closes the file after all processing is done

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
	   06/85    T. Butzer		Original implementation
	   03/86    T. Butzer		dealloc the buffer space used
	   03/86    K. Johnson		handle multiple file groups
	   12/87    B. Ailts		change include directory specifications
					change to raw 'C' types
					place bridge routines is seperate file
	   04/88    D.Hollaren		Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg
	   05/88    B.Ailts		Replaced xi___ calls with i___ calls
	   10/88    B.Ailts		Added statements to free dynamic
					allocated memory
	   02/91    B.Ailts		Standardized error messages
	   10/92    T.Mittan		Added compression option.
	   12/92    T.Mittan		Corrected problem closing file.
	   12/92    T.Mittan		Corrected problem freeing memory.
	    1/94    K.Gacke		Modified to read/write dal images.  TAE
					images can be read, but all images are
					written as a BSQ DAL image.
  7.0	    4/95    T.Logan (ASF)	Removed TAE dependencies

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:  None

PROJECT			LAS

ALGORITHM 
	If CONVERSION or MOVEDATA or UPDATE
	   Free conversion buffer memory
	Close the file
	Free the file descriptor memory
	Return

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include <unistd.h>
#include "las.h"
#include "locinc.h"

lasErr FUNCTION c_eclose(struct FDESC **fdesc)
{

	if (((*fdesc)->flags & CONVERSION) || ((*fdesc)->flags & MOVEDATA) || 
	    ((*fdesc)->acc == IUPDATE))
	free((*fdesc)->conv_buf);
        if ((*fdesc)->compres == TRUE)
           {
           if ((*fdesc)->acc == IWRITE)
              {
              fseek((*fdesc)->fp,18,0);
              fwrite((*fdesc)->f_pointer,sizeof(int),
                     ((*fdesc)->nl * (*fdesc)->nbands),(*fdesc)->fp);
              free((*fdesc)->f_pointer);
              free((*fdesc)->tempbuf);
              }
           else
              free((*fdesc)->foffset);
           fclose((*fdesc)->fp);
           c_dkfree((*fdesc)->fname);
           }
/**************
        else if ((*fdesc)->tae == TRUE)
           {
	   printf("tae i/o not supported\n");
	   return(E_FAIL);
	   disp = I_SAVE;
	   i_clse(&((*fdesc)->ifcb), disp);
	   c_dkfree((*fdesc)->fname);
           }
***************/	/*ASF*/
        else if ((*fdesc)->dal == TRUE)
           {
	   close((*fdesc)->dalfd);
	   c_dkfree((*fdesc)->fname);
           }
	free(*fdesc);
	return(E_SUCC);
}
