/******************************************************************************
NAME:	GETTYP

FUNCTION:
   	GETTYP is a routine which returns the data type for an array of 
	images.  It returns a bad status code if the DDR cannot be read,
	or if the data types of the images are not the same.  No error 
	message is output.

PROGRAM HISTORY:
  Version	Date       Author       Request
  -------	----	   ------       -------
    1.0         8/86       K. Gacke     initial development
    1.1        12/87       B. Ailts     Change include directory specifications
					Use raw 'C' types
					Replace DESC arguments with char. desc.
					Place bridge routines in a seperate file
    1.2	       12/87	   B.Ailts	Implemented new DDR structure and call
    5.0        04/88	   D. Hollaren  changed include spec to las
					removed the call to squeeze routine
					return E_SUCC/E_FAIL
    5.1        08/88       B. Ailts	Fixed the declaration of the hostname
     5.2        07/90      D. VanderZee Standardized error handling 


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
	For each image 
	    "c_getddr" is called to get the data type of the image.
	    If the data type of the current image is not the same as the data 
	    type of the previous image,
		 a bad status is returned.
	Exit

ALGORITHM REFERENCES:

******************************************************************************/
#include "asf.h"
#include "las.h"

lasErr FUNCTION c_gettyp(const char *hosin[],int   *nmrimg,int   *odtype)
		/*char   hosin[][CMLEN];	 input host file names		     */
		/*int   *nmrimg;	 number of input images		     */
		/*int   *odtype; output data type -- data type of input img*/
{
struct DDR ddr;			/* DDR record				     */
int	image;			/* loop index for current input image        */
int	idtype;			/* input image data type                     */

/*
   Retrieve the DDR for each image.  If the data types of the input images are 
   not the same, a bad status code is returned.  Otherwise the input data type 
   is assigned to "odtype" which is returned to the calling program.  
*/
for (image = 0; image < *nmrimg; image++)
   {
   if (c_getddr(hosin[image],&ddr) != E_SUCC)
      {
      c_errmsg("Error returned from getddr","gettyp-call",NON_FATAL);
      return(E_FAIL);
      }
   idtype = ddr.dtype;
   if (image == 0)
      *odtype = idtype;
   else if (idtype != *odtype)
      {
      c_errmsg("Error: Data types of images are not the same","gettyp-dtype",
		NON_FATAL);
      return(E_FAIL);
      }
   }

return(E_SUCC);
}
