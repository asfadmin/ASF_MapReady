/*******************************************************************************
NAME:      	 SET_DTYPE

PURPOSE:   	 Sets the data type of the output image by determining the 
		 maximum data type of the input images.

PROGRAM HISTORY:
PROGRAMMER	  DATE		REASON
----------	  ----		------
B. Wilsey      Aug   1984	Original development
D. Akkerman    Aug   1987       PR #4247 modified to allow for single band 
				 specification of a multi-band images
K. Zanter      Sept. l987       NEWLAS (conversion to 'C')
B. Ailts       July  1988       LAS5.0 conversion -- added -AUTO subcommand
B. Ailts       Nov   1989       Shortened name to rn on IBM-RT
T. Logan       April 1995	Removed TAE Dependencies.  Biggest change was
			         limiting maskval to a scalar value
P. Denny       Mar   2002       Nuked unused function parameters:
      	      	      	      	  float maskval  // mask value -- ASF
      	      	      	      	  int  *in_count // number of mask values

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:        LAS		                

ALGORITHM DESCRIPTION:
Initialization 
For every image
   Retreive the data type
   If current data type is greater than output data type
      Output data type equals current data type

Switch on dtype -- EBYTE, EWORD, ELONG, or EREAL
   For each mask value
     Check to see if the mask value is within the range of the data type
Return

ALGORITHM REFERENCES:     none
*******************************************************************************/
#include "asf.h"



#include "las.h"
#include "concat.h"
#include "pixman.h"

void set_dtype(char  *hostin[],    /* pointer to input host file descriptor */
      	       int    image_count, /* number of input images                */
               int   *out_dtype    /* pointer to data type of output image  */
/*             float  maskval,     // mask value (ASF)
 *             int   *in_count     // number of mask values
 */           )
{
  int i;        /* loop counter                */
  int one = 1;  /* value of one                */
  int status;   /* function return status code */
  int type;     /* data type of current image  */

  /* Initialization of variables
     --------------------------*/
  *out_dtype = 0;

  for (i = 0; i < image_count; i++)
  {
    status = c_gettyp((const char **)&hostin[i],&one,&type);
    if (status != E_SUCC)
    {
      c_errmsg("Error reading DDR file","concat-ddr",NON_FATAL);
      c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
    }
    if (type > *out_dtype) *out_dtype = type;
  }  /* for i < image_count  */
  return;
}
