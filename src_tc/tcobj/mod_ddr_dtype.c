/****************************************************************
NAME: mod_ddr.c -  Changes a DDR data type.

SYNOPSIS:  mod_ddr filename 

DESCRIPTION:
   mod_ddr_dtype extracts the first two records from a las data
   descriptor record file that was generated on SUN workstations.
   (Actually will read any DDR that was made using IEEE standards
   for encoding numbers.) Once the data is read in (as characters),
   the data type of the ddr is set to EBYTE.

EXTERNAL ASSOCIATES:
   value 	returns the value of buffer as longerpretted as
		an integer value

FILE REFERENCES:

PROGRAM HISTORY:

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
   Open the ddr file called filename.ddr
   Read the integer data record
   Modify the data type to be type byte
   Write the integer data record
   Close the ddr file

ALGORITHM REFERENCES:

BUGS:

********************************************************************/
#include "asf.h"

#include "ddr.h"
#include "sarsim.h"

int mod_ddr(file,newDataType)
   char *file;                  /* host name of DDR file           */
   int newDataType;
{
	struct DDR ddr;
	c_getddr(file,&ddr);
	ddr.dtype=newDataType;
	c_putddr(file,&ddr);
	return(0);
}
