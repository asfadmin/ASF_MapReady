/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from various
metadata files and meta_parameters structure.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "meta_init.h"

/*meta_create:
	Constructs a new meta_parameters record
from CEOS, AISP inputs, etc-- whatever it can find.
*/
meta_parameters *meta_create(const char *fName)
{
	meta_parameters *meta=NULL;
	meta=raw_init();
	if (extExists(fName,".L")
	  ||extExists(fName,".ldr"))
		ceos_init(fName,meta);
	if (extExists(fName,".in"))
		aisp_init(fName,meta);
	return meta;
}
