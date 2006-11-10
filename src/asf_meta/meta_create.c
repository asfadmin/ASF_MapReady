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
#include "get_ceos_names.h"

void meta_new2old(meta_parameters *meta);

/***********************************************************
 * meta_create:
 * Constructs a new meta_parameters record from CEOS, ARDOP
 * inputs, etc-- whatever it can find.*/
meta_parameters *meta_create(const char *fName)
{
	meta_parameters *meta = raw_init();
	int success=FALSE;
	char junk[256];

	if (get_ceos_metadata_name(fName,junk) != NO_CEOS_METADATA) {
		ceos_init(fName,meta);
		success=TRUE;
	}
	if (extExists(fName,".in")) {
		ardop_init(fName,meta);
		success=TRUE;
	}
	//if (success) meta_new2old(meta);
	return meta;
}
