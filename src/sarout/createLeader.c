/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include <string.h>
#include "ceos_defaults.h"
#include "sarout.h"


void createLeader(ceosLeader *data, char *filename);
void createLeader(ceosLeader *data, char *filename)
{
   data->dssr  = default_dssr;
   data->mpdr  = default_mpdr;
   data->ppdr  = default_ppdr;
   data->atdr  = default_atdr;
   data->raddr = default_raddr;
   data->dqsr  = default_dqsr;
   data->rsr   = default_rsr;
   data->sdhr  = default_dhr;
   data->pdhr  = default_dhr;
   data->facdr = default_facdr;

   strcpy(data->dssr.product_id, filename);
   strcpy(data->facdr.imageid, filename);
}






