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
#include "asf.h"
#include "ceos.h"
#include "asf_endian.h"
#include "sarout.h"

void readLeader(char *inFile, int mode, ceosLeader *data);
void readLeader(char *inFile, int mode, ceosLeader *data)
 {
	get_dssr (inFile,&(data->dssr));
	get_ppdr (inFile,&(data->ppdr));
	get_atdr (inFile,&(data->atdr));
	get_dqsr (inFile,&(data->dqsr));
        get_sdhr (inFile,&(data->sdhr));
	get_rsr  (inFile,&(data->rsr));
	get_asf_facdr (inFile,&(data->facdr));

	if (mode != CEOS_SLC && mode != CEOS_CCSD)
 	 {
	   get_mpdr (inFile,&(data->mpdr));
	   get_raddr(inFile,&(data->raddr));
	   get_dhr  (inFile,&(data->pdhr)); 
         }
 }
