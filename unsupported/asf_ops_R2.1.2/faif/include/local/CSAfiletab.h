/*==============================================================================
Filename:	CSAfiletab.h
Description:	Used in CSA2PMF modules; Contains CSA and GEN file types table
Creator:	Phil Yurchuk (phil@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSAFILETAB_
#define _CSAFILETAB_

#include <stdio.h>
#include "faifdefs.h"
#include "PMF.h"
#include "CSA.h"

typedef struct filetype
{
  char CSAfiletype[MAXLINE+1] ;
  char FAfiletype[MAXLINE+1] ;
  char Genfiletype[MAXLINE+1] ;
} CSA_Filetype_Table_Entry ;

CSA_Filetype_Table_Entry CSA_Filetype_Table[] = 
{
  { CSA_FTYPE_ORBDATA,    CSA_PREDORBIT_STR,  GFTYPE_PSV      },
  { CSA_FTYPE_ORBDATA,    CSA_DEFVORBIT_STR,  GFTYPE_RSV      },
  { CSA_FTYPE_RECRQST,    CSA_RECRQST_STR,    GFTYPE_FAPLAN   },
  { CSA_FTYPE_RECSCHED,   CSA_FTYPE_RECSCHED, GFTYPE_FASKED   },
  { CSA_FTYPE_CALRQST,    CSA_CALIBRQST_STR,  GFTYPE_CAL_RQST },
  { CSA_FTYPE_CALSCHED,   CSA_CALIBSCHED_STR, GFTYPE_CAL_SKED },
  { CSA_FTYPE_SARPROCPRM, CSA_SARPROCPRM_STR, GFTYPE_SARPROCP },
  { NULL,                 NULL,               NULL            }
} ;

#endif

/* End of File */
