/****************************************************************
FUNCTION NAME:  meta_init_ceos

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#ifndef __INIT_CEOS_H
#define __INIT_CEOS_H

#include "ceos.h"

typedef struct {
	struct dataset_sum_rec dssr;
	enum {unknownFacility,ASF,VEXCEL,ESA/*,ESA, etc.*/} facility;
	enum {unknownSatellite,ERS,JERS,RSAT} satellite;
	double version;/*Processor version number, or zero.*/
	enum {unknownProcessor,ASP,SPS,AISP,PREC,PP,SP2,AMM,FOCUS} processor;
	enum {unknownProduct,CCSD, LOW_REZ, HI_REZ, SCANSAR} product;
} ceos_description;

/*get_ceos_description:
Extract a ceos_description structure from given CEOS file.
This contains "meta-meta-"data, data about the CEOS, such
as the generating facility, a decoded product type, etc.*/
ceos_description *get_ceos_description(char *fName);

/*Facility-specific decoders:*/
void ceos_init_asf(char *fName,ceos_description *ceos,meta_parameters *sar);
/*void ceos_init_esa(char *fName,ceos_description *ceos,meta_parameters *sar), etc.*/

/*Ceos_init_stVec:
	Reads state vectors from given CEOS file, writing
them in the appropriate format to SAR parameters structure.*/
void ceos_init_stVec(char *fName,ceos_description *ceos,meta_parameters *sar);

#endif


