/************************************************************************** 
FUNCTION NAME:   ceos2ddr

SYNTAX:	ceos2ddr(ceos_name,ddr,headerLen,lineLen)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    ceosIn      char *          Input CEOS file name.
    ddrOut      struct DDR *    Output LAS DDR.
    headerLen   int *           Length (in bytes) of file header.
    lineLen     int *           Length (in bytes) of one line of the file.
    

DESCRIPTION:
    	Creates a valid DDR file by taking necessary data from
      the Imagery Options File, Map Projection data record, etc.

RETURN VALUE:	None

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
	1.0	Mike Shindle    3/95
	1.1	Tom Logan	4/95	Removed dependency on TAE parameters
	1.2     D. Corbett      5/97    add handling for geocoded SCANSAR
                                        change references to sarin
	1.3	D. Corbett 	1/98    correct units on remajor/minor if off 
	1.4     O. Lawlor      10/98    Made general to handle any CEOS image.
	1.5	T. Logan        7/99    Modified headerLen calculation to work
				        with RAMP data.
	1.6	P. Denny	9/01	Modified headerLen calculation to work
					with RAMP and any ASF CEOS (hardcoded)

****************************************************************************/
#include "asf.h"
#include "ddr.h"
#include "ceos.h"
#include "asf_endian.h"
#include "proj.h"
#include "asf_meta.h"
#include "ceos_io.h"

int firstRecordLen(char *ceosIn)
{
	char ceosName[255];
	FILE *f;
	struct HEADER h;
	set_era(ceosIn,ceosName,0);/*Get image file name.*/
	f=FOPEN(ceosName,"rb");/*Open file.*/
	FREAD(&h,1,12,f);/*Read first CEOS header.*/
	FCLOSE(f);
	return bigInt32(h.recsiz);
}

void ceos2ddr(char *ceosIn,struct DDR *ddrOut,int *headerLen,int *lineLen)
{
	int i;                  /* loop counter */
	int dataSize;		/* Number of bytes per image pixel.*/
	struct IOF_VFDR iof;    /* Imagery Options File, from CEOS.*/
	struct VFDRECV facdr;	/* Facility related data record   */
	struct VMPDREC mpdr;    /* Map Projection Data Record */
	meta_parameters *meta=meta_create(ceosIn);
	int has_mpdr=0;
	char dummy[255];
	
	/*Extract information about the CEOS image.*/
	get_ifiledr(ceosIn,&iof);
	get_facdr(ceosIn,&facdr);
	if (-1!=get_mpdr(ceosIn,&mpdr))
		has_mpdr=1;
	
	if (lineLen!=NULL)
		*lineLen=iof.reclen;
	if (headerLen!=NULL)
	{
		if (set_era(ceosIn,dummy,0)) /*new data header size*/
			*headerLen=firstRecordLen(ceosIn) + 192;
		else /* old data header size */
			*headerLen=firstRecordLen(ceosIn) + 12;
	}

	/* Set non-projection dependent records */
	dataSize=(iof.bitssamp+7)/8;
	ddrOut->nl = iof.numofrec;
	ddrOut->ns = facdr.npixels;
	ddrOut->nbands=iof.sampdata;
	strcpy(ddrOut->system,"ieee-std");
	switch (dataSize)/*switch on number of bytes per pixel.*/
	{
		case 2:
			ddrOut->dtype=DTYPE_SHORT;break;/*Short int data*/
		case 4:
			ddrOut->dtype=DTYPE_LONG;break;/*Long int data*/
		default:
			ddrOut->dtype=DTYPE_BYTE;break;/*probably byte data*/
	}
	ddrOut->master_line=ddrOut->master_sample=1.0;
	ddrOut->line_inc=ddrOut->sample_inc=1.0;
	
	/* Set all fields to INVALID */
	for (i = 0; i < 8; i++)
		ddrOut->valid[i] = INVAL;
	
	ddrOut->valid[DDINCV]=
	ddrOut->valid[DDPDV]=
	ddrOut->valid[DDPUV]=VALID;
	
	strcpy(ddrOut->proj_units,"METERS");
	ddrOut->pdist_x = meta->general->x_pixel_size;
	ddrOut->pdist_y = meta->general->y_pixel_size;
	
	if (has_mpdr)
	{
		/*char *projName;*/
	/*If we have a map-projection data record, 
	  we can extract all kinds of useful stuff from it.*/
		for (i=0;i<4;i++)
			ddrOut->valid[i]=VALID;
	        ddrOut->ns = mpdr.npixels;		
		ddrOut->pdist_x = mpdr.nomipd;
		ddrOut->pdist_y = mpdr.nomild;
		ddrOut->upleft[0] = mpdr.tlcnorth;
		ddrOut->upleft[1] = mpdr.tlceast;
		ddrOut->loleft[0] = mpdr.blcnorth;
		ddrOut->loleft[1] = mpdr.blceast;
		ddrOut->upright[0] = mpdr.trcnorth;
		ddrOut->upright[1] = mpdr.trceast;
		ddrOut->loright[0] = mpdr.brcnorth;
		ddrOut->loright[1] = mpdr.brceast;

		ddrOut->valid[DDCCV] = VALID; /* TL 1/99 - Set Corners Valid */
		
		if (mpdr.remajor < 10000.0) mpdr.remajor*=1000.0;
		if (mpdr.reminor < 10000.0) mpdr.reminor*=1000.0;
		
		/*projName=mpdr.mpdesig;*/
		for (i=0; i<15; i++)
		  ddrOut->proj_coef[i] = 0.0;
		ddrOut->proj_coef[0] = mpdr.remajor;
		ddrOut->proj_coef[1] = mpdr.reminor;
	}
		
	/* set projection dependent records */
	if (meta->sar->image_type=='P')
	{/*Map projected input*/
		meta_projection *p = meta->projection;
		int proj_invalid = 0;
		switch(p->type)
		{
		    case 'A':/*Along Track/Cross Track*/
			/*Can't do anything here until we add AT/CT to asf_geolib.*/
			proj_invalid=1;
			break;
		    case 'P':/*Polar Stereographic*/
			ddrOut->proj_code = PS;
			ddrOut->datum_code = 0;
			ddrOut->zone_code = 62;
			
			ddrOut->proj_coef[4] = packed_deg(p->param.ps.slon);
			ddrOut->proj_coef[5] = packed_deg(p->param.ps.slat);
			
			ddrOut->proj_coef[6] = 0.0;
			ddrOut->proj_coef[7] = 0.0; 
			break;
		    case 'L':/*Lambert Conformal Conic*/
			ddrOut->proj_code = LAMCC;	
			ddrOut->datum_code = 0;
			ddrOut->zone_code = 62;
			ddrOut->proj_coef[2] = packed_deg(p->param.lambert.plat1); /*standard parallel1*/
			ddrOut->proj_coef[3] = packed_deg(p->param.lambert.plat2); /*standard parallel2*/
			ddrOut->proj_coef[4] = packed_deg(p->param.lambert.lon0);  /*center longitude  */
			ddrOut->proj_coef[5] = packed_deg(p->param.lambert.lat0);   /*center latitude   */
			ddrOut->proj_coef[6] = 0;  /*false easting     */
			ddrOut->proj_coef[7] = 0; /*false northing    */
		
			break;
		    case 'U':/*Universal Transverse Mercator*/
			ddrOut->proj_code = UTM;
			ddrOut->datum_code = 0;
			ddrOut->zone_code = p->param.utm.zone;
			break;
		    default:
			printf("Unrecognized map projection type '%c' passed to ceos2ddr!\n",p->type);
			printf("Continuing...\n");
			proj_invalid=1;
			break;
		}
		if (proj_invalid)
		{
			for (i=0;i<4;i++) 
				ddrOut->valid[i] = UNKNOW;
			ddrOut->valid[DDCCV] = UNKNOW;
		}
	}
}
