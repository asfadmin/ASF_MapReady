/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/18/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include <ctype.h>
#include "meta_init.h"
#include "jpl_proj.h"

/*Allocate a projection parameters structure,
given an ASF map projection data record.*/
void init_asf_proj(meta_parameters *sar,ceos_description *ceos,
	struct VFDRECV *facdr,struct VMPDREC *mpdr)
{
	proj_parameters *proj=(proj_parameters *)MALLOC(sizeof(proj_parameters));
	sar->geo->type='P';/*Map-Projected image.*/
	sar->geo->proj=proj;

	if (strncmp(mpdr->mpdesig, "GROUND RANGE",12) == 0) {
		proj->type='A';/*Along Track/Cross Track.*/
	} 
	else if (strncmp(mpdr->mpdesig, "LAMBERT", 7) == 0) {
		proj->type='L';/*Lambert Conformal.*/
		printf("WARNING: Lambert-geocoded images may not be\n"
			"accurately geocoded!\n");
		proj->param.lambert.plat1=mpdr->nsppara1;
		proj->param.lambert.plat2=mpdr->nsppara2;
		proj->param.lambert.lat0=mpdr->blclat+0.023;/*<-- Note: This line is a hack.*/
		proj->param.lambert.lon0=mpdr->blclong+2.46;/*<-- Note: This line is a hack, too.*/
		/*We have to hack the lambert projection because the true lat0
		and lon0, as far as we can tell, are never stored in the CEOS.*/
	}
	else if (strncmp(mpdr->mpdesig, "UPS", 3) == 0) {
		proj->type='P';/*Polar Stereographic: pre-radarsat era.*/
		proj->param.ps.slat=70.0;
		proj->param.ps.slon=-45.0;
	}
	else if (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0) {
		proj->type='P';/*Polar Stereographic: radarsat era.*/
		proj->param.ps.slat=mpdr->upslat;
		proj->param.ps.slon=mpdr->upslong;
		if (proj->param.ps.slat>0 && proj->param.ps.slon==0.0)
		  proj->param.ps.slon=-45.0;/*Correct reference longitude bug.*/
	} 
	else if (strncmp(mpdr->mpdesig, "UTM", 3) == 0) {
		proj->type='U';/*Universal Transverse Mercator.*/
		proj->param.utm.zone=UTM_zone(mpdr->utmpara1);
	} 
	else { 
		printf("Cannot match projection '%s',\n"
			"in map projection data record.\n",mpdr->mpdesig); 
		exit(1); 
	}
	
	if (facdr->imgclat > 0.0) 
		proj->hem='N'; 
	else 
		proj->hem='S';
/*ASF Geocoded images use the GEM-06 (Goddard Earth Model 6) Ellipsoid*/
	proj->re_major=6378144.0;
	proj->re_minor=6356754.9;
	
	if (proj->type=='A') 
	{/*The along Track/Cross Track projection requires special initialization.*/
		stateVector st_start;
		
		proj->param.atct.rlocal=sar->ifm->er;
		st_start=meta_get_stVec(sar,0.0);
		fixed2gei(&st_start,0.0);/*Remove earth's spin.  
				JPL's AT/CT projection requires this.*/
		atct_init(sar->geo,st_start);
		proj->startY = mpdr->tlceast;
		proj->startX = mpdr->tlcnorth;
		proj->perY   = (mpdr->blceast-mpdr->tlceast)/mpdr->nlines;
		proj->perX   = (mpdr->trcnorth-mpdr->tlcnorth)/mpdr->npixels;
	} else {
		proj->startY = mpdr->tlcnorth;
		proj->startX = mpdr->tlceast;
		proj->perY   = (mpdr->blcnorth-mpdr->tlcnorth)/mpdr->nlines;
		proj->perX   = (mpdr->trceast-mpdr->tlceast)/mpdr->npixels;
	}
   

}

/*ceos_init_asf:
	Called to read all ASF-specific fields from CEOS.
This is amounts to the facility-related data record.*/
void ceos_init_asf(char *fName,ceos_description *ceos,meta_parameters *sar)
{
	struct VFDRECV facdr;
	struct VMPDREC mpdr;
	struct dataset_sum_rec dssr;
	/*int has_mpdr=0;*/

/*Fetch the facility-related data record.*/
	get_facdr(fName,&facdr);

  	sar->info->bitErrorRate = facdr.biterrrt;
	
	sar->geo->slantFirst=facdr.sltrngfp*1000.0;

	/* For ASF SLC, the spacings are incorrect in the FACDR - TL 3/01 */
	if (ceos->product != CCSD)
	 {
  	  if (facdr.rapixspc>0.0)
		sar->geo->xPix=facdr.rapixspc;
	  if (facdr.azpixspc>0.0)
		sar->geo->yPix=facdr.azpixspc;
	 }

	sar->geo->azPixTime=sar->geo->yPix/facdr.swathvel;
	sar->ifm->er=facdr.eradcntr*1000.0;
	sar->ifm->ht=sar->ifm->er+facdr.scalt*1000;
	sar->ifm->orig_nLines=facdr.nlines;
	sar->ifm->orig_nSamples=facdr.apixels;
	
	if (toupper(facdr.deskewf[0])=='Y')
		sar->geo->deskew=1;/*Image has been doppler deskewed.*/
	else 
		sar->geo->deskew=0;/*Image has NOT been doppler deskewed.*/

/*Extract and parse the map projection data record, if present.*/
	if (-1!=get_mpdr(fName,&mpdr))
	/*Has map projection data record.*/
		init_asf_proj(sar,ceos,&facdr,&mpdr);
	else if (0==strncmp(facdr.grndslnt,"GROUND",6))
		sar->geo->type='G';/*Ground-range image.*/
	else
		sar->geo->type='S';/*Slant-range image.*/

/*For ASP images, flip the image top-to-bottom:*/
	if (ceos->processor==ASP||ceos->processor==SPS||ceos->processor==PREC)
	{
		sar->geo->timeShift+=sar->geo->azPixTime*facdr.alines;
		sar->geo->azPixTime*=-1.0;
	}

/*Read extra information about the sensor.*/
	get_dssr(fName,&dssr);
	if (strncmp(dssr.sensor_id,"ERS-1",5)==0) strcpy(sar->info->sensor,"ERS1");
	else if (strncmp(dssr.sensor_id,"ERS-2",5)==0) strcpy(sar->info->sensor,"ERS2");
	else if (strncmp(dssr.sensor_id,"JERS-1",6)==0) strcpy(sar->info->sensor,"JERS1");
	else if (strncmp(dssr.sensor_id,"RSAT-1",6)==0)
	  {
	    char beamname[32];
	    strcpy(sar->info->sensor,"RSAT-1");
            if (strncmp(dssr.product_type,"SCANSAR",7)==0)
             {
              if (strncmp(dssr.beam3,"WD3",3)==0) strcpy(beamname,"SWA");
              else if (strncmp(dssr.beam3,"ST5",3)==0) strcpy(beamname,"SWB");
              else if (strncmp(dssr.beam3,"ST6",3)==0) strcpy(beamname,"SNA");
              else strcpy(beamname,"SNB");	
             }
            else
	     {
	      int beamnum = atoi(&(dssr.beam1[2]));
	      switch(dssr.beam1[0])
               {
                case 'S': sprintf(beamname,"ST%i",beamnum); break;
                case 'W': sprintf(beamname,"WD%i",beamnum); break;
                case 'F': sprintf(beamname,"FN%i",beamnum); break;
                case 'E':
                  if (dssr.beam1[1]=='H') sprintf(beamname,"EH%i",beamnum);
                  else sprintf(beamname,"EL%i",beamnum);
		  break;
                default:
                 printf("Unknown Beam Name: %s\n",dssr.beam1);
               }
	     } 
	    strcpy(sar->info->mode,beamname);
          }
}
