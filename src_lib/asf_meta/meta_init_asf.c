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
void init_asf_proj(meta_parameters *meta,ceos_description *ceos,
	struct VFDRECV *facdr,struct VMPDREC *mpdr)
{
	meta_projection *projection=(meta_projection *)MALLOC(sizeof(meta_projection));
	meta->sar->image_type = 'P';/*Map-Projected image.*/
	meta->projection = projection;

	if (strncmp(mpdr->mpdesig, "GROUND RANGE",12) == 0) {
		projection->type='A';/*Along Track/Cross Track.*/
	} 
	else if (strncmp(mpdr->mpdesig, "LAMBERT", 7) == 0) {
		projection->type='L';/*Lambert Conformal.*/
		printf("WARNING: Lambert-geocoded images may not be\n"
			"accurately geocoded!\n");
		projection->param.lambert.plat1=mpdr->nsppara1;
		projection->param.lambert.plat2=mpdr->nsppara2;
		projection->param.lambert.lat0=mpdr->blclat+0.023;/*<-- Note: This line is a hack.*/
		projection->param.lambert.lon0=mpdr->blclong+2.46;/*<-- Note: This line is a hack, too.*/
		/*We have to hack the lambert projection because the true lat0
		and lon0, as far as we can tell, are never stored in the CEOS.*/
	}
	else if (strncmp(mpdr->mpdesig, "UPS", 3) == 0) {
		projection->type='P';/*Polar Stereographic: pre-radarsat era.*/
		projection->param.ps.slat=70.0;
		projection->param.ps.slon=-45.0;
	}
	else if (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0) {
		projection->type='P';/*Polar Stereographic: radarsat era.*/
		projection->param.ps.slat=mpdr->upslat;
		projection->param.ps.slon=mpdr->upslong;
		if (projection->param.ps.slat>0 && projection->param.ps.slon==0.0)
		  projection->param.ps.slon=-45.0;/*Correct reference longitude bug.*/
	} 
	else if (strncmp(mpdr->mpdesig, "UTM", 3) == 0) {
		projection->type='U';/*Universal Transverse Mercator.*/
		projection->param.utm.zone=UTM_zone(mpdr->utmpara1);
	} 
	else { 
		printf("Cannot match projection '%s',\n"
			"in map projection data record.\n",mpdr->mpdesig); 
		exit(1); 
	}
	
	if (facdr->imgclat > 0.0) 
		projection->hem='N'; 
	else 
		projection->hem='S';
/*ASF Geocoded images use the GEM-06 (Goddard Earth Model 6) Ellipsoid*/
	projection->re_major=6378144.0;
	projection->re_minor=6356754.9;
	
	if (projection->type=='A') 
	{/*The along Track/Cross Track projection requires special initialization.*/
		stateVector st_start;
		
		projection->param.atct.rlocal = meta_get_earth_radius(meta, meta->general->line_count/2, 0);
		st_start=meta_get_stVec(meta,0.0);
		fixed2gei(&st_start,0.0);/*Remove earth's spin.  
				JPL's AT/CT projection requires this.*/
		atct_init(meta->proj,st_start);
		projection->startY = mpdr->tlceast;
		projection->startX = mpdr->tlcnorth;
		projection->perY   = (mpdr->blceast-mpdr->tlceast)/mpdr->nlines;
		projection->perX   = (mpdr->trcnorth-mpdr->tlcnorth)/mpdr->npixels;
	} else {
		projection->startY = mpdr->tlcnorth;
		projection->startX = mpdr->tlceast;
		projection->perY   = (mpdr->blcnorth-mpdr->tlcnorth)/mpdr->nlines;
		projection->perX   = (mpdr->trceast-mpdr->tlceast)/mpdr->npixels;
	}
}



/*ceos_init_asf:
	Called to read all ASF-specific fields from CEOS.
This is amounts to the facility-related data record.*/
void ceos_init_asf(char *fName,ceos_description *ceos,meta_parameters *meta)
{
	struct VFDRECV facdr;
	struct VMPDREC mpdr;
	struct dataset_sum_rec dssr;
	/*int has_mpdr=0;*/

/*Fetch the facility-related data record.*/
	get_facdr(fName,&facdr);

  	meta->general->bit_error_rate = facdr.biterrrt;
	
	meta->sar->slant_range_first_pixel = facdr.sltrngfp*1000.0;

	/* For ASF SLC, the spacings are incorrect in the FACDR - TL 3/01 */
	if (ceos->product != CCSD)
	 {
  	  if (facdr.rapixspc>0.0)
		meta->general->x_pixel_size = facdr.rapixspc;
	  if (facdr.azpixspc>0.0)
		meta->general->y_pixel_size = facdr.azpixspc;
	 }

	meta->sar->azimuth_time_per_pixel = meta->general->y_pixel_size / facdr.swathvel;
	meta->general->line_count   = facdr.nlines;
	meta->general->sample_count = facdr.apixels;
	
	if (toupper(facdr.deskewf[0])=='Y')
		meta->sar->deskewed=1;/*Image has been doppler deskewed.*/
	else 
		meta->sar->deskewed=0;/*Image has NOT been doppler deskewed.*/

/*Extract and parse the map projection data record, if present.*/
	if (-1!=get_mpdr(fName,&mpdr))
	/*Has map projection data record.*/
		init_asf_proj(meta,ceos,&facdr,&mpdr);
	else if (0==strncmp(facdr.grndslnt,"GROUND",6))
		meta->sar->image_type='G';/*Ground-range image.*/
	else
		meta->sar->image_type='S';/*Slant-range image.*/

/*For ASP images, flip the image top-to-bottom:*/
	if (ceos->processor==ASP||ceos->processor==SPS||ceos->processor==PREC)
	{
		meta->sar->time_shift += meta->sar->azimuth_time_per_pixel * facdr.alines;
		meta->sar->azimuth_time_per_pixel *= -1.0;
	}

/*Read extra information about the sensor.*/
	get_dssr(fName,&dssr);
	if (strncmp(dssr.sensor_id,"ERS-1",5)==0) strcpy(meta->general->sensor,"ERS1");
	else if (strncmp(dssr.sensor_id,"ERS-2",5)==0) strcpy(meta->general->sensor,"ERS2");
	else if (strncmp(dssr.sensor_id,"JERS-1",6)==0) strcpy(meta->general->sensor,"JERS1");
	else if (strncmp(dssr.sensor_id,"RSAT-1",6)==0)
	  {
	    char beamname[32];
	    strcpy(meta->general->sensor,"RSAT-1");
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
	    strcpy(meta->general->mode,beamname);
          }
}
