/********************************************************************
Routines used by projectGeo:
Projection-package routines. These routines interface with the
asf_geolib library to actually do the lat,lon -> projX,projY
transformation.  Also contains metadata conversion routines,
to create the new DDR and .meta file.

O. Lawlor, 3/99  Initial Creation
P. Denny,  6/01  Properly update ddr proj_units
P. Denny, 10/03  Replace all DDR use with .meta
*********************************************************************/
#include "projectGeo.h"
#include "cproj.h"
/******************* Projection Parameters Routines **************/

/*Initialize the transform, and get a function pointer*/
forward_transform init_proj(proj_prm *proj)
{
	forward_transform for_trans[100];
	int status=0;
	for_init(proj->proj,proj->zone,proj->parms,proj->datum,
		NULL,NULL,&status,for_trans);
	if (status!=0)
	{
		sprintf(errbuf, "   ERROR: For_init returned error code %d!\n",status);
		printErr(errbuf);
	}
	return for_trans[proj->proj];
}

/* DEPRICATED
 *Write the given projection parameters into the given DDR*
 *void write_proj_ddr(double pixSize,const proj_prm *proj,const window *w,
 *	struct DDR *ddr)
 *{
 *	int i;
 *	 *Set output type*
 *	ddr->nbands=1;
 *	ddr->dtype=1;
 *	
 *	 *Set output image size*
 *	ddr->nl=(w->maxY-w->minY)/pixSize;
 *	ddr->ns=(w->maxX-w->minX)/pixSize;
 *	
 *	 *Set all fields to valid*
 *	ddr->valid[DDPCV]=ddr->valid[DDZCV]=
 *	  ddr->valid[DDDCV]=ddr->valid[DDPPV]=
 *	  ddr->valid[DDPUV]=ddr->valid[DDPDV]=
 *	  ddr->valid[DDCCV]=ddr->valid[DDINCV]=1;
 *	
 *	 *Assign all projection fields*
 *	ddr->proj_code=proj->proj;
 *	ddr->zone_code=proj->zone;
 *	ddr->datum_code=proj->datum;
 *	switch(proj->units){
 *	  case 0: strcpy(ddr->proj_units,"radians");break;
 *	  case 1: strcpy(ddr->proj_units,"feet");   break;
 *	  case 2: strcpy(ddr->proj_units,"meters"); break;
 *	  case 3: strcpy(ddr->proj_units,"seconds");break;
 *	  case 4: strcpy(ddr->proj_units,"degrees");break;
 *	  case 5: strcpy(ddr->proj_units,"dms");    break;
 *	  default: strcpy(ddr->proj_units,"meters");break;
 *	}
 *	
 *	 *Assign projection parameters*
 *	for (i=0;i<15;i++)
 *		ddr->proj_coef[i]=proj->parms[i];
 *	
 *	 *Assign corner coordinates*
 *	ddr->upleft[0]=w->maxY;ddr->upleft[1]=w->minX;
 *	ddr->loleft[0]=w->minY;ddr->loleft[1]=w->minX;
 *	ddr->upright[0]=w->maxY;ddr->upright[1]=w->maxX;
 *	ddr->loright[0]=w->minY;ddr->loright[1]=w->maxX;
 *	
 *	 *Assign projection distances*
 *	ddr->pdist_y=ddr->pdist_x=pixSize;
 *}
 */

int write_proj_meta(double pixSize,proj_prm *proj,const window *w,
	meta_parameters *meta)
{
	double ignored_double;
	int ignored;
	char message[256];
	meta_projection *p = meta->projection = meta_projection_init();

	/* Stuff outside the projection block*/
	meta->general->data_type = BYTE;
	meta->general->line_count = (w->maxY-w->minY)/pixSize;
	meta->general->sample_count = (w->maxX-w->minX)/pixSize;
	meta->general->x_pixel_size = pixSize;
	meta->general->y_pixel_size = pixSize;
	meta->sar->image_type='P';	

	/* Projection stuff */
	switch(proj->proj)
	{/*Switch on the output projection used.*/
	case UTM:/*Universal Transverse Mercator projection*/
		p->type = 'U';
		p->param.utm.zone = proj->zone;
		break;
	case PS:/*Polar Stereographic projection*/
		p->type = 'P';
		p->param.ps.slat = paksz(proj->parms[5],&ignored);
		p->param.ps.slon = paksz(proj->parms[4],&ignored);
		break;
	case LAMCC:/*Lambert Conformal Conic*/
		p->type = 'L';
		p->param.lamcc.lat0 = paksz(proj->parms[5],&ignored);
		p->param.lamcc.lon0 = paksz(proj->parms[4],&ignored);
		p->param.lamcc.plat1 = paksz(proj->parms[2],&ignored);
		p->param.lamcc.plat2 = paksz(proj->parms[3],&ignored);
		break;
	default:/*Unrecognized Map projection!*/
		sprintf(message,
			"   WARNING: Unrecognized map projection code %d passed to \n"
			"            geocode/proj.c:write_proj_meta!\n"
			"            NOT creating .meta file...\n",proj->proj);
		printf("%s",message);
		if (logflag) {
			printLog(message);
		}
		return 0;
	}
	p->startX = w->minX;
	p->startY = w->maxY;
	p->perX = pixSize;
	p->perY = -pixSize;
	switch(proj->units){
	  case 2: strcpy(p->units,"meters"); break;
	  case 3: strcpy(p->units,"arcsec");break;
	  default: strcpy(p->units,"meters");break;
	}
	p->hem = (aLat<0)?'S':'N';
	/*Find earth radii*/
	sphdz(proj->datum,proj->parms,&p->re_major,&p->re_minor,&ignored_double);
	return 1;
}







