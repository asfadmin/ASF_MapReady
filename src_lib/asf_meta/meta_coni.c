/****************************************
meta_coni:
	Reads and writes meta_parameters structure 
to/from the given CONI file name.
****************************************/

#include "asf.h"
#include "coniFetch.h"
#include "asf_meta.h"

/*
void coniIO_str(coniStruct *coni,char *loc,char *name,char *value,char *comment);
void coniIO_structOpen(coniStruct *coni,char *name,char *comment);
void coniIO_structClose(coniStruct *coni,char *comment);
void coniIO_int(coniStruct *coni,char *loc,char *name,int *value,char *comment);
void coniIO_double(coniStruct *coni,char *loc,char *name,double *value,char *comment);
void coniIO_char(coniStruct *coni,char *loc,char *name,char *value,char *comment);
*/

/*meta_io_state:
	Called by meta_io, below, this routine reads/writes
the given state vector structure.
*/
void meta_io_state(coniStruct *coni, meta_state_vectors *state)
{
	int i;
	coniIO_structOpen(coni,"state {","begin list of state vectors for satellite, over image");
	coniIO_int(coni,"state.","year:",&state->year,"Year of image start");
	coniIO_int(coni,"state.","day:",&state->julDay,"Julian day of the year for image start");
	coniIO_double(coni,"state.","second:",&state->second,"Second of the day for image start");
	coniIO_int(coni,"state.","number:",&state->num,"Number of state vectors below");
	for (i=0;i<state->num;i++)
	{
		coniIO_structOpen(coni,"vector {","begin a single state vector");
		coniIO_double(coni,"state.vector.","time:",&state->vecs[i].time,"Time, relative to image start [s]");
		coniIO_double(coni,"state.vector.","x:",&state->vecs[i].vec.pos.x,"X Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","y:",&state->vecs[i].vec.pos.y,"Y Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","z:",&state->vecs[i].vec.pos.z,"Z Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","vx:",&state->vecs[i].vec.vel.x,"X Velocity, earth-fixed [m/s]");
		coniIO_double(coni,"state.vector.","vy:",&state->vecs[i].vec.vel.y,"Y Velocity, earth-fixed [m/s]");
		coniIO_double(coni,"state.vector.","vz:",&state->vecs[i].vec.vel.z,"Z Velocity, earth-fixed [m/s]");
		coniIO_structClose(coni,"end vector");
	}
	coniIO_structClose(coni,"end of list of state vectors\n");
}

/*meta_io:
	This routine reads/writes the given meta_parameters
structure from/to the given CONI file.  If reading==0,
we're writing to the file.  If reading==1, we're reading the file.
*/
void meta_io(coniStruct *coni,meta_parameters *meta,int reading)
{
	double version=0.9;
	geo_parameters *geo=meta->geo;
	ifm_parameters *ifm=meta->ifm;
	
	coniIO_double(coni,"","meta_version:",&version,"ASF-STEP Lab Metadata File.\n");
/*Geolocation parameters.*/
	coniIO_structOpen(coni,"geo {","begin parameters used in geolocating the image.");
	coniIO_char(coni,"geo.","type:",&geo->type,"Image type: [S=slant range; G=ground range; P=map projected]");
	if (geo->type=='P')
	{
		/*Projection Parameters.*/
		proj_parameters *proj=meta->geo->proj;
		if (reading)
			proj=meta->geo->proj=(proj_parameters *)MALLOC(sizeof(proj_parameters));
		coniIO_structOpen(coni,"proj {","Map Projection parameters");
		coniIO_char(coni,"geo.proj.","type:",&proj->type,"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
		coniIO_double(coni,"geo.proj.","startX:",&proj->startX,"Projection Coordinate at top-left, X direction");
		coniIO_double(coni,"geo.proj.","startY:",&proj->startY,"Projection Coordinate at top-left, Y direction");
		coniIO_double(coni,"geo.proj.","perX:",&proj->perX,"Projection Coordinate per pixel, X direction");
		coniIO_double(coni,"geo.proj.","perY:",&proj->perY,"Projection Coordinate per pixel, X direction");
		coniIO_char(coni,"geo.proj.","hem:",&proj->hem,"Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
		if (version>0.8)
		{ /*Read geoid from file*/
			coniIO_double(coni,"geo.proj.","re_major:",&proj->re_major,"Major (equator) Axis of earth (meters)");
			coniIO_double(coni,"geo.proj.","re_minor:",&proj->re_minor,"Minor (polar) Axis of earth (meters)");
		} else {
		/*Default: use the GEM-06 (Goddard Earth Model 6) Ellipsoid*/
			proj->re_major=6378144.0;
			proj->re_minor=6356754.9;
		}
		switch(proj->type)
		{
			case 'A':/*Along-track/cross-track projection.*/
				coniIO_double(coni,"geo.proj.","rlocal:",&proj->param.atct.rlocal,"Local earth radius [m]");
				coniIO_double(coni,"geo.proj.","atct_alpha1:",&proj->param.atct.alpha1,"at/ct projection parameter");
				coniIO_double(coni,"geo.proj.","atct_alpha2:",&proj->param.atct.alpha2,"at/ct projection parameter");
				coniIO_double(coni,"geo.proj.","atct_alpha3:",&proj->param.atct.alpha3,"at/ct projection parameter");
				break;
			case 'L':/*Lambert Conformal Conic projection.*/
				coniIO_double(coni,"geo.proj.","lam_plat1:",&proj->param.lambert.plat1,"Lambert first standard parallel");
				coniIO_double(coni,"geo.proj.","lam_plat2:",&proj->param.lambert.plat2,"Lambert second standard parallel");
				coniIO_double(coni,"geo.proj.","lam_lat:",&proj->param.lambert.lat0,"Lambert original latitude");
				coniIO_double(coni,"geo.proj.","lam_lon:",&proj->param.lambert.lon0,"Lambert original longitude");
				break;
			case 'P':/*Polar Stereographic Projection.*/
				coniIO_double(coni,"geo.proj.","ps_lat:",&proj->param.ps.slat,"Polar Stereographic reference Latitude");
				coniIO_double(coni,"geo.proj.","ps_lon:",&proj->param.ps.slon,"Polar Stereographic reference Longitude");
				break;
			case 'U':/*Universal Trasnverse Mercator Projection.*/
				coniIO_int(coni,"geo.proj.","utm_zone:",&proj->param.utm.zone,"UTM Zone Code");
				break;
			default:
				printf("ERROR! Unrecognized map projection code '%c!'\n",proj->type);
				exit(1);
		}
		coniIO_structClose(coni,"end proj");
	}
	coniIO_char(coni,"geo.","lookDir:",&geo->lookDir,"SAR Satellite look direction (normally R) [R=right; L=left]");
	coniIO_int(coni,"geo.","deskew:",&geo->deskew,"Image moved to zero doppler? [1=yes; 0=no]");
	coniIO_double(coni,"geo.","xPix:",&geo->xPix,"Pixel size in X direction [m]");
	coniIO_double(coni,"geo.","yPix:",&geo->yPix,"Pixel size in Y direction [m]");
	coniIO_double(coni,"geo.","rngPixTime:",&geo->rngPixTime,"Time/pixel, range (xPix/(c/2.0), or 1/fs) [s]");
	coniIO_double(coni,"geo.","azPixTime:",&geo->azPixTime,"Time/pixel, azimuth (yPix/swathVel, or 1/prf) [s]");
	coniIO_double(coni,"geo.","slantShift:",&geo->slantShift,"Error correction factor, in slant range [m]");
	coniIO_double(coni,"geo.","timeShift:",&geo->timeShift,"Error correction factor, in time [s]");
	coniIO_double(coni,"geo.","slantFirst:",&geo->slantFirst,"Slant range to first image pixel [m]");
	coniIO_double(coni,"geo.","wavelength:",&geo->wavelen,"SAR Carrier Wavelength [m]");
	coniIO_double(coni,"geo.","dopRangeCen:",&geo->dopRange[0],"Doppler centroid [Hz]");
	coniIO_double(coni,"geo.","dopRangeLin:",&geo->dopRange[1],"Doppler per range pixel [Hz/pixel]");
	coniIO_double(coni,"geo.","dopRangeQuad:",&geo->dopRange[2],"Doppler per range pixel sq. [Hz/(pixel^2)]");
	coniIO_double(coni,"geo.","dopAzCen:",&geo->dopAz[0],"Doppler centroid [Hz]");
	coniIO_double(coni,"geo.","dopAzLin:",&geo->dopAz[1],"Doppler per azimuth pixel [Hz/pixel]");
	coniIO_double(coni,"geo.","dopAzQuad:",&geo->dopAz[2],"Doppler per azimuth pixel sq. [Hz/(pixel^2)]");
	coniIO_structClose(coni,"end geo\n");
	
/*Interferometry parameters:*/
	coniIO_structOpen(coni,"ifm {","begin interferometry-related parameters");
	coniIO_double(coni,"ifm.","er:",&ifm->er,"Local earth radius [m]");
	coniIO_double(coni,"ifm.","ht:",&ifm->ht,"Satellite height, from center of earth [m]");
	if (version>0.6)
		coniIO_int(coni,"ifm.","nLooks:",&ifm->nLooks,"Number of looks to take from SLC");
	coniIO_int(coni,"ifm.","orig_lines:",&ifm->orig_nLines,"Number of lines in original image");
	coniIO_int(coni,"ifm.","orig_samples:",&ifm->orig_nSamples,"Number of samples in original image");
	coniIO_structClose(coni,"end ifm\n");
	
/*State Vectors:*/
	if (reading)
	{/*Check to see if the state vectors even exist.*/
		int err,nVec;
		nVec=coniInt(coni,"state.number:",&err);
		coniReopen(coni);/*Seek back to beginning of file.*/
		if (err==CONI_OK && nVec!=0)
		{/*We have state vectors!*/
			meta->stVec=raw_init_state(nVec);/*Allocate state vectors.*/
			meta_io_state(coni,meta->stVec);/*And initialize them.*/
		}
	}
	else if (meta->stVec!=NULL)/*Writing state vectors.*/
		meta_io_state(coni,meta->stVec);

/*Extra Info:*/
	if (reading)
 	  { /* Check to see if extra info exists.*/
		int err;
		coniStr(coni,"extra.sensor:",&err);
		coniReopen(coni);/*Seek back to beginning of file.*/
		if (err==CONI_OK)
		  {
		    meta->info = (extra_info *) MALLOC (sizeof(extra_info));
		    coniIO_str(coni,"extra.","sensor:",meta->info->sensor,"Imaging sensor");
	    	    if (version>0.7) coniIO_str(coni,"extra.","mode:",meta->info->mode,"Imaging mode");
	            if (version>0.6) coniIO_str(coni,"extra.","processor:",meta->info->processor,"Name & Version of SAR Processor");
	            if (version>0.7)
                     {
 		      coniIO_int(coni,"extra.","orbit:",&meta->info->orbit,"Orbit Number for this datatake");
		      coniIO_double(coni,"extra.","bitErrorRate:",&meta->info->bitErrorRate,"Bit Error Rate");
		      coniIO_str(coni,"extra.","satBinTime:",meta->info->satBinTime,"Satellite Binary Time");
		      coniIO_str(coni,"extra.","satClkTime:",meta->info->satClkTime,"Satellite Clock Time (UTC)");
		      coniIO_double(coni,"extra.","prf:",&meta->info->prf,"Pulse Repition Frequency");
                     }
		  }
          }
	else if (meta->info!=NULL)/*Writing*/
          {
	    coniIO_structOpen(coni,"extra {","begin extra sensor information");
	    coniIO_str(coni,"extra.","sensor:",meta->info->sensor,"Imaging sensor");
	    coniIO_str(coni,"extra.","mode:",meta->info->mode,"Imaging mode");
      	    coniIO_str(coni,"extra.","processor:",meta->info->processor,"Name & Version of SAR Processor");
	    coniIO_int(coni,"extra.","orbit:",&meta->info->orbit,"Orbit Number for this datatake");
	    coniIO_double(coni,"extra.","bitErrorRate:",&meta->info->bitErrorRate,"Bit Error Rate");
	    coniIO_str(coni,"extra.","satBinTime:",meta->info->satBinTime,"Satellite Binary Time");
	    coniIO_str(coni,"extra.","satClkTime:",meta->info->satClkTime,"Satellite Clock Time (UTC)");
            coniIO_double(coni,"extra.","prf:",&meta->info->prf,"Pulse Repition Frequency");
            coniIO_structClose(coni,"end extra\n");
          }
			
 
}

void meta_write(meta_parameters *meta,const char *outName)
{
	char *meta_name=appendExt(outName,".meta");
	coniStruct *coni=coniOpen(meta_name,asciiOut);
	meta_io(coni,meta,0);
	coniClose(coni);
	free(meta_name);
}
void final_init(meta_parameters *meta);
meta_parameters *meta_read(const char *inName)
{
	char *meta_name=appendExt(inName,".meta");
	meta_parameters *meta=raw_init();
	coniStruct *coni=coniOpen(meta_name,asciiIn);
	meta_io(coni,meta,1);
	coniClose(coni);
	final_init(meta);
	free(meta_name);
	return meta;
}
