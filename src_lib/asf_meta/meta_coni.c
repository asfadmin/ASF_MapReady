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
	coniIO_int   (coni,"state.","year:",  &state->year,  "Year of image start");
	coniIO_int   (coni,"state.","julDay:",&state->julDay,"Julian day of the year for image start");
	coniIO_double(coni,"state.","second:",&state->second,"Second of the day for image start");
	coniIO_int   (coni,"state.","num:",   &state->num,   "Number of state vectors below");
	for (i=0;i<state->num;i++)
	{
		coniIO_structOpen(coni,"vector {","begin a single state vector");
		coniIO_double(coni,"state.vector.","time:",&state->vecs[i].time,     "Time, relative to image start [s]");
		coniIO_double(coni,"state.vector.","x:",   &state->vecs[i].vec.pos.x,"X Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","y:",   &state->vecs[i].vec.pos.y,"Y Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","z:",   &state->vecs[i].vec.pos.z,"Z Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","vx:",  &state->vecs[i].vec.vel.x,"X Velocity, earth-fixed [m/s]");
		coniIO_double(coni,"state.vector.","vy:",  &state->vecs[i].vec.vel.y,"Y Velocity, earth-fixed [m/s]");
		coniIO_double(coni,"state.vector.","vz:",  &state->vecs[i].vec.vel.z,"Z Velocity, earth-fixed [m/s]");
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
	double        version = 1.0;
	meta_general *general = meta->general;
	meta_stats   *stats   = meta->stats;

	coniIO_double(coni,"","meta_version:",&version,"ASF APD Metadata File.\n");
	if ((version<1.0) && reading) {
	/* Read meta files predating version 1.0 fill meta structure 1.0 as best as possible*/
		meta_sar *sar = meta->sar = (meta_sar *)MALLOC(sizeof(meta_sar));

		coniIO_char(coni,"geo.","type:",&sar->proj_type,"Image type: [S=slant range; G=ground range; P=map projected]");
		if (sar->proj_type=='P') {
		/*Projection Parameters.*/
			meta_projection *projection = meta->projection = (meta_projection *)MALLOC(sizeof(meta_projection));
			coniIO_structOpen(coni,"proj {","Map Projection parameters");
			coniIO_char(coni,"geo.proj.","type:",&projection->type,"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
			coniIO_double(coni,"geo.proj.","startX:",&projection->startX,"Projection Coordinate at top-left, X direction");
			coniIO_double(coni,"geo.proj.","startY:",&projection->startY,"Projection Coordinate at top-left, Y direction");
			coniIO_double(coni,"geo.proj.","perX:",&projection->perX,"Projection Coordinate per pixel, X direction");
			coniIO_double(coni,"geo.proj.","perY:",&projection->perY,"Projection Coordinate per pixel, Y direction");
			coniIO_char(coni,"geo.proj.","hem:",&projection->hem,"Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
			if (version>0.8) {
			/*Read geoid from file*/
				coniIO_double(coni,"geo.proj.","re_major:",&projection->re_major,"Major (equator) Axis of earth (meters)");
				coniIO_double(coni,"geo.proj.","re_minor:",&projection->re_minor,"Minor (polar) Axis of earth (meters)");
			} else {
			/*Default: use the GEM-06 (Goddard Earth Model 6) Ellipsoid*/
				projection->re_major=6378144.0;
				projection->re_minor=6356754.9;
			}
			switch(projection->type) {
				case 'A':/*Along-track/cross-track projection.*/
					coniIO_double(coni,"geo.proj.","rlocal:",&projection->param.atct.rlocal,"Local earth radius [m]");
					coniIO_double(coni,"geo.proj.","atct_alpha1:",&projection->param.atct.alpha1,"at/ct projection parameter");
					coniIO_double(coni,"geo.proj.","atct_alpha2:",&projection->param.atct.alpha2,"at/ct projection parameter");
					coniIO_double(coni,"geo.proj.","atct_alpha3:",&projection->param.atct.alpha3,"at/ct projection parameter");
					break;
				case 'L':/*Lambert Conformal Conic projection.*/
					coniIO_double(coni,"geo.proj.","lam_plat1:",&projection->param.lambert.plat1,"Lambert first standard parallel");
					coniIO_double(coni,"geo.proj.","lam_plat2:",&projection->param.lambert.plat2,"Lambert second standard parallel");
					coniIO_double(coni,"geo.proj.","lam_lat:",&projection->param.lambert.lat0,"Lambert original latitude");
					coniIO_double(coni,"geo.proj.","lam_lon:",&projection->param.lambert.lon0,"Lambert original longitude");
					break;
				case 'P':/*Polar Stereographic Projection.*/
					coniIO_double(coni,"geo.proj.","ps_lat:",&projection->param.ps.slat,"Polar Stereographic reference Latitude");
					coniIO_double(coni,"geo.proj.","ps_lon:",&projection->param.ps.slon,"Polar Stereographic reference Longitude");
					break;
				case 'U':/*Universal Trasnverse Mercator Projection.*/
					coniIO_int(coni,"geo.proj.","utm_zone:",&projection->param.utm.zone,"UTM Zone Code");
					break;
				default:
					printf("ERROR! Unrecognized map projection code '%c!'\n",projection->type);
					exit(1);
			}
			coniIO_structClose(coni,"end proj");
		}
		coniIO_char(coni,"geo.","lookDir:",&sar->look_direction,"SAR Satellite look direction (normally R) [R=right; L=left]");
		coniIO_int(coni,"geo.","deskew:",&sar->deskewed,"Image moved to zero doppler? [1=yes; 0=no]");
		coniIO_double(coni,"geo.","xPix:",&general->xPix,"Pixel size in X direction [m]");
		coniIO_double(coni,"geo.","yPix:",&general->yPix,"Pixel size in Y direction [m]");
		coniIO_double(coni,"geo.","rngPixTime:",&sar->range_time_per_pixel,"Time/pixel, range (xPix/(c/2.0), or 1/fs) [s]");
		coniIO_double(coni,"geo.","azPixTime:",&sar->azimuth_time_per_pixel,"Time/pixel, azimuth (yPix/swathVel, or 1/prf) [s]");
		coniIO_double(coni,"geo.","slantShift:",&sar->slantShift,"Error correction factor, in slant range [m]");
		coniIO_double(coni,"geo.","timeShift:",&sar->timeShift,"Error correction factor, in time [s]");
		coniIO_double(coni,"geo.","slantFirst:",&sar->slant_range_first_pixel,"Slant range to first image pixel [m]");
		coniIO_double(coni,"geo.","wavelength:",&sar->wavelength,"SAR Carrier Wavelength [m]");
		coniIO_double(coni,"geo.","dopRangeCen:",&sar->range_doppler_coefficients[0],"Doppler centroid [Hz]");
		coniIO_double(coni,"geo.","dopRangeLin:",&sar->range_doppler_coefficients[1],"Doppler per range pixel [Hz/pixel]");
		coniIO_double(coni,"geo.","dopRangeQuad:",&sar->range_doppler_coefficients[2],"Doppler per range pixel sq. [Hz/(pixel^2)]");
		coniIO_double(coni,"geo.","dopAzCen:",&sar->azimuth_doppler_coefficients[0],"Doppler centroid [Hz]");
		coniIO_double(coni,"geo.","dopAzLin:",&sar->azimuth_doppler_coefficients[1],"Doppler per azimuth pixel [Hz/pixel]");
		coniIO_double(coni,"geo.","dopAzQuad:",&sar->azimuth_doppler_coefficients[2],"Doppler per azimuth pixel sq. [Hz/(pixel^2)]");
		coniIO_structClose(coni,"end geo\n");

	/*Interferometry parameters:*/
		coniIO_structOpen(coni,"ifm {","begin interferometry-related parameters");
		if (version>0.6)
			coniIO_int(coni,"ifm.","nLooks:",&sar->look_count,"Number of looks to take from SLC");
		coniIO_int(coni,"ifm.","orig_lines:",&general->line_count,"Number of lines in original image");
		coniIO_int(coni,"ifm.","orig_samples:",&general->sample_count,"Number of samples in original image");
		coniIO_structClose(coni,"end ifm\n");

	/*State Vectors:*/
		{ /*Check to see if the state vectors even exist.*/
		    int err,nVec;
		    nVec=coniInt(coni,"state.number:",&err);
		    coniReopen(coni);/*Seek back to beginning of file.*/
		    if (err==CONI_OK && nVec!=0) {
		    /*We have state vectors!*/
			    meta->stVec=raw_init_state(nVec);/*Allocate state vectors.*/
			    meta_io_state(coni,meta->stVec);/*And initialize them.*/
		    }
		}

	/*Extra Info:*/
		{ /* Check to see if extra info exists.*/
		    int err;
		    coniStr(coni,"extra.sensor:",&err);
		    coniReopen(coni);/*Seek back to beginning of file.*/
		    if (err==CONI_OK) {
			coniIO_str(coni,"extra.","sensor:",general->sensor,"Imaging sensor");
			if (version>0.7) coniIO_str(coni,"extra.","mode:",general->mode,"Imaging mode");
			if (version>0.6) coniIO_str(coni,"extra.","processor:",general->processor,"Name & Version of SAR Processor");
			if (version>0.7) {
 			  coniIO_int(coni,"extra.","orbit:",&general->orbit,"Orbit Number for this datatake");
			  coniIO_double(coni,"extra.","bitErrorRate:",&general->bit_error_rate,"Bit Error Rate");
			  coniIO_str(coni,"extra.","satBinTime:",sar->satellite_binary_time,"Satellite Binary Time");
			  coniIO_str(coni,"extra.","satClkTime:",sar->satellite_clock_time,"Satellite Clock Time (UTC)");
			  coniIO_double(coni,"extra.","prf:",&sar->prf,"Pulse Repition Frequency");
			}
		    }
		}

	/************** READ IN DDR **************/

	} /* End pre-1.0 version read */
	else
	{	
	/*General parameters.*/
		coniIO_structOpen(coni, "general {", "begin parameters generally used in remote sensing");
		coniIO_str   (coni,"general.", "sensor:",                general->sensor,               "Imaging sensor");
		coniIO_str   (coni,"general.", "mode:",                  general->mode,                 "Imaging mode");
		coniIO_str   (coni,"general.", "processor:",             general->processor,            "Name & Version of Processor");
		coniIO_str   (coni,"general.", "data_type:",             general->data_type,            "Type of samples (e.g. REAL*4)");
		coniIO_str   (coni,"general.", "system:",                general->system,               "System of samples (e.g. ieee-std)");
		coniIO_int   (coni,"general.", "orbit:",                &general->orbit,                "Orbit Number for this datatake");
		coniIO_int   (coni,"general.", "frame:",                &general->frame,                "Frame for this image [-1 if n/a]");
		coniIO_char  (coni,"gerenal.", "orbit_direction:",      &general->orbit_direction,      "Ascending 'A', or descending 'D'");
		coniIO_int   (coni,"general.", "original_line_count:",  &general->line_count,           "Number of lines in original image");
		coniIO_int   (coni,"general.", "original_sample_count:",&general->sample_count,         "Number of samples in original image");
		coniIO_int   (coni,"general.", "start_line:",           &general->start_line,           "First line relative to original image");
		coniIO_int   (coni,"general.", "start_sample:",         &general->start_sample,         "First sample relative to original image");
		coniIO_double(coni,"general.", "xPix:",                 &general->xPix,                 "Range pixel size [m]");
		coniIO_double(coni,"general.", "yPix:",                 &general->yPix,                 "Azimuth pixel size [m]");
		coniIO_double(coni,"general.", "center_latitude:",      &general->center_latitude,      "Approximate image center latitude");
		coniIO_double(coni,"general.", "center_longitude:",     &general->center_longitude,     "Approximate image center longitude");
		coniIO_double(coni,"general.", "re_major:",             &general->re_major,             "Major (equator) Axis of earth [m]");
		coniIO_double(coni,"general.", "re_minor:",             &general->re_minor,             "Minor (polar) Axis of earth [m]");
		coniIO_double(coni,"general.", "bit_error_rate:",       &general->bit_error_rate,       "Fraction of bits which are in error");
		coniIO_int   (coni,"general.", "missing_lines:",        &general->missing_lines,        "Number of missing lines in data take");
		coniIO_structClose(coni, "end general");

	/*SAR parameters*/
		if ( ((!reading)&&(meta->sar!=NULL)) || (reading)&&(/*SAR STRUCT IS IN META FILE*/ 1 ) )
		{
			meta_sar *sar = meta->sar;
			if (reading)
				{sar = meta->sar = (meta_sar *)MALLOC(sizeof(meta_sar));}
			coniIO_structOpen(coni,"sar {","begin parameters used specifically in SAR imaging");
			coniIO_char  (coni,"sar.","proj_type:",              &sar->proj_type,                      "Image type: [S=slant range; G=ground range; P=map projected]");
			coniIO_char  (coni,"sar.","look_direction:",         &sar->look_direction,                 "SAR Satellite look direction (normally R) [R=right; L=left]");
			coniIO_int   (coni,"sar.","look_count:",             &sar->look_count,                     "Number of looks to take from SLC");
			coniIO_double(coni,"sar.","look_angle:",             &sar->look_angle,                     "Angle SAR looks at earth [radians]");
			coniIO_int   (coni,"sar.","deskewed:",               &sar->deskewed,                       "Image moved to zero doppler? [1=yes; 0=no]");
			coniIO_double(coni,"sar.","range_time_per_pixel:",   &sar->range_time_per_pixel,           "Time per pixel in range [s]");
			coniIO_double(coni,"sar.","azimuth_time_per_pixel:", &sar->azimuth_time_per_pixel,         "Time per pixel in azimuth [s]");
			coniIO_double(coni,"sar.","slant_range_first_pixel:",&sar->slant_range_first_pixel,        "Slant range to first pixel [m]");
			coniIO_double(coni,"sar.","slantShift:",             &sar->slantShift,                     "Error correction factor, in slant range [m]");
			coniIO_double(coni,"sar.","timeShift:",              &sar->timeShift,                      "Error correction factor, in time [s]");
			coniIO_double(coni,"sar.","wavelength:",             &sar->wavelength,                     "SAR carrier wavelength [m]");
			coniIO_double(coni,"sar.","prf:",                    &sar->prf,                            "Pulse Repition Frequency");
			coniIO_double(coni,"sar.","dopRangeCen:",            &sar->range_doppler_coefficients[0],  "Range doppler centroid [Hz]");
			coniIO_double(coni,"sar.","dopRangeLin:",            &sar->range_doppler_coefficients[1],  "Range doppler per range pixel [Hz/pixel]");
			coniIO_double(coni,"sar.","dopRangeQuad:",           &sar->range_doppler_coefficients[2],  "Range doppler per range pixel sq. [Hz/(pixel^2)]");
			coniIO_double(coni,"sar.","dopAzCen:",               &sar->azimuth_doppler_coefficients[0],"Azimuth doppler centroid [Hz]");
			coniIO_double(coni,"sar.","dopAzLin:",               &sar->azimuth_doppler_coefficients[1],"Azimuth doppler per azimuth pixel [Hz/pixel]");
			coniIO_double(coni,"sar.","dopAzQuad:",              &sar->azimuth_doppler_coefficients[2],"Azimuth doppler per azimuth pixel sq. [Hz/(pixel^2)]");
			coniIO_str   (coni,"sar.","satBinTime:",              sar->satellite_binary_time,          "Satellite Binary Time");
			coniIO_str   (coni,"sar.","satClkTime:",              sar->satellite_clock_time,           "Satellite Clock Time (UTC)");
			coniIO_structClose(coni,"end sar");
		}

	/*Optical parameters ** NOT YET IN USE **/
		if (/****general->type=='O'****/ 0 )
		{
			meta_optical *optical = meta->optical;
			if (reading)
				{optical = meta->optical = (meta_optical *)MALLOC(sizeof(meta_optical));}
		}

	/*Thermal parameters*/
		if (/****general->type=='T'****/ 0 )
		{
			meta_thermal *thermal = meta->thermal;
			if (reading)
				{thermal = meta->thermal = (meta_thermal *)MALLOC(sizeof(meta_thermal));}
		}

	/*Projection Parameters.*/
		if (/**** sar->proj_type=='P' ****/ 1 )
		{
			meta_projection *projection = meta->projection;
			if (reading)
				{projection = meta->projection = (meta_projection *)MALLOC(sizeof(meta_projection));}
			coniIO_structOpen(coni,"projection {","Map Projection parameters");
			coniIO_char  (coni,"projection.","type:",    &projection->type,    "Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
			coniIO_double(coni,"projection.","startX:",  &projection->startX,  "Projection Coordinate at top-left, X direction");
			coniIO_double(coni,"projection.","startY:",  &projection->startY,  "Projection Coordinate at top-left, Y direction");
			coniIO_double(coni,"projection.","perX:",    &projection->perX,    "Projection Coordinate per pixel, X direction");
			coniIO_double(coni,"projection.","perY:",    &projection->perY,    "Projection Coordinate per pixel, Y direction");
			coniIO_str   (coni,"projection.","units:",    projection->units,   "Projection units: [meters, arcsec]");
			coniIO_char  (coni,"projection.","hem:",     &projection->hem,     "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
			coniIO_double(coni,"projection.","re_major:",&projection->re_major,"Major (equator) Axis of earth [m]");
			coniIO_double(coni,"projection.","re_minor:",&projection->re_minor,"Minor (polar) Axis of earth [m]");
			switch(projection->type) {
			    case 'A':/*Along-track/cross-track projection.*/
				coniIO_double(coni,"projection.","rlocal:",     &projection->param.atct.rlocal,"Local earth radius [m]");
				coniIO_double(coni,"projection.","atct_alpha1:",&projection->param.atct.alpha1,"at/ct projection parameter");
				coniIO_double(coni,"projection.","atct_alpha2:",&projection->param.atct.alpha2,"at/ct projection parameter");
				coniIO_double(coni,"projection.","atct_alpha3:",&projection->param.atct.alpha3,"at/ct projection parameter");
				break;
			    case 'L':/*Lambert Conformal Conic projection.*/
				coniIO_double(coni,"projection.","lam_plat1:",&projection->param.lambert.plat1,"Lambert first standard parallel");
				coniIO_double(coni,"projection.","lam_plat2:",&projection->param.lambert.plat2,"Lambert second standard parallel");
				coniIO_double(coni,"projection.","lam_lat:",  &projection->param.lambert.lat0, "Lambert original latitude");
				coniIO_double(coni,"projection.","lam_lon:",  &projection->param.lambert.lon0, "Lambert original longitude");
				break;
			    case 'P':/*Polar Stereographic Projection.*/
				coniIO_double(coni,"projection.","ps_lat:",&projection->param.ps.slat,"Polar Stereographic reference Latitude");
				coniIO_double(coni,"projection.","ps_lon:",&projection->param.ps.slon,"Polar Stereographic reference Longitude");
				break;
			    case 'U':/*Universal Transverse Mercator Projection.*/
				coniIO_int(coni,"projection.","utm_zone:",&projection->param.utm.zone,"UTM Zone Code");
				break;
			    default:
				printf("ERROR! Unrecognized map projection code '%c!'\n",projection->type);
				exit(1);
			}
			coniIO_structClose(coni,"end proj");
		}

	/* Statistics */
		coniIO_structOpen(coni,"stats {","Image statistics");
		coniIO_double(coni,"stats.", "max:",          &stats->max,          "Maximum image value");
		coniIO_double(coni,"stats.", "min:",          &stats->min,          "Minimum image value");
		coniIO_double(coni,"stats.", "mean:",         &stats->mean,         "Mean");
		coniIO_double(coni,"stats.", "rms:",          &stats->rms,          "Root mean squared");
		coniIO_double(coni,"stats.", "std_deviation:",&stats->std_deviation,"Standard deviation");
		coniIO_structClose(coni,"end stats");

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
	} /* End read/write meta version 1.0 */
	
} /* End meta_io() */


/* meta_io_old:
 * Reads/writes pre-1.0 metafiles and  pre-1.0 metastructures
 */
void meta_io_old(coniStruct *coni,meta_parameters *meta,int reading)
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
			
 
} /* End meta_io_old */


void meta_write_old(meta_parameters *meta,const char *outName)
{
	char *meta_name=appendExt(outName,".meta");
	coniStruct *coni=coniOpen(meta_name,asciiOut);
	meta_io_old(coni,meta,0);
	coniClose(coni);
	free(meta_name);
}
void meta_write(meta_parameters *meta,const char *outName)
{
	meta_write_old(meta, outName);
}


void final_init(meta_parameters *meta);
meta_parameters *meta_read_old(const char *inName)
{
	char *meta_name=appendExt(inName,".meta");
	meta_parameters *meta=raw_init();
	coniStruct *coni=coniOpen(meta_name,asciiIn);
	meta_io_old(coni,meta,1);
	coniClose(coni);
	final_init(meta);
	free(meta_name);
	return meta;
}
meta_parameters *meta_read(const char *inName)
{
	meta_read_old(inName);
}
