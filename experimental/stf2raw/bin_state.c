/****************************************************************
FUNCTION NAME:
Interface to the "bin_state" structure.
This structure holds the state information used
during the decoding/analysis of an LZP binary file.

Also contains metadata interfaces for same.

SYNTAX:

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

	????/??		Orion Lawlor - Original Creation
	10/2000		R. Gens      - Modified updateAGC_window to check for and
					ignore bit errors in RSAT AGC fields.
	6/2001		T. Logan     - Modified updateAGC_window to check for and
					ignore bit errors in window shift for ERS
	1/2003		P. Denny     - Updated to new meta structures

****************************************************************/

#include "decoder.h"
#include "missing.h"
#include "lzFetch.h"

/*********************************
new_bin_state:
	Allocate, set to defaults, and return
a new bin_state structure.
*/
bin_state *new_bin_state(void)
{
	bin_state *s=(bin_state *)MALLOC(sizeof(bin_state));
	strcpy(s->satName,"Unknown");
	strcpy(s->beamMode,"STD");
	s->zeroDopSteered=0;
	s->nPulseInAir=0;
	s->nSamp=0;
	s->nBeams=1;/*Normally, you have just one beam*/
	s->slope=0; /*chirp slope, Hz/sec.*/
	s->frequency=0; /*radar wavelength, in m.*/
	s->fs=0;/*Range sampling frequency, Hz*/
	s->prf=0;/*Pulse Repetition Freqency*/
	s->prf_code=-1;
	s->dwp=0;
	s->dwp_code=-1;
	s->range_gate=0;
	s->time_code=0;

	s->binary=NULL;
	s->curFrame=0;
	s->bytesPerFrame=0;
	s->bytesInFile=0;
	s->missing=NULL;

	s->nValid=0;
	s->estDop=0.0;
	s->I_BIAS=s->Q_BIAS=0.0;

	s->re=6363000.989; /*approximate earth radius at scene center.*/
	s->vel=7463.989; /*satellite velocity, m/s.*/
	s->ht=792000.989; /*satellite height above earth, m.*/
	s->pulsedur=0; /*chirp length, in sec.*/
	s->lookDir='R';
	s->azres=8.0;    /* Desired azimuth resolution (m)*/
	s->nLooks=5;     /* Number of looks to square up data */
	s->dotFMT=NULL;

	return s;
}
/************************
Delete_bin_state: destroys the given bin_state structure.
*/
void delete_bin_state(bin_state *s)
{
	strcpy(s->satName,"Deleted...");
	if (s->binary) FCLOSE(s->binary);
	if (s->dotFMT) FCLOSE(s->dotFMT);
	if (s->missing!=NULL) freeMissing(s);
	FREE(s);
}

/************************
Writes the satellite * fields into the given meta_parameters
*/
void updateMeta(bin_state *s,meta_parameters *meta,char *inN)
{
	char parN[256];

	strcat(strcpy(parN,inN),".par");
/*Update fields for which we have decoded header info.*/
	meta->sar->image_type = 'S';               /*Slant range product*/
	meta->sar->look_direction = s->lookDir;
	meta->sar->look_count = s->nLooks;
	meta->sar->deskewed = 0;
	meta->sar->original_line_count = s->nLines;
	meta->sar->original_sample_count = s->nSamp;
	meta->sar->line_increment = 1.0;
	meta->sar->sample_increment = 1.0;
	meta->sar->range_time_per_pixel = 1.0/s->fs;
	meta->sar->azimuth_time_per_pixel = 1.0/s->prf;
	meta->sar->slant_shift = 0.0;
	meta->sar->time_shift = 0.0;
	meta->sar->slant_range_first_pixel = s->range_gate*speedOfLight/2.0;
	meta->sar->wavelength = speedOfLight/s->frequency;
	meta->sar->prf = s->prf;
	meta->sar->earth_radius = s->re;
	meta->sar->satellite_height = s->re+s->ht;
	if (!meta_is_valid_string(meta->sar->satellite_binary_time)) {
		sprintf(meta->sar->satellite_binary_time, "%f", s->time_code);
	}
	if (!meta_is_valid_string(meta->sar->satellite_clock_time)) {
		strcpy (meta->sar->satellite_clock_time, "0");
	}

	strcpy(meta->general->sensor, s->satName);
	strcpy(meta->general->mode, s->beamMode);
	strcpy(meta->general->processor, "ASF/STF2RAW");
	meta->general->data_type = BYTE;
	strcpy(meta->general->system, meta_get_system());
	meta->general->orbit = lzInt(parN,"prep_block.OrbitNr:",NULL);
	if (meta->state_vectors->vecs[0].vec.vel.z > 0)
		meta->general->orbit_direction  = 'A';
	else if (meta->state_vectors->vecs[0].vec.vel.z < 0)
		meta->general->orbit_direction  = 'D';
/*FIXME* meta->general->frame = HOW TO DO THIS??*/
	meta->general->band_number = 0;
	meta->general->line_count = s->nLines;
	meta->general->sample_count = s->nSamp;
	meta->general->start_line = 0;
	meta->general->start_sample = 0;
	meta->general->x_pixel_size = meta->sar->range_time_per_pixel * (speedOfLight/2.0);
	meta->general->y_pixel_size = meta->sar->azimuth_time_per_pixel * s->vel /*Orbital velocity*/
					* (s->re / (s->re+s->ht));               /*Swath velocity*/
/* Set center latitude & longitude ** not a legit calculation yet **
 *	{
 * 	char buf[256], junk[256];
 *	char *timeStr;
 *	double upperLat, leftLon, lowerLat, rightLon;
 *	int done = FALSE;
 *	int ii;
 *
 *	timeStr=lzStr(parN,"prep_block.location[0].line_date:",NULL);
 *	** Do something with time string to get upper lat **
 *	FREE(timeStr);
 *	ii=0;
 *	** find last location block **
 *	while (!done) {
 *	   ii++;
 *	   sprintf(buf,"prep_block.location[%d].line_date:",ii);
 *	   if (!(timeStr=lzStr(parN,buf,NULL)))
 *	      { done = TRUE; }
 *	   FREE(timeStr);
 *	}
 *	sprintf(buf,"prep_block.location[%d].line_date:",(ii-1));
 *	timeStr=lzStr(parN,buf,NULL);
 *	** do something with time string to get lower lat **
 *	FREE(timeStr);
 *	meta->general->center_latitude = lowerLat + (upperLat-lowerLat)/2;
 *	meta->general->center_longitude = rightLon + (leftLon-rightLon)/2;
 *	}
 */
	if (!meta_is_valid_double(meta->general->re_major))
/*FIXME*/	meta->general->re_major = 6378137.0;/*Would be better if this weren't hardcoded*/
	if (!meta_is_valid_double(meta->general->re_minor))
/*FIXME*/	meta->general->re_minor = 6356752.31414;/*Would be better if this weren't hardcoded*/
	meta->general->bit_error_rate = lzDouble(parN,"prep_block.bit_error_rate:",NULL);
	meta->general->missing_lines = lzDouble(parN,"prep_block.missing_lines:",NULL);
}

/********************************
AddStateVector:
	Updates the Earth Radius, spacecraft hieght, velocity, etc. using the 
	given state vector.
Format:
	stVec[0-2]: Earth-Fixed position, in meters.
	stVec[3-5]: Earth-Fixed velocity, in meters/second.
*/
void addStateVector(bin_state *s,stateVector *stVec)
{
    double latCen;/*Geocentric latitude of state vector, in radians.*/
    double er;/*Radius of earth under state vector, in m.*/

    /* Use state vector to estimate latitude.
     ---------------------------------------*/
    latCen=atan(stVec->pos.z/
    	sqrt(stVec->pos.x*stVec->pos.x+stVec->pos.y*stVec->pos.y));

    /* Use the latitude to determine earth's (ellipsoidal) radius.
     -----------------------------------------------------------*/
    er=er_polar/sqrt(1-ecc2/(1+tan(latCen)*tan(latCen)));

    /* Now write all these parameters into satellite structure.
     --------------------------------------------------------*/
    s->re=er;
    s->ht=vecMagnitude(stVec->pos)-er;
    s->vel=vecMagnitude(stVec->vel);

    printf("Updating for more accurate earth radius (%.2f), \n"
    	   "height (%.2f), and velocity (%.2f).\n",
    	   s->re,s->ht,s->vel);

   /* Estimate the doppler value at beam center
    ------------------------------------------*/
   if (!s->zeroDopSteered)
   {
     GEOLOCATE_REC *g=init_geolocate(stVec);
     g->side=s->lookDir;
     g->lambda=speedOfLight/s->frequency;
     s->estDop=yaw2doppler(g,s->range_gate*speedOfLight/2.0,1.10)/s->prf;
     printf("   Estimated doppler: %f PRF\n",s->estDop);
     if (logflag) {
       sprintf(logbuf,"   Estimated doppler: %f PRF\n",s->estDop);
       printLog(logbuf);
     }
     free_geolocate(g);
   }
}

/********************************
updateAGC_window:
	Writes the given AGC value (floating-point amplitude amplification that
	should be applied to this image) and window position (starting offset of
	this row in pixels) to the .fmt file. Called by decoding routines.
*/
void updateAGC_window(bin_state *s,float amplify,float startOff)
{
	static float 	sto_amp=-1000.0,	/* last gain value used */
			sto_off=-1000.0,	/* last window shift used */
			sto_line=-1000.0,	/* last line to be used */
			diff=5.0;		/* difference in gain values */

	diff=abs((10*log(sto_amp*sto_amp)/log(10))-(10*log(amplify*amplify)/log(10)));
	if (s->dotFMT==NULL) return;/*Skip it if no .fmt file exists*/

	/* The DWP will only change by approximately 88 samples
	   at any given time, if it changes more, we assume it is
	   bit error and ignore it */
	if (0==strncmp(s->satName,"ERS",3))
	  {
		if ( (abs(abs(sto_off-startOff)-88.0)>1.0) &&
		    sto_off != -1000.0 && sto_amp == amplify) return;
	  }

	if ( (s->nLines!=sto_line) &&
	     ( ((sto_amp!=amplify)&&(diff<3)) || (sto_off!=startOff))  )

	{/*Amplification factor or window position has changed! Update .fmt*/
		sto_amp=amplify;
		sto_off=startOff;
		sto_line=s->nLines;

		if (!quietflag) printf("   Writing to Format file - line %i\n",s->nLines);

		fprintf(s->dotFMT,"%-8d %12.3f %10.5g\n",s->nLines,startOff,amplify);
		fflush(s->dotFMT);  /*Flush the format file*/
	}
}



