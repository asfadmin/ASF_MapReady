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

/* couple of prototypes */
double lzDouble(char *granN,char *desiredParam,int *err);
int lzInt(char *granN,char *desiredParam,int *err);
/************************
Writes the satellite * fields into the given meta_parameters
*/
void updateMeta(bin_state *s,meta_parameters *meta,char *inN)
{
	char parN[255];
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
	sprintf(meta->sar->satellite_binary_time, "%f", s->time_code);
	strcpy (meta->sar->satellite_clock_time, "0");

	strcpy(meta->general->sensor, s->satName);
	strcpy(meta->general->mode, s->beamMode);
	strcpy(meta->general->processor, "ASF/LZ2RAW_FLYWHEEL");
	meta->general->data_type = BYTE;
	strcpy(meta->general->system, meta_get_system());
	meta->general->orbit = lzInt(parN,"prep_block.OrbitNr:",NULL);
/*****	meta->general->orbit_direction = FIXME: HUM, HOW TO DO THIS??*/
/*****	meta->general->frame = FIXME: HUM, HOW TO DO THIS??*/
	meta->general->band_number = 0;
	meta->general->line_count = s->nLines;
	meta->general->sample_count = s->nSamp;
	meta->general->start_line = 0;
	meta->general->start_sample = 0;
	meta->general->x_pixel_size = meta->sar->range_time_per_pixel * (speedOfLight/2.0);
	meta->general->y_pixel_size = meta->sar->azimuth_time_per_pixel * s->vel /*Orbital velocity*/
					* (s->re / (s->re+s->ht));               /*Swath velocity*/
/*****	meta->general->center_latitude = Gotten in main() function */
/*****	meta->general->center_longitude = Gotten in main() function */
	if (meta->general->re_major != meta->general->re_major) /* See if its NaN */
/*FIXME*/	meta->general->re_major = 6378137.0;/*Would be better if this weren't hardcoded*/
	if (meta->general->re_minor != meta->general->re_minor) /* See if its NaN */
/*FIXME*/	meta->general->re_minor = 6356752.31414;/*Would be better if this weren't hardcoded*/
	meta->general->bit_error_rate = lzDouble(parN,"prep_block.bit_error_rate:",NULL);
	meta->general->missing_lines = lzDouble(parN,"prep_block.missing_lines:",NULL);
	
/* temporary fix for earth radius and satellite height */
	s->re = meta_get_earth_radius(meta, s->nLines/2, s->nSamp/2);
	s->ht = meta_get_sat_height(meta, s->nLines/2, s->nSamp/2) - s->re;
}

/********************************
AddStateVector:
	Updates the Earth Radius, spacecraft hieght,
velocity, etc. using the given state vector.
Format:
	stVec[0-2]: Earth-Fixed position, in meters.
	stVec[3-5]: Earth-Fixed velocity, in meters/second.

*/
/*** THIS FUNCTION IS PROBABLY NOT NEEDED AND NEEDS TO BE LOOKED AT ***********/
void addStateVector(bin_state *s,stateVector *stVec)
{
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
	Writes the given AGC value (floating-point amplitude
amplification that should be applied to this image) and 
window position (starting offset of this row in pixels) to
the .fmt file. Called by decoding routines.
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



