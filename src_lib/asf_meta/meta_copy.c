#include "asf.h"
#include "asf_meta.h"

meta_parameters *meta_copy(meta_parameters *src)
{
	meta_parameters *ret = raw_init();
	
	ret->meta_version = src->meta_version;

	strncpy (ret->general->sensor,   src->general->sensor, FIELD_STRING_MAX-1);
	strncpy (ret->general->mode,     src->general->mode, MODE_FIELD_STRING_MAX-1);
	strncpy (ret->general->processor, src->general->processor, FIELD_STRING_MAX-1);
	strncpy (ret->general->data_type, src->general->data_type, FIELD_STRING_MAX-1);
	strncpy (ret->general->system,   src->general->system, FIELD_STRING_MAX-1);
	ret->general->orbit            = src->general->orbit;
	ret->general->orbit_direction  = src->general->orbit_direction;
	ret->general->frame            = src->general->frame;
	ret->general->band_number      = src->general->band_number;
	ret->general->line_count       = src->general->line_count;
	ret->general->sample_count     = src->general->sample_count;
	ret->general->start_line       = src->general->start_line;
	ret->general->start_sample     = src->general->start_sample;
	ret->general->x_pixel_size     = src->general->x_pixel_size;
	ret->general->y_pixel_size     = src->general->y_pixel_size;
	ret->general->center_latitude  = src->general->center_latitude;
	ret->general->center_longitude = src->general->center_longitude;
	ret->general->re_major         = src->general->re_major;
	ret->general->re_minor         = src->general->re_minor;
	ret->general->bit_error_rate   = src->general->bit_error_rate;
	ret->general->missing_lines    = src->general->missing_lines;

	ret->sar->image_type              = src->sar->image_type; 
	ret->sar->look_direction          = src->sar->look_direction;
	ret->sar->look_count              = src->sar->look_count;
	ret->sar->deskewed                = src->sar->deskewed;
	ret->sar->original_line_count     = src->sar->original_line_count;
	ret->sar->original_sample_count   = src->sar->original_sample_count;
	ret->sar->line_increment          = src->sar->line_increment;
	ret->sar->sample_increment        = src->sar->sample_increment;
	ret->sar->range_time_per_pixel    = src->sar->range_time_per_pixel;
	ret->sar->azimuth_time_per_pixel  = src->sar->azimuth_time_per_pixel;
	ret->sar->slant_shift             = src->sar->slant_shift;
	ret->sar->time_shift              = src->sar->time_shift;
	ret->sar->slant_range_first_pixel = src->sar->slant_range_first_pixel;
	ret->sar->wavelength              = src->sar->wavelength;
	ret->sar->prf                     = src->sar->prf;
	strncpy (ret->sar->satellite_binary_time,   src->sar->satellite_binary_time, FIELD_STRING_MAX-1);
	strncpy (ret->sar->satellite_clock_time,    src->sar->satellite_clock_time, FIELD_STRING_MAX-1);
	ret->sar->range_doppler_coefficients[0]   = src->sar->range_doppler_coefficients[0];
	ret->sar->range_doppler_coefficients[1]   = src->sar->range_doppler_coefficients[1];
	ret->sar->range_doppler_coefficients[2]   = src->sar->range_doppler_coefficients[2];
	ret->sar->azimuth_doppler_coefficients[0] = src->sar->azimuth_doppler_coefficients[0]; 
	ret->sar->azimuth_doppler_coefficients[1] = src->sar->azimuth_doppler_coefficients[1]; 
	ret->sar->azimuth_doppler_coefficients[2] = src->sar->azimuth_doppler_coefficients[2]; 

	if (src->projection) {
        	ret->projection = (meta_projection*)MALLOC(sizeof(meta_projection));
		ret->projection->type   = src->projection->type;
		ret->projection->startX = src->projection->startX;
		ret->projection->startY = src->projection->startY;
		ret->projection->perX   = src->projection->perX;
		ret->projection->perY   = src->projection->perY;
                strncpy (ret->projection->units, src->projection->units,12);
		ret->projection->hem      = src->projection->hem;
		ret->projection->re_major = src->projection->re_major;
		ret->projection->re_minor = src->projection->re_minor;
		switch (ret->projection->type) {
		    case 'A':/* Along-track/cross-track.*/
			ret->projection->param.atct.rlocal = src->projection->param.atct.rlocal;
			ret->projection->param.atct.alpha1 = src->projection->param.atct.alpha1;
			ret->projection->param.atct.alpha2 = src->projection->param.atct.alpha2;
			ret->projection->param.atct.alpha3 = src->projection->param.atct.alpha3;
			break;
		    case 'L':
			ret->projection->param.lambert.plat1 = src->projection->param.lambert.plat1;
			ret->projection->param.lambert.plat2 = src->projection->param.lambert.plat2;
			ret->projection->param.lambert.lat0 = src->projection->param.lambert.lat0;
			ret->projection->param.lambert.lon0 = src->projection->param.lambert.lon0;
			break;
		    case 'P':
			ret->projection->param.ps.slat = src->projection->param.ps.slat;
			ret->projection->param.ps.slon = src->projection->param.ps.slon;
			break;
		    case 'U':
			ret->projection->param.utm.zone = src->projection->param.utm.zone;
			break;
		    default:
                	printf("meta_copy: Unrecognized projection type '%c' in meta structure.\n"
                               "           Projection specific data has not been copied.\n",ret->projection->type);
                        break;
		}
	}

	if (src->state_vectors) {
		int ii;
                ret->state_vectors = (meta_state_vectors*)MALLOC(sizeof(meta_state_vectors));
		ret->state_vectors->year         = src->state_vectors->year;
		ret->state_vectors->julDay       = src->state_vectors->julDay;
		ret->state_vectors->second       = src->state_vectors->second;
		ret->state_vectors->vector_count = src->state_vectors->vector_count;
		ret->state_vectors->num          = src->state_vectors->num;
		ret->state_vectors->vecs = (state_loc*) MALLOC(sizeof(state_loc) * ret->state_vectors->vector_count);
		for (ii = 0; ii < ret->state_vectors->vector_count; ii++ ) {
			ret->state_vectors->vecs[ii].time      = src->state_vectors->vecs[ii].time;
			ret->state_vectors->vecs[ii].vec.pos.x = src->state_vectors->vecs[ii].vec.pos.x;
			ret->state_vectors->vecs[ii].vec.pos.y = src->state_vectors->vecs[ii].vec.pos.y;
			ret->state_vectors->vecs[ii].vec.pos.z = src->state_vectors->vecs[ii].vec.pos.z;
			ret->state_vectors->vecs[ii].vec.vel.x = src->state_vectors->vecs[ii].vec.vel.x;
			ret->state_vectors->vecs[ii].vec.vel.y = src->state_vectors->vecs[ii].vec.vel.y;
			ret->state_vectors->vecs[ii].vec.vel.z = src->state_vectors->vecs[ii].vec.vel.z;
		}
	}

/************************* Structures not yet in use *************************
	if (src->optical) {
		ret->optical->cloud_pct      = src->optical->cloud_pct;
		ret->optical->sun_az_angle   = src->optical->sun_az_angle;
		ret->optical->sun_elev_angle = src->optical->sun_elev_angle;
	}
	if (src->thermal) {
		ret->thermal->band_gain        = src->thermal->band_gain;
		ret->thermal->band_gain_change = src->thermal->band_gain_change;
		ret->thermal->day              = src->thermal->day;
	}
	if (src->stats) {
		ret->stats->max  = src->stats->max;
		ret->stats->min  = src->stats->min;
		ret->stats->mean = src->stats->mean;
		ret->stats->rms  = src->stats->rms;
		ret->stats->std_deviation = src->stats->std_deviation;
	}
*****************************************************************************/

/* Copy Depricated structures*/
	ret->geo->type        = src->geo->type;
	ret->geo->proj        = src->geo->proj;
	ret->geo->lookDir     = src->geo->lookDir;
	ret->geo->deskew      = src->geo->deskew;
	ret->geo->xPix        = src->geo->xPix;
	ret->geo->yPix        = src->geo->yPix;
	ret->geo->rngPixTime  = src->geo->rngPixTime;
	ret->geo->azPixTime   = src->geo->azPixTime;
	ret->geo->timeShift   = src->geo->timeShift;
	ret->geo->slantShift  = src->geo->slantShift;
	ret->geo->slantFirst  = src->geo->slantFirst;
	ret->geo->wavelen     = src->geo->wavelen;
	ret->geo->dopRange[0] = src->geo->dopRange[0];
	ret->geo->dopRange[1] = src->geo->dopRange[1];
	ret->geo->dopRange[2] = src->geo->dopRange[2];
	ret->geo->dopAz[0]    = src->geo->dopAz[0];
	ret->geo->dopAz[1]    = src->geo->dopAz[1];
	ret->geo->dopAz[2]    = src->geo->dopAz[2];

	ret->ifm->er            = src->ifm->er;
	ret->ifm->ht            = src->ifm->ht;
	ret->ifm->nLooks        = src->ifm->nLooks;
	ret->ifm->orig_nLines   = src->ifm->orig_nLines;
	ret->ifm->orig_nSamples = src->ifm->orig_nSamples;
	ret->ifm->lookCenter    = src->ifm->lookCenter;

	if (src->info) {
		ret->info = (extra_info*)MALLOC(sizeof(extra_info));
		strncpy (ret->info->sensor,     src->info->sensor, FIELD_STRING_MAX-1);
		strncpy (ret->info->mode,       src->info->mode, MODE_FIELD_STRING_MAX-1);
		strncpy (ret->info->processor,  src->info->processor, FIELD_STRING_MAX-1);
		ret->info->orbit              = src->info->orbit;
		ret->info->bitErrorRate       = src->info->bitErrorRate;
		strncpy (ret->info->satBinTime, src->info->satBinTime, FIELD_STRING_MAX-1);
		strncpy (ret->info->satClkTime, src->info->satClkTime, FIELD_STRING_MAX-1);
		ret->info->prf                = src->info->prf;
	}

/* Set depricated redundant stVec to point at new state vectors structure */
	ret->stVec = ret->state_vectors;

	return ret;
}
