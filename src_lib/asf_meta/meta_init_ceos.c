/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
  1.5 - P. Denny.   8/02      Formatted for new meta structure
****************************************************************/
#include "asf.h"
#include <ctype.h>
#include "meta_init.h"

/************************************************************
 * ceos_init:
 * Reads SAR structure parameters from CEOS into existing
 * meta_parameters structure.  Calls the facility-specific
 * decoders below. */
void ceos_init(const char *in_fName,meta_parameters *meta)
{
	char fName[255],fac[50],sys[50],ver[50];
	ceos_description *ceos;
	int dataSize;		    /* Number of bytes per image pixel.*/

        /* Fetch the Data Set Summary Record and Image File Descriptor
	   record, which are always present.
         ------------------------------------------------------------*/
	struct dataset_sum_rec dssr;/* Data set summary record */
	struct IOF_VFDR iof;        /* Image File Descriptor Record */
	strcpy(fName,in_fName);
	get_dssr(fName,&dssr);
	strcpy(fName,in_fName);
	get_ifiledr(fName, &iof);
	
	ceos = get_ceos_description(fName);

	/* Extract everything we possibly can from the DSSR.
	 --------------------------------------------------*/
    /* Fill meta->general structure */
	strcpy(meta->general->sensor, dssr.mission_id);
	strtok(meta->general->sensor," ");/*Remove spaces from field.*/
	strcpy(meta->general->mode,  dssr.beam1);
	strcpy(fac,dssr.fac_id);strtok(fac," ");/*Remove spaces from field*/
	strcpy(sys,dssr.sys_id);strtok(sys," ");/*Remove spaces from field*/
	strcpy(ver,dssr.ver_id);strtok(ver," ");/*Remove spaces from field*/
	sprintf(meta->general->processor,"%s/%s/%s",fac,sys,ver);
	dataSize = (iof.bitssamp+7)/8;
	switch (dataSize) {
	    case 2:strcpy(meta->general->data_type,"INTEGER*2");break;
	    case 4:strcpy(meta->general->data_type,"INTEGER*4");break;
	    default:strcpy(meta->general->data_type,"BYTE");    break;
	}
#if defined(big_ieee)
	strcpy(meta->general->system,     "big_ieee");
#elif defined(lil_ieee)
	strcpy(meta->general->system,     "lil_ieee");
#elif defined(cray_float)
	strcpy(meta->general->system,     "cray_float");
#else
	strcpy(meta->general->system,     "???");
#endif
	meta->general->orbit            = atoi(dssr.revolution);
/**/	meta->general->frame            = 0; /*TEMPORARY SETTING, WILL EVENTUALLY CACLUATE FROM CENTER LAT*/
	meta->general->band_number      = 0;
	meta->general->orbit_direction  = dssr.asc_des[0];
	meta->general->line_count       = iof.numofrec;
	meta->general->sample_count     = -2147283648; /* Retrieved below by ceos_init_asf() */
	meta->general->start_line       = 0;
	meta->general->start_sample     = 0;
	meta->general->x_pixel_size     = dssr.pixel_spacing;
	meta->general->y_pixel_size     = dssr.line_spacing;
	meta->general->center_latitude  = dssr.pro_lat;
	meta->general->center_longitude = dssr.pro_long;
	meta->general->re_major         = (dssr.ellip_maj < 10000.0) ? dssr.ellip_maj*1000.0 : dssr.ellip_maj;
	meta->general->re_minor         = (dssr.ellip_min < 10000.0) ? dssr.ellip_min*1000.0 : dssr.ellip_min;
	meta->general->bit_error_rate   = 0.0;

    /* Fill meta->sar structure */
/**/	meta->sar->image_type = '?';
	meta->sar->look_direction = (dssr.clock_ang>=0.0) ? 'R' : 'L';
/*	meta->sar->look_count = -2147283648;  found below */
/**/	meta->sar->deskewed = -2147283648;
    {	struct data_hist_rec pdhr;
	strcpy(fName,in_fName);
	if ( get_dhr(fName, &pdhr) != -1 ) {
	    meta->sar->original_line_count   = (pdhr.data)->ns_lin;
	    meta->sar->original_sample_count = (pdhr.data)->ns_pix;
	}
	else {
	    meta->sar->original_line_count   = -2147283648;
	    meta->sar->original_sample_count = -2147283648;
	}
    }	meta->sar->line_increment = NAN;
	meta->sar->sample_increment = NAN;
	meta->sar->range_time_per_pixel   = dssr.n_rnglok
		/ (dssr.rng_samp_rate * get_units(dssr.rng_samp_rate,EXPECTED_FS));
	meta->sar->azimuth_time_per_pixel = dssr.n_azilok/dssr.prf;
/**/	meta->sar->slant_shift = NAN;
/**/	meta->sar->time_shift = NAN;
	meta->sar->slant_range_first_pixel = dssr.rng_gate
		* get_units(dssr.rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
	meta->sar->wavelength = dssr.wave_length * get_units(dssr.wave_length,EXPECTED_WAVELEN);
	meta->sar->prf        = dssr.prf;
	meta->sar->range_doppler_coefficients[0]   = dssr.crt_dopcen[0];
	meta->sar->range_doppler_coefficients[1]   = dssr.crt_dopcen[1];
	meta->sar->range_doppler_coefficients[2]   = dssr.crt_dopcen[2];
	meta->sar->azimuth_doppler_coefficients[0] = dssr.alt_dopcen[0];
	meta->sar->azimuth_doppler_coefficients[1] = dssr.alt_dopcen[1];
	meta->sar->azimuth_doppler_coefficients[2] = dssr.alt_dopcen[2];
	strcpy(meta->sar->satellite_binary_time,dssr.sat_bintim);
	strtok(meta->sar->satellite_binary_time," ");/*Remove spaces from field.*/
	strcpy(meta->sar->satellite_clock_time, dssr.sat_clktim);
	strtok(meta->sar->satellite_clock_time, " ");/*Remove spaces from field*/

    /* Fetch state vectors. */
	ceos_init_stVec(fName,ceos,meta);

    /* Decode facility-dependant values. */
	if (ceos->facility==ASF)
		ceos_init_asf(fName,ceos,meta);

    /* Set the number of looks correctly, must be done after fetching of stVecs & facility stuff */
	if (ceos->satellite==ERS) meta->sar->look_count = 5;
	else if (ceos->satellite==JERS) meta->sar->look_count = 3;
	else if (ceos->satellite==RSAT)
	{
		double looks, look;
		look = meta_look(meta, 0.0, (double)(meta->general->sample_count)/2.0);
		looks = ((meta->general->x_pixel_size / sin(look))
		          / meta->general->y_pixel_size) + 0.5;
		meta->sar->look_count = (int) looks;

		if (0==strncmp(meta->general->mode,"FN",2)) meta->sar->look_count = 1;
	}
}


/************************************************************
 * get_ceos_description:
 * Extract a ceos_description structure from given CEOS file.
 * This contains "meta-meta-"data, data about the CEOS, such
 * as the generating facility, a decoded product type, etc.*/
ceos_description *get_ceos_description(char *fName)
{
	char *versPtr,*satStr;
	char *prodStr,*procStr;
	ceos_description *ceos=(ceos_description *)MALLOC(sizeof(ceos_description));
/*Fetch DSSR*/
	get_dssr(fName,&ceos->dssr);
	
/*Determine the sensor.*/
	satStr=ceos->dssr.mission_id;
	if (0==strncmp(satStr,"E",1)) ceos->satellite=ERS;
	else if (0==strncmp(satStr,"J",1)) ceos->satellite=JERS;
	else if (0==strncmp(satStr,"R",1)) ceos->satellite=RSAT;
	else {
		printf("get_ceos_description Warning! Unknown sensor '%s'!\n",satStr);
		ceos->satellite=unknownSatellite;
	}
	
/*Determine the processor version.*/
	ceos->version=0.0;/*Default is zero.*/
	versPtr=ceos->dssr.ver_id;
	while (!isdigit(*versPtr)) versPtr++;
	sscanf(versPtr,"%lf",&ceos->version);

/*Set other fields to unknown (to be filled out by facility-specific
	init. routines)*/
	procStr=ceos->dssr.sys_id;
	prodStr=ceos->dssr.product_type;
	ceos->processor=unknownProcessor;
	ceos->product=unknownProduct;
	
/*Determine the facility that processed the data.*/
	if (0==strncmp(ceos->dssr.fac_id,"ASF",3))
	{/*Alaska SAR Facility Image*/
/*Determine the image type and processor ID.*/
		ceos->facility=ASF;
		if (0==strncmp(procStr,"ASP",3)) ceos->processor=ASP;
		else if (0==strncmp(procStr,"SPS",3)) ceos->processor=SPS;
		else if (0==strncmp(procStr,"PREC",3)) ceos->processor=PREC;
		else if (0==strncmp(procStr,"AISP",4)) ceos->processor=AISP;
		else if (0==strncmp(procStr,"PP",2)) ceos->processor=PP;
		else if (0==strncmp(procStr,"SP2",3)) ceos->processor=SP2;
		else if (0==strncmp(procStr,"AMM",3)) ceos->processor=AMM;
		else if ( 0==strncmp(procStr,"SKY",3) 
			  || 0==strncmp(procStr, "FOCUS", 5) ) {
		  /*Is VEXCEL level-0 processor, not ASF*/
			ceos->facility=VEXCEL;
			ceos->processor=FOCUS;
			ceos->product=CCSD;
			return ceos;
		} else {
			printf("get_ceos_description Warning! Unknown ASF processor '%s'!\n",procStr);
			ceos->processor=unknownProcessor;
		}
	
		if (0==strncmp(prodStr,"LOW",3)) ceos->product=LOW_REZ;
		else if (0==strncmp(prodStr,"FUL",3)) ceos->product=HI_REZ;
		else if (0==strncmp(prodStr,"SCANSAR",7)) ceos->product=SCANSAR;
		else if (0==strncmp(prodStr,"CCSD",4)) ceos->product=CCSD;
		else if (0==strncmp(prodStr,"COMPLEX",7)) ceos->product=CCSD;
		else {
			printf("get_ceos_description Warning! Unknown ASF product type '%s'!\n",prodStr);
			ceos->product=unknownProduct;
		}
		
	}
	else if (0==strncmp(ceos->dssr.fac_id,"ES",2))
	{/*European Space Agency Image*/
		ceos->facility=ESA;
		
		if (0==strncmp(prodStr,"SAR RAW SIGNAL",14)) ceos->product=CCSD;
		else {
			printf("Get_ceos_description Warning! Unknown ESA product type '%s'!\n",prodStr);
			ceos->product=unknownProduct;
		}
	} else
	{
		printf( "****************************************\n"
			"SEVERE WARNING!!!!  Unknown CEOS Facility '%s'!\n"
			"****************************************\n",
			ceos->dssr.fac_id);
		ceos->facility=unknownFacility;
	}
	
	return ceos;
}

