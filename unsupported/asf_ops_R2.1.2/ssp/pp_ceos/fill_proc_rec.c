/* SccsId[]= @(#)fill_proc_rec.c	2.41 3/24/98 */
static char sccsid_fill_proc_rec[]= "@(#)PPfill_proc_rec.c:2.41";

#include <stdio.h>

#include "odl_file.h"
#include "input_file.h"
#include "externs.h"


int fill_processed_data_record(char *buf)
{
	typedef unsigned char B1;
	typedef unsigned short B2;
	typedef unsigned int B4;

	char s[20];


	static B4 rec_seq = 1;		/* Record sequence number */
	B1 rec_sub1 = 50;	/* File record sub-type code */
	B1 rec_type = 11;	/* Record type code */
	B1 rec_sub2 = 18;	/* Second record sub-type code */
	B1 rec_sub3 = 20;	/* Third record sub-type code */
	B4 length;	/* Length of this record */
	static B4 line_num;	/* Image data line number */
	B4 rec_num = 1;		/* Image data record index */
	B4 n_left_pixel = 0;	/* Left fill pixel count */
	B4 n_data_pixel;	/* Data pixel count */
	B4 n_right_pixel = 0;	/* Right fill pixel count */
	B4 sensor_updf = 1;	/* Sensor parameter update flag */
	B4 acq_year;		 /* Acquisition year */
	B4 acq_day;		 /* Acquisition day of year */
	B4 acq_msec;		 /* Acquisition msecs of day */
	B2 sar_chan_ind_bin = 1; /* SAR channel indicator */
	B2 sar_chan_code = 2;	 /* SAR channel code */
	B2 tran_polar = 0;	 /* Transmitted polarization */
	B2 recv_polar = 0;	 /* Received polarization */
	B4 prf_bin = data_set.prf;		 /* Pulse repetition frequency, Hz */
	B4 spare_pdr_1 = 0;	 /* Unused */
	B4 sr_first = facility_record.sl_rng_1st_pix * 1000.00;		 /* Slant range to first pixel, m */
	B4 sr_mid = facility_record.sl_rng_mid_pix * 1000.00;		 /* Slant range to mid pixel, m */
	B4 sr_lst = facility_record.sl_rng_last_pix * 1000.00;		 /* Slant range to last pixel, m */
	B4 fdc_first = 0;	 /* First pixel Doppler centroid, Hz */
	B4 fdc_mid = 0;		 /* Mid-pixel Doppler centroid, Hz */
	B4 fdc_last = 0;	 /* Last pixel Doppler centroid, Hz */
	B4 ka_first = 0;	/* First pixel azimuth FM rate, Hz */
	B4 ka_mid = 0; 		/* Mid-pixel azimuth FM rate, Hz */
	B4 ka_last = 0;		/* Last pixel azimuth FM rate, Hz */
	B4 nadir_ang = 0;	/* Nadir look angle */
	B4 squint_ang = 0;	/* Azimuth squint angle */
	B4 null_f = 0; 		/* Null line flag */
	B4 spare_pdr_2 = 0; 	/* Unused */
	B4 geo_updf = 1; 	/* Geographic ref parm update flag */
	B4 lat_first = 0; 		/* First pixel latitude */
	B4 lat_mid = 0; 		/* Mid-pixel latitude */
	B4 lat_last = 0; 		/* Last pixel latitude */
	B4 long_first = 0; 		/* First pixel longitude */
	B4 long_mid = 0; 		/* Mid pixel longitude */
	B4 long_last = 0; 		/* Last pixel longitude */
	B4 north_first = 0;	/* Northing of first pixel */
	B4 north_last = 0; 	/* Northing of last pixel */
	B4 east_first = 0;	/* Easting of first pixel */
	B4 spare_pdr_4 = 0;	/* Spare */
	B4 east_last = 0;	/* Easting of last pixel */
	B4 heading = 0; 		/* Line heading */
	B4 spare_pdr_5 = 0; 	/* Spare */

/******************************************************************************/

	if (!strcmp(odl_file->prod_type, "STANDARD")) {
		length = rec_length + 192;
		n_data_pixel = rec_length;
	}
	else if (!strcmp(odl_file->prod_type, "RAMP")) {
		length = rec_length + 192;
		n_data_pixel = rec_length / 2;
	}
	else {     /* COMPLEX */
		length = rec_length;
		n_data_pixel = (rec_length - 192) / 4;
	}


	rec_seq++;	
	memcpy(&buf[0], &rec_seq, 4); /* Record sequence number */

	memcpy(&buf[4], &rec_sub1, 1); /* First record sub-type code */
	memcpy(&buf[5], &rec_type, 1); /* Record type code */
	memcpy(&buf[6], &rec_sub2, 1); /* Second record sub-type code */
	memcpy(&buf[7], &rec_sub3, 1); /* Third record sub-type code */
	memcpy(&buf[8], &length, 4); /* Length of this record */

	line_num++;
	memcpy(&buf[12], &line_num, 4); /* Image data line number */

	memcpy(&buf[16], &rec_num, 4); /* Image data record index */
	memcpy(&buf[20], &n_left_pixel, 4); /* Left fill pixel count */
	memcpy(&buf[24], &n_data_pixel, 4); /* Data pixel count */
	memcpy(&buf[28], &n_right_pixel, 4); /* Right fill pixel count */
	memcpy(&buf[32], &sensor_updf, 4); /* Sensor parameter update flag */

	strcpy(s, facility_record.start_gmt);
	acq_year = atoi(strtok(s, "-")); 
	memcpy(&buf[36], &acq_year, 4); /* Acquisition year */

	acq_day = atoi(strtok(NULL, "T"));
	memcpy(&buf[40], &acq_day, 4); /* Acquisition day of year */
	
	
	acq_msec = atoi(strtok(NULL, ":")) * 3600000 +
			atoi(strtok(NULL, ":")) * 60000 +
			atoi(strtok(NULL, ".")) * 1000 +
			atoi(strtok(NULL, "\0"));

	memcpy(&buf[44], &acq_msec, 4); /* Acquisition msecs of day */

	memcpy(&buf[48], &sar_chan_ind_bin, 2); /* SAR channel indicator */
	memcpy(&buf[50], &sar_chan_code, 2); /* SAR channel code */
	memcpy(&buf[52], &tran_polar, 2); /* Transmitted polarization */
	memcpy(&buf[54], &recv_polar, 2); /* Received polarization */

	memcpy(&buf[56], &prf_bin, 4); /* Pulse repetition frequency, Hz */

	memcpy(&buf[60], &spare_pdr_1, 4); /* Unused */

	memcpy(&buf[64], &sr_first, 4); /* Slant range to first pixel, m */

	memcpy(&buf[68], &sr_mid, 4); /* Slant range to mid pixel, m */

	memcpy(&buf[72], &sr_lst, 4); /* Slant range to last pixel, m */

	memcpy(&buf[76], &fdc_first, 4); /* First pixel Doppler centroid, Hz */
	memcpy(&buf[80], &fdc_mid, 4); /* Mid-pixel Doppler centroid, Hz */
	memcpy(&buf[84], &fdc_last, 4); /* Last pixel Doppler centroid, Hz */
	memcpy(&buf[88], &ka_first, 4); /* First pixel azimuth FM rate, Hz */
	memcpy(&buf[92], &ka_mid, 4); /* Mid-pixel azimuth FM rate, Hz */
	memcpy(&buf[96], &ka_last, 4); /* Last pixel azimuth FM rate, Hz */
	memcpy(&buf[100], &nadir_ang, 4); /* Nadir look angle */
	memcpy(&buf[104], &squint_ang, 4); /* Azimuth squint angle */
	memcpy(&buf[108], &null_f, 4); /* Null line flag */ 
	memset(&buf[112], 0, 16);	/* Unused */
	memcpy(&buf[128], &geo_updf, 4); /* Geographic ref parm update flag */

	memcpy(&buf[132], &lat_first, 4); /* First pixel latitude */

	memcpy(&buf[136], &lat_mid, 4); /* Mid-pixel latitude */

	memcpy(&buf[140], &lat_last, 4); /* Last pixel latitude */

	memcpy(&buf[144], &long_first, 4); /* First pixel longitude */

	memcpy(&buf[148], &long_mid, 4); /* Mid pixel longitude */

	memcpy(&buf[152], &long_last, 4); /* Last pixel longitude */

	memcpy(&buf[156], &north_first, 4); /* Northing of first pixel */
	memcpy(&buf[160], &north_last, 4); /* Northing of last pixel */
	memcpy(&buf[164], &east_first, 4); /* Easting of first pixel */
	memcpy(&buf[168], &spare_pdr_4, 4); /* Spare */
	memcpy(&buf[172], &east_last, 4); /* Easting of last pixel */
	memcpy(&buf[176], &heading, 4); /* Line heading */
	memset(&buf[180], 0, 12); /* Spare */

	return 0;
}

