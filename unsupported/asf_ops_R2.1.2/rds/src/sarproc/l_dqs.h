#ifndef _L_DQS_H
#define _L_DQS_H

static char sccsid_l_dqs_h[] = 
    "@(#)l_dqs.h	1.2 96/04/09 20:29:08";

/**********************************************************************/
/* leader file data quality summary record structure 'l_dqs.h'        */
/**********************************************************************/

typedef struct {


unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	dqs007[4];	/*Data summary quality record sequence number*/
char	dqs008[4];	/*SAR channel indicator*/
char	dqs009[6];	/*Date of last calibration update as YYMMDD*/
			/*YY = last two digits of year*/
			/*MM = month of the year*/
			/*DD = day of the month*/
char	dqs010[4];	/*Number of channels*/

/*ABSOLUTE RADIOMETRIC DATA QUALITY*/

char	dqs011[16];	/*Nominal ISLR (dB)*/
char	dqs012[16];	/*Nominal PSLR (dB)*/
char	dqs013[16];	/*Nominal azimuth ambiguity*/
char	dqs014[16];	/*Nominal range ambiguity*/
char	dqs015[16];	/*Estimate of SNR (from range spectra)*/
char	dqs016[16];	/*Actual Bit Error Rate (BER)*/
char	dqs017[16];	/*Nominal slant range resolution (m)*/
char	dqs018[16];	/*Nominal azimuth resolution (meters)*/
char	dqs019[16];	/*Nominal radiometric resolution (dB)*/
char	dqs020[16];	/*Instantaneous dynamic range (dB)*/
char	dqs021[16];	/*Nominal absolute  radiometric calibration*/
			/*magnitude of uncertainty of SAR channel */
			/*indicated in field 8 (dB)*/
char	dqs022[16];	/*Nominal absolute  radiometric calibration*/
			/*magnitude of uncertainty of SAR channel */
			/*indicated in field 8 (deg)*/

	/*RELATIVE RADIOMETRIC DATA QUALITY*/
char	dqs023[512];	/*(32x16) blanks*/

	/*ABSOLUTE GEOMETRIC DATA QUALITY*/
char	dqs053[16];	/*Nominal abs loc error along track (meters)*/
char	dqs054[16];	/*Abs loc error cross track (meters)*/
char	dqs055[16];	/*Nominal geometric distortion scale */
			/*in line direction*/
char	dqs056[16];	/*Nominal geometric distortion scale */
			/*in pixel direction*/
char	dqs057[16];	/*Nominal geometric distortion skew*/
char	dqs058[16];	/*Scene orientation error*/
}L_DQS_FILE,*L_DQS_PTR;


#endif /* ! _L_DQS_H */
