#ifndef _L_PPD_H
#define _L_PPD_H

static char sccsid_l_ppd_h[] = 
    "@(#)l_ppd.h	1.2 96/04/09 20:29:09";

/**********************************************************************/
/*leader file platform position data record structure 'l_ppd.h'       */
/**********************************************************************/


typedef struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	ppd007[32];	/*Orbital elements designator */
char	ppd008[16];	/*1st orbital element*/
char	ppd009[16];	/*2nd orbital element*/
char	ppd010[16];	/*3rd orbital element*/
char	ppd011[16];	/*4th orbital element*/
char	ppd012[16];	/*5th orbital element*/
char	ppd013[16];	/*6th orbital element*/
char	ppd014[4];	/*Number of data points*/
char	ppd015[4];	/*Year of data point. (YY)*/
char	ppd016[4];	/*Month of data point. (MM)*/
char	ppd017[4];	/*Day of data point. (DD)*/
char	ppd018[4];	/*Day in the year (GMT)*/
char	ppd019[22];	/*Seconds of day (GMT) of data*/
char	ppd020[22];	/*Time charerval between DATA points (sec)*/
char	ppd021[64];	/*Reference coordinate system */
char	ppd022[22];	/*Greenwich mean hour angle (degrees)*/
char	ppd023[16];	/*Along track position error (meters)*/
char	ppd024[16];	/*Across track position error (meters)*/
char	ppd025[16];	/*Radial position error (meters/sec)*/
char	ppd026[16];	/*Along track velocity error (meters/sec)*/
char	ppd027[16];	/*Across track velocity error (meters/sec)*/
char	ppd028[16];	/*Radial velocity error (degrees/sec)*/

	/*First positional data point*/
char	ppd029[66];	/*1st data point position vector*/
char	ppd030[66];	/*1st data point velocity vector */

	/*Second positional data point*/
char	ppd031[66];	/*second data point position vector*/
char	ppd032[66];	/*second data point velocity vector*/

	/*Third positional data point*/
char	ppd033[66];	/*third data point position vector*/
char	ppd034[66];	/*third data point velocity vector*/
}L_PPD_FILE,*L_PPD_PTR;


#endif /* ! _L_PPD_H */
