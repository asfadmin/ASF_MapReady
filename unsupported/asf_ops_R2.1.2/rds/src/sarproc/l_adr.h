#ifndef _L_ADR_H
#define _L_ADR_H

static char sccsid_l_adr_h[] = 
    "@(#)l_adr.h	1.2 96/04/09 20:29:08";


/**********************************************************************/
/* leader file attitude data record structure 'l_adr.h'               */
/**********************************************************************/




typedef struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	adr007[4];	/*Number of attitude data points*/

	/*First Attitude data set*/
char	adr008[4];	/*Day of the year (GMT)*/
char	adr009[8];	/*Millisecond of day (GMT)*/
char	adr010[4];	/*Pitch data quality flag*/
char	adr011[4];	/*Roll data quality flag*/
char	adr012[4];	/*Yaw data quality flag*/
char	adr013[14];	/*Pitch (degrees)*/
char	adr014[14];	/*Roll (degrees)*/
char	adr015[14];	/*Yaw (degrees)*/
char	adr016[4];	/*Pitch rate data quality flag*/
char	adr017[4];	/*Roll rate data quality flag*/
char	adr018[4];	/*Yaw rate data quality flag*/
char	adr019[14];	/*Pitch rate (degrees/sec)*/
char	adr020[14];	/*Roll rate (degrees/sec)*/
char	adr021[14];	/*Yaw rate (degrees/sec)*/

	/*Second Attitude data set*/
char	adr022[4];	/*Day of the year (GMT)*/
char	adr023[8];	/*Millisecond of day (GMT)*/
char	adr024[4];	/*Pitch data quality flag*/
char	adr025[4];	/*Roll data quality flag*/
char	adr026[4];	/*Yaw data quality flag*/
char	adr027[14];	/*Pitch (degrees)*/
char	adr028[14];	/*Roll (degrees)*/
char	adr029[14];	/*Yaw (degrees)*/
char	adr030[4];	/*Pitch rate data quality flag*/
char	adr031[4];	/*Roll rate data quality flag*/
char	adr032[4];	/*Yaw rate data quality flag*/
char	adr033[14];	/*Pitch rate (degrees/sec)*/
char	adr034[14];	/*Roll rate (degrees/sec)*/
char	adr035[14];	/*Yaw rate (degrees/sec)*/
	
	/*Third Attitude data set*/
char	adr036[4];	/*Day of the year (GMT)*/
char	adr037[8];	/*Millisecond of day (GMT)*/
char	adr038[4];	/*Pitch data quality flag*/
char	adr039[4];	/*Roll data quality flag*/
char	adr040[4];	/*Yaw data quality flag*/
char	adr041[14];	/*Pitch (degrees)*/
char	adr042[14];	/*Roll (degrees)*/
char	adr043[14];	/*Yaw (degrees)*/
char	adr044[4];	/*Pitch rate data quality flag*/
char	adr045[4];	/*Roll rate data quality flag*/
char	adr046[4];	/*Yaw rate data quality flag*/
char	adr047[14];	/*Pitch rate (degrees/sec)*/
char	adr048[14];	/*Roll rate (degrees/sec)*/
char	adr049[14];	/*Yaw rate (degrees/sec)*/
}L_ADR_FILE,*L_ADR_PTR;

#endif /* ! _L_ADR_H */
