#ifndef _T_CDR_H
#define _T_CDR_H

static char sccsid_t_cdr_h[] = 
    "@(#)t_cdr.h	1.2 96/04/09 20:29:12";

/**********************************************************************/
/*trailer file calibration data record structure 't_cdr.h'            */
/**********************************************************************/


typedef struct  {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	cdr007[4];	/*calibration data record sequence number*/
char	cdr008[4];	/*blanks*/
char	cdr009[256];	/*ASCII characters*/
}T_CDR_FILE,*T_CDR_PTR;


#endif /* ! _T_CDR_H */
