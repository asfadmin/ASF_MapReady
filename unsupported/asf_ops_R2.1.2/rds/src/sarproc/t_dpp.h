#ifndef _T_DPP_H
#define _T_DPP_H

static char sccsid_t_dpp_h[] = 
    "@(#)t_dpp.h	1.2 96/04/09 20:29:12";

/**********************************************************************/
/*trailer file detailed processing parameter record structure't_dpp.h'*/
/**********************************************************************/

typedef struct t_dpp_str  {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	dpp007[4];	/*detailed processing parameters rec seq#*/
char	dpp008[4];	/*blanks*/
char	dpp009[1200];	/*ASCII characters*/
char 	dpp010[320];	/*160x16bit binary values*/
}T_DPP_FILE,*T_DPP_PTR;


#endif /* ! _T_DPP_H */
