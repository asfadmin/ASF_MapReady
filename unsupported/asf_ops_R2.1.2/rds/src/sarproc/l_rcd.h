#ifndef _L_RCD_H
#define _L_RCD_H

static char sccsid_l_rcd_h[] = 
    "@(#)l_rcd.h	1.2 96/04/09 20:29:09";

/**********************************************************************/
/*leader file radiometric compensation data record structure 'l_rcd.h'*/
/**********************************************************************/

typedef struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	rcd007[4];	/*Radiometric compensation record Seq number*/
char	rcd008[4];	/*SAR channel indicator (1)*/
char	rcd009[8];	/*# of radiometric compensation data sets(0)*/
char	rcd010[8];	/*Compensation data set size (0)*/
}L_RCD_FILE,*L_RCD_PTR;


#endif /* ! _L_RCD_H */
