#ifndef _L_RDR_H
#define _L_RDR_H

static char sccsid_l_rdr_h[] = 
    "@(#)l_rdr.h	1.2 96/04/09 20:29:10";

/**********************************************************************/
/* leader file radiometric data record structure 'l_rdr.h'            */
/**********************************************************************/

typedef struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	rdr007[4];	/*Radiometric data record sequence number*/
char	rdr008[4];	/*# of radiometric data fields in  record*/

	/*RADIOMETRIC DATA SET*/
char	rdr009[8];	/*Radiometric data set size in bytes*/
char	rdr010[4];	/*SAR channel indicator*/
char	rdr011[4];	/*spare*/
char	rdr012[24];	/*Look up table designator */
char	rdr013[8];	/*Number of samples in the look up table*/
char	rdr014[16];	/*Sample type designator*/
char	rdr015[16];	/*Noise scale factor(a1)*/
char	rdr016[16];	/*Linear conversion factor(a2)*/
char	rdr017[16];	/*Offset conversion factor(a3)*/
char    rdr018[4];	/*spare*/
	/*LOOK UP TABLE VALUES*/
char	rdr019[4096];	/*16x256 samples*/
}L_RDR_FILE,*L_RDR_PTR;


#endif /* ! _L_RDR_H */
