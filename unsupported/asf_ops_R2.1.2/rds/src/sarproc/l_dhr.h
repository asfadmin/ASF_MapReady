#ifndef _L_DHR_H
#define _L_DHR_H

static char sccsid_l_dhr_h[] = 
    "@(#)l_dhr.h	1.2 96/04/09 20:29:08";

/********************************************************************/
/* leader file data histogram record structure 'l_dhr.h'            */
/* modified: 10/2/91, dtc, SIS error in not repeating fields 11-20  */
/********************************************************************/

typedef struct l_dhr_str{

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	dhr007[4];	/*Data histograms record sequence number*/
char	dhr008[4];	/*SAR channel indicator*/
char	dhr009[8];	/*# of histogram table data sets in record*/
char	dhr010[8];	/*Histogram table data set size (bytes)*/

/* For field #1*/
	/*HISTOGRAM TABLE DATA SET DESCRIPTION*/
char	dhr011[32];	/*Histogram descriptor*/
char	dhr012[4];	/*# of histogram records needed to */
			/*reconstitute the full histogram table*/
char	dhr013[4];	/*Sequence number in the full histogram */
			/*table ofthe table contained in the record*/
char	dhr014[8];	/*Total # of table bins in the full histogram*/
			/*table*/
char	dhr015[8];	/*Total # of data samples in line directn (P)*/
char	dhr016[8];	/*Total # of data samples across lines (L)*/
char	dhr017[8];	/*Data samples group size in line directn (M)*/
char	dhr018[8];	/*Data samples group size across lines (N)*/
char	dhr019[8];	/*# of samples per group in line directn (k)*/
char	dhr020[8];	/*# of samples per group across lines (l)*/

	/*DATA STATISTICS*/
char	dhr021[16];	/*Min sample value of 1st. histogram table bin*/
char	dhr022[16];	/*Max sample value of last histogram table bin*/
char	dhr023[16];	/*Mean sample value*/
char	dhr024[16];	/*Standard deviation of sample value*/
char	dhr025[16];	/*Sample value increment*/

	/*DATA HISTOGRAM STATISTICS*/
char    dhr026[16];	/*Minimum histogram table value*/
char	dhr027[16];	/*Maximum histogram table value*/
char	dhr028[16];	/*Mean histogram table value*/
char	dhr029[16];	/*Standard deviation of histogram table*/
char	dhr030[8];	/*Histogram table size (256)*/
char	dhr031[2048];	/*(8x256) histogram table values*/


/* For field #2*/
	/*HISTOGRAM TABLE DATA SET DESCRIPTION*/
char	dhr032[32];	/*Histogram descriptor*/
char	dhr033[4];	/*# of histogram records needed to */
			/*reconstitute the full histogram table*/
char	dhr034[4];	/*Sequence number in the full histogram */
			/*table ofthe table contained in the record*/
char	dhr035[8];	/*Total # of table bins in the full histogram*/
			/*table*/
char	dhr036[8];	/*Total # of data samples in line directn (P)*/
char	dhr037[8];	/*Total # of data samples across lines (L)*/
char	dhr038[8];	/*Data samples group size in line directn (M)*/
char	dhr039[8];	/*Data samples group size across lines (N)*/
char	dhr040[8];	/*# of samples per group in line directn (k)*/
char	dhr041[8];	/*# of samples per group across lines (l)*/

	/*DATA STATISTICS*/
char	dhr042[16];	/*Min sample value of 1st. histogram table bin*/
char	dhr043[16];	/*Max sample value of last histogram table bin*/
char	dhr044[16];	/*Mean sample value*/
char	dhr045[16];	/*Standard deviation of sample value*/
char	dhr046[16];	/*Sample value increment*/


	/*DATA HISTOGRAM STATISTICS*/
char    dhr047[16];	/*Minimum histogram table value*/
char	dhr048[16];	/*Maximum histogram table value*/
char	dhr049[16];	/*Mean histogram table value*/
char	dhr050[16];	/*Standard deviation of histogram table*/
char	dhr051[8];	/*Histogram table size (256)*/
char	dhr052[2048];	/*(8x256) histogram table values*/
}L_DHR_FILE,*L_DHR_PTR;

#endif /* ! _L_DHR_H */
