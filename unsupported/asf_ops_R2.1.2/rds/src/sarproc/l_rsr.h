#ifndef _L_RSR_H
#define _L_RSR_H

static char sccsid_l_rsr_h[] = 
    "@(#)l_rsr.h	1.2 96/04/09 20:29:10";

/**********************************************************************/
/* leader file range spectra record structure 'l_rsr.h'               */
/**********************************************************************/

typedef struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	rsr007[4];	/*Range spectra record sequence #*/
char	rsr008[4];	/*SAR channel indicator*/
char	rsr009[8];	/*# of spectra table data sets in record*/
char	rsr010[8];	/*Spectra table data set size*/

	/*RANGE SPECTRA DATA*/
char	rsr011[4];	/*# of range spectra data records required */
			/*to reconstitute the full spectra table */
char	rsr012[4];	/*Sequence # in the full spectra table */
			/*of the table contained in this record*/
char	rsr013[8];	/*Total # of samples in range direction*/
char	rsr014[8];	/*# of samples offset from first sample */
			/*in range line*/
char	rsr015[8];	/*# of range lines integrated for spectra*/
char	rsr016[16];	/*Center frequency of first spectra bin (Hz)*/
char	rsr017[16];	/*Center frequency of last spectra bin (Hz)*/
char	rsr018[16];	/*Minimum spectral power (dB)*/
char	rsr019[16];	/*Maximum spectral power (dB)*/
char	rsr020[16];	/*spare*/
char	rsr021[16];	/*spare*/

	/*SPECTRAL DATA TABLE VALUES*/
char	rsr022[8];	/*# of frequency bins in table (128)*/

char	rsr023[2048];	/*(16x128) spectral data values (dB)*/
}L_RSR_FILE,*L_RSR_PTR;
 

#endif /* ! _L_RSR_H */
