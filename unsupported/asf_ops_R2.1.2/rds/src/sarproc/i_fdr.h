#ifndef _I_FDR_H
#define _I_FDR_H

static char sccsid_i_fdr_h[] = 
    "@(#)i_fdr.h	1.2 96/04/09 20:29:07";

/**********************************************************************/
/*imagery options file descriptor record structure  'i_fdr.h'         */
/**********************************************************************/

typedef  struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	fdr007[2];	/*A$-ACSII/E$-EBCDIC flag*/
char	fdr008[2];	/*blanks*/
char	fdr009[12];	/*format control document ID*/
char	fdr010[2];	/*format control document version*/
char	fdr011[2];	/*record format revision level*/
char	fdr012[12];	/*<software.id.> */
char	fdr013[4];	/*<nnnn>*/
char	fdr014[16];	/*<file.name........>*/
char	fdr015[4];	/*Record sequence & location type flag*/
char	fdr016[8];	/*sequence number location*/
char	fdr017[4];	/*sequence number field length*/
char	fdr018[4];	/*record code & location type flag*/
char	fdr019[8];	/*record code location*/
char	fdr020[4];	/*record code field length*/
char	fdr021[4];	/*record length & location type flag*/
char	fdr022[8];	/*record length location*/
char	fdr023[4];	/*record length field length*/
char	fdr024[1];	/*blank*/
char	fdr025[1];	/*blank*/
char	fdr026[1];	/*blank*/
char	fdr027[1];	/*blank*/
char	fdr028[64];	/*blank*/
char	fdr029[6];	/*number of SAR data records*/
char	fdr030[6];	/*record length*/
char	fdr031[24];	/*blank*/
/*sample group data*/
char	fdr032[4];	/*bits per sample or pixel*/
char	fdr033[4];	/*samples or pixels per data group*/
char	fdr034[4];	/*bytes per data group*/
char	fdr035[4];	/*justification & order of samples in group*/
/*SAR data in file*/
char	fdr036[4];	/*number of images or SAR channels*/
char	fdr037[8];	/*lines per data set (excl. border lines)*/
char	fdr038[4];	/*left border pixels per line*/
char	fdr039[8];	/*total pixels per line per band*/
char	fdr040[4];	/*right border pixels per line*/
char	fdr041[4];	/*# of top border scan lines*/
char	fdr042[4];	/*# of bottom border scan lines*/
char	fdr043[4];	/*interleaving indicator BIL, BSQ or BIP*/
/*record data in file*/
char	fdr044[2];	/*# of physical records per line*/
char	fdr045[2];	/*# of physical records per multiband line*/
char	fdr046[4];	/*length of prefix data per line (bytes)*/
char	fdr047[8];	/*# of bytes of sample data per line*/
char	fdr048[4];	/*length of suffix data per line (bytes)*/
char	fdr049[4];	/*prefix/suffix repeat flag*/
/*prefix/suffix data locators*/
char	fdr050[8];	/*blanks*/
char	fdr051[8];	/*blanks*/
char	fdr052[8];	/*blanks*/
char	fdr053[8];	/*blanks*/
char	fdr054[8];	/*blanks*/
char	fdr055[32];	/*blanks*/
char	fdr056[8];	/*blanks*/
char	fdr057[8];	/*blanks*/
char	fdr058[8];	/*blanks*/
char	fdr059[8];	/*blanks*/
/*pixel data description*/
char	fdr060[28];	/*data format type identifier*/
char	fdr061[4];	/*data format type code*/
char	fdr062[4];	/*# of left fill bits within pixel*/
char	fdr063[4];	/*$ of right fill bits within pixel*/
char	fdr064[8];	/*maximum data range of pixel (start from 0)*/
char	fdr065[1];	/*blanks (up to end of record; size varies)*/
} I_FDR_FILE, *I_FDR_PTR;

#endif /* ! _I_FDR_H */
