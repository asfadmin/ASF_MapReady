#ifndef _L_FDR_H
#define _L_FDR_H

static char sccsid_l_fdr_h[] = 
    "@(#)l_fdr.h	1.2 96/04/09 20:29:09";

/**********************************************************************/
/*leader file descriptor record structure  'l_fdr.h'                  */
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
char	fdr029[6];	/*number of data set summary record*/
char	fdr030[6];	/*record length*/
char	fdr031[6];	/*number of map projection data records*/
char	fdr032[6];	/*record length*/
char	fdr033[6];	/*number of platform position data records*/
char	fdr034[6];	/*record length*/
char	fdr035[6];	/*number of attitude data records*/
char	fdr036[6];	/*record length*/
char	fdr037[6];	/*number of radiometric data records*/
char	fdr038[6];	/*record length*/
char	fdr039[6];	/*number of radiometric compensation records*/
char	fdr040[6];	/*record length*/
char	fdr041[6];	/*number of data quality summary records*/
char	fdr042[6];	/*record length*/
char	fdr043[6];	/*number of data histogram records*/
char	fdr044[6];	/*record length*/
char	fdr045[6];	/*number of range spectra records*/
char	fdr046[6];	/*record length*/
char	fdr047[6];	/*number of DEM descriptor records*/
char	fdr048[6];	/*record length*/
char	fdr049[6];	/*number of Radar parameter update records*/
char	fdr050[6];	/*record length*/
char	fdr051[6];	/*number of annotation data records*/
char	fdr052[6];	/*record length*/
char	fdr053[6];	/*number of detailed processing records*/
char	fdr054[6];	/*record length*/
char	fdr055[6];	/*number of calibration data records*/
char	fdr056[6];	/*record length*/
char	fdr057[6];	/*number of GCP records*/
char	fdr058[6];	/*record length*/
char	fdr059[6];	/*spare*/
char	fdr060[6];	/*spare*/
char	fdr061[6];	/*spare*/
char	fdr062[6];	/*spare*/
char	fdr063[6];	/*spare*/
char	fdr064[6];	/*spare*/
char	fdr065[6];	/*spare*/
char	fdr066[6];	/*spare*/
char	fdr067[6];	/*spare*/
char	fdr068[6];	/*spare*/
char	fdr069[6];	/*number of facility data records*/
char	fdr070[6];	/*record length*/
char	fdr071[288];	/*blanks*/
}L_FDR_FILE,*L_FDR_PTR;


#endif /* ! _L_FDR_H */
