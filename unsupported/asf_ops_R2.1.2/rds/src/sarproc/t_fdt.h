#ifndef _T_FDT_H
#define _T_FDT_H

static char sccsid_t_fdt_h[] = 
    "@(#)t_fdt.h	1.2 96/04/09 20:29:13";

/**********************************************************************/
/* trailer file descriptor record structure 't_fdt.h'                 */
/**********************************************************************/

typedef struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	fdt007[2];	/*$A -ASCII flag*/
char	fdt008[2];	/*$$ -blank*/
char	fdt009[12];	/*format control document ID*/
char	fdt010[2];	/*format control document version*/
char	fdt011[2];	/*record format revision level*/
char	fdt012[12];	/*<software.id.>*/
char	fdt013[4];	/*<nnnn> -file number (3 for BIL; 3nn for BSQ)*/
char	fdt014[16];	/*<file.name........>*/
char	fdt015[4];	/*record sequence & location type flag*/
char	fdt016[8];	/*sequence number location*/
char	fdt017[4];	/*sequence number field length*/
char	fdt018[4];	/*record code & location type flag*/
char	fdt019[8];	/*record code location*/
char	fdt020[4];	/*record code field length*/
char	fdt021[4];	/*record length & location type flag*/
char	fdt022[8];	/*record length location*/
char	fdt023[4];	/*record length field length*/
char	fdt024[1];	/*blank*/
char	fdt025[1];	/*blank*/
char	fdt026[1];	/*blank*/
char	fdt027[1];	/*blank*/
char	fdt028[64];	/*blanks*/
char    fdt029[6];	/*# of data set summary record*/
char    fdt030[6];	/*record length*/
char    fdt031[6];	/*# of map projection datarecords*/
char    fdt032[6];	/*record length*/
char    fdt033[6];	/*# of platform position data records*/
char    fdt034[6];	/*record length*/
char    fdt035[6];	/*# of attitude data records*/
char    fdt036[6];	/*record length*/
char    fdt037[6];	/*# of radiometric data records*/
char    fdt038[6];	/*record length*/
char    fdt039[6];	/*# of radiometric compensation records*/
char    fdt040[6];	/*record length*/
char    fdt041[6];	/*# of data quality summary*/
char    fdt042[6];	/*record length*/
char    fdt043[6];	/*# of data histograms*/
char    fdt044[6];	/*record length*/
char    fdt045[6];	/*# of range spectra records*/
char    fdt046[6];	/*record length*/
char    fdt047[6];	/*# of DEM descriptor records*/
char    fdt048[6];	/*record length*/
char    fdt049[6];	/*# of Radar par.update records*/
char    fdt050[6];	/*record length*/
char    fdt051[6];	/*# of annotation data records*/
char    fdt052[6];	/*record length*/
char    fdt053[6];	/*# of Detailed processing  records*/
char    fdt054[6];	/*record length*/
char    fdt055[6];	/*# of calibration data records*/
char    fdt056[6];	/*record length*/
char    fdt057[6];	/*# of GCP records*/
char    fdt058[6];	/*record length*/
char    fdt059[6];	/*spare*/
char    fdt060[6];	/*spare*/
char    fdt061[6];	/*spare*/
char    fdt062[6];	/*spare*/
char    fdt063[6];	/*spare*/
char    fdt064[6];	/*spare*/
char    fdt065[6];	/*spare*/
char    fdt066[6];	/*spare*/
char    fdt067[6];	/*spare*/
char    fdt068[6];	/*spare*/
char    fdt069[6];	/*# of facility data records*/
char    fdt070[6];	/*record length*/
char	fdt071[288];	/*blanks*/
}T_FDT_FILE,*T_FDT_PTR;


#endif /* ! _T_FDT_H */
