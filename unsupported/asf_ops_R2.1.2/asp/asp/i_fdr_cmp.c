/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* CLW 12-08-95 Fill in NSAMP & NBYTE for CPX & CSD */
/*  EYC 2-23-96 - Changed field 39 to reflect actual pixel count  */
/*  EYC 2-27-96 - added i_prefix_cmp to preset 192 byte prefix  */

/* i_fdr_cmp(ip,linelen) ------------------------------------

*********************************************************************
* i_fdr_cmp.c  "Imagery Options File Descriptor"		    *
*********************************************************************

*/

#include <stdio.h>
#include <netinet/in.h>
#include <procdec.h>
#include <procfil.h>
#include <string.h>
#include "i_fdr.h"
#include <scene_file.h>

#define PREFIX1_SIZE	12
#define PREFIX2_SIZE	180
#define CEOS_PREFIX_SIZE (PREFIX1_SIZE+PREFIX2_SIZE)
#define I_FDR_SIZE	449

extern RQST_PTR Cur_Rqst;	/* current job request */
extern SCENE_FILE	sf;
extern int ap_rmlines;
extern float prf;

i_fdr_cmp(ip,linelen)
    I_FDR_FILE		*ip;
    int			linelen;
{

FILE *fp0;

char filename[80],t[10],s[13];
char *cp;
int i;
int i029,i030,i037,i039,i040,i047;

/* initialize the file area to blanks */
for (i=0, cp=(char *) ip; i<linelen+CEOS_PREFIX_SIZE; i++)
	*cp++ = ' ';

/* open the initialized file 'ceo_img_init' for reading */
sprintf(filename,"%sceos_img_init",PROC_PATH);
if((fp0 = fopen(filename,"r"))==NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }
if (fread(ip,I_FDR_SIZE,1,fp0)!=1) {
  if(feof(fp0))
    return (FAIL);
  printf("File I_FDR_FILE0 read error!\n");
  }

fclose(fp0);

/*extract sf.sw_id and sf.asp_ver*/
/*load into fdr012[12]		software id and version*/
for (i=0; i<13; i++) s[i] = 0;
strncpy(s,sf.sw_id,strlen(sf.sw_id));
strcat(s,sf.asp_ver);
strncpy(ip->fdr012,s,strlen(s));

/*extract sf.file_name*/
/*load into fdr014[16]		file name*/
sprintf(ip->fdr014,"%.2s%.5s%.9s", Cur_Rqst->take_id,
	&Cur_Rqst->take_id[5], Cur_Rqst->site );

/*set file sizes*/
ip->rec_len = ntohl(linelen+CEOS_PREFIX_SIZE);
  i029 = linelen;
  i030 = linelen;
  i037 = linelen;
  i039 = linelen;
  i = ap_rmlines;
  if( i==8192 ) i = 8191;

/*  Take out for ASP v5.27, put back in for 5.28 (ASF R1.4.7)
  if( linelen>=i ) i040 = i030 - i;
  else {
	i = ap_rmlines / 8;
	if( i==1024 ) i = 1023;
	i040 = i030 - i;
  }
*/
i040 = 0;
  i047 = linelen;
if (strcmp(Cur_Rqst->type,"CPX") == 0) {
  i029 = 12800;
  i030 = 8192;
  i037 = 12800;
  i039 = 2048;
  i040 = 0;
  i047 = 8192;
}
if (strcmp(Cur_Rqst->type,"CSD") == 0) {
  i029 = 26624;
  i030 = 11264;
  i037 = 26624;
  i039 = 5632;
  i040 = 0;
  i047 = 11264;
}
i030 += CEOS_PREFIX_SIZE;
sprintf(t,"%6d",i029);
strncpy(ip->fdr029,t,6);
sprintf(t,"%6d",i030);
strncpy(ip->fdr030,t,6);
sprintf(t,"%8d",i037);
strncpy(ip->fdr037,t,8);
sprintf(t,"%8d",i039);
strncpy(ip->fdr039,t,8);
sprintf(t,"%4d",i040);
strncpy(ip->fdr040,t,4);
sprintf(t,"%8d",i047);
strncpy(ip->fdr047,t,8);

/* set up format identifier and code */
if (strcmp(Cur_Rqst->type,"CPX") == 0) {
  strncpy(ip->fdr033,"   2",4);
  strncpy(ip->fdr034,"   4",4);
  strncpy(ip->fdr060,"COMPLEX INTEGER*4           ",28);
  strncpy(ip->fdr061,"CI*4",4);
}
else if (strcmp(Cur_Rqst->type,"CSD") == 0) {
  strncpy(ip->fdr033,"   2",4);
  strncpy(ip->fdr034,"   2",4);
  strncpy(ip->fdr060,"COMPLEX INTEGER*2           ",28);
  strncpy(ip->fdr061,"CI*2",4);
}

return (PASS);

}

/* i_prefix_cmp(ip) ------------------------------------

*********************************************************************
* i_prefix_cmp - set CEOS data records prefix fields                *
*********************************************************************

*/
i_prefix_cmp( ip )
char *ip;
{
    unsigned int i, j, k, *lp;
    float *fp;
    char cbuf[32];

    lp = (unsigned int *) ip;
    fp = (float *) ip;
    bzero( &ip[12], 180 );		/* Zero record */

    ip[4] = 50;			/* Field 2, REC_SUB1 */
    ip[6] = 18;			/* Field 4, REC_SUB2 */
    ip[7] = 20;			/* Field 5, REC_SUB3 */

    ip[19] = 1;			/* Field 8, REC_NUM */

    strncpy( cbuf, sf.tme_scene_ctr, 4 );	/* get year */
    cbuf[4] = 0;
    lp[9] = htonl( atoi( cbuf ) );		/* Field 13, ACQ_YEAR */
    strncpy( cbuf, &sf.tme_scene_ctr[4], 3 );	/* get day in year */
    cbuf[3] = 0;
    lp[10] = htonl( atoi( cbuf ) );	/* Field 14, ACQ_DAY */
    i = 0;
    strncpy( cbuf, &sf.tme_scene_ctr[8], 2 );    /* get hour in day */
    cbuf[2] = 0;
    i += atoi( cbuf );		/* add hour in day */
    i *= 60;			/* convert to minutes */
    strncpy( cbuf, &sf.tme_scene_ctr[11], 2 );    /* get minutes in hour */
    cbuf[2] = 0;
    i += atoi( cbuf );		/* add minutes in hour */
    i *= 60;			/* convert to seconds */
    strncpy( cbuf, &sf.tme_scene_ctr[14], 2 );    /* get seconds in minutes */
    cbuf[2] = 0;
    i += atoi( cbuf );		/* add seconds in minute */
    i *= 1000;			/* convert to milliseconds */
    lp[11] = htonl( i );	/* Field 15, ACQ_MSEC */

    fp[14] = prf;
    lp[14] = htonl( lp[14] );	/* Field 20, PRF */

/*  different fillings for different data types  */

    if (strcmp(Cur_Rqst->type,"CSD") == 0) {
	ip[5] = 10;		/* Field 3, REC_TYPE */
	i = 5632;
	lp[6] = htonl( i );	/* Field 10, N_DATA_PIXEL */
	ip[49] = 1;		/* Field 16, SAR_CHAN_IND */
	if( Cur_Rqst->take_id[0]!='J') ip[51]=2;/* Field 17, SAR_CHAN_CODE */
	if( Cur_Rqst->take_id[0]=='E' ){
	    ip[53] = 1;		/* Field 18, TRAN_POLAR */
	    ip[55] = 1;		/* Field 19, RECV_POLAR */
	}
/*
	fp[17] = 	Field 24, CHP_LEN
*/
	fp[25] = sf.lk_angle*1000000.0;		/* Field 32, ELE_NADIR */
	lp[25] = lp[26] = htonl( lp[25] );	/* Field 33, MEC_NADIR */
	lp[27] = 0;				/* Field 34, ELE_SQUINT */
	fp[28] = sf.squint*1000000.0;
	lp[28] = htonl( lp[28] );		/* Field 35, MEC_SQUINT */
	fp[29] = sf.r_near*1000.0;
	lp[29] = htonl( lp[29] );	/* Field 36, SR_FIRST */
	fp[30] = sf.wndw_pos*1000.0;
	lp[30] = htonl( lp[30] );	/* Field 37, DR_WINDOW */
    } else {
	ip[5] = 11;		/* Field 3, REC_TYPE */
	if( strcmp(Cur_Rqst->type,"CPX")== 0 ){
	    i = 2048;
	    lp[6] = htonl( i );
	} else {
	    i = ntohl( lp[2] ) - 192;	/* get record length */
	    if( i>=ap_rmlines ){
		j = ap_rmlines;
		if( j==8192 ) j = 8191;
	    } else {
		j = ( ap_rmlines+7 ) / 8;
		if( j==1024 ) j = 1023;
	    }
	    lp[6] = htonl( j );	/* Field 10, N_DATA_PIXEL */
	    k = i - j;
	    lp[7] = htonl( k );	/* Field 11, NRIGHT_PIXEL */
	}
	ip[35] = 0;		/* Field 12, SENSOR_UPDF */
	ip[49] = 1;		/* Field 16, SAR_CHAN_IND */
	ip[51] = 2;		/* Field 17, SAR_CHAN_CODE */
	ip[60] = ip[61] = ip[62] = ip[63] = 0x20;
	ip[131] = 1;		/* Field 38, GEO_UPDF */

	fp[16] = sf.r_near*1000.0;
	lp[16] = htonl( lp[16] );	/* Field 22, SR_FIRST */
	fp[17] = sf.r_mid*1000.0;
	lp[17] = htonl( lp[17] );	/* Field 23, SR_MID */
	fp[18] = sf.r_far*1000.0;
	lp[18] = htonl( lp[18] );	/* Field 24, SR_LST */
/*
	lp[33] = 	Field 39, LAT_FIRST
	lp[34] = 	Field 40, LAT_MID
	lp[35] = 	Field 41, LAT_LAST
	lp[36] = 	Field 42, LON_FIRST
	lp[37] = 	Field 43, LON_MID
	lp[38] = 	Field 44, LON_LAST
*/
	fp[45] = sf.trk_ang*1000000.0;
	lp[45] = htonl( lp[45] );	/* Field 51, HEADING */
    }
    return;
}
