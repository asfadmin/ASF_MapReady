static char sccsid_i_fdr_cmp_c[] =
    "@(#)i_fdr_cmp.c	1.3 96/04/09 20:48:47";

/* i_fdr_cmp(ip,linelen) ------------------------------------

*********************************************************************
* i_fdr_cmp.c  "Imagery Options File Descriptor"		    *
*********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "i_fdr.h"
#include "procdec.h"
#include "procfil.h"
#include "scene_file.h"

#define I_FDR_SIZE	449

extern RQST_PTR Cur_Rqst;	/* current job request */
extern SCENE_FILE	sf;

extern char DEF_PATH[80];	/* default files */


i_fdr_cmp(ip,linelen)
    I_FDR_FILE		*ip;
    int			linelen;
{

FILE *fp0;

char filename[80],t[10],s[13];
char *cp;
int i;
int i029,i030,i037,i039,i047;

/* initialize the file area to blanks */
for (i=0, cp=(char *) ip; i<linelen; i++)
  *cp++ = ' ';

/* open the initialized file 'I_FDR_FILE0' for reading */
sprintf(filename,"%sI_FDR_FILE0",DEF_PATH);
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
strncpy(ip->fdr014,sf.image_id,strlen(sf.image_id));

/*set file sizes*/
ip->rec_len = linelen;
  i029 = linelen;
  i030 = linelen;
  i037 = linelen;
  i039 = linelen;
  i047 = linelen;
if (strcmp(Cur_Rqst->type,"CPX") == 0) {
  i029 = 12800;
  i030 = 8192;
  i037 = 12800;
  i039 = 2048;
  i047 = 8192;
}
if (strcmp(Cur_Rqst->type,"CSD") == 0) {
  i029 = 26624;
  i030 = 11264;
  i037 = 26624;
  i039 = 5632;
  i047 = 11264;
}

sprintf(t,"%6d",i029);
strncpy(ip->fdr029,t,6);
sprintf(t,"%6d",i030);
strncpy(ip->fdr030,t,6);
sprintf(t,"%8d",i037);
strncpy(ip->fdr037,t,8);
sprintf(t,"%8d",i039);
strncpy(ip->fdr039,t,8);
sprintf(t,"%8d",i047);
strncpy(ip->fdr047,t,8);

/* set up format identifier and code */
if (strcmp(Cur_Rqst->type,"CPX") == 0) {
  strncpy(ip->fdr060,"COMPLEX INTEGER*4           ",28);
  strncpy(ip->fdr061,"CI*4",4);
}
if (strcmp(Cur_Rqst->type,"CSD") == 0) {
  strncpy(ip->fdr060,"COMPLEX INTEGER*2           ",28);
  strncpy(ip->fdr061,"CI*2",4);
}

return (PASS);

}
