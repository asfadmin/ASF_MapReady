static char sccsid_l_fdr_cmp_c[] =
    "@(#)l_fdr_cmp.c	1.3 96/04/09 20:48:49";

/* l_fdr_cmp(sp,pp,fp1)  --------------------------------------

**********************************************************************
* l_fdr_cmp.c  "Leader File Descriptor"			             *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "l_fdr.h"
#include "procdec.h"
#include "procfil.h"
#include "scene_file.h"

extern char DEF_PATH[80];	/* default files */

#define L_FDR_SIZE	720

extern SCENE_FILE	sf;


l_fdr_cmp(sp,pp,fp1)
    TAPE_SEG_PTR	sp;
    PP_BLOCK_PTR	pp;
    FILE		*fp1;
{

FILE *fp0;
L_FDR_FILE l_fdr_buf;

int i;
char filename[80],s[13];

/* open the initialized file 'L_FDR_FILE0' for reading */
sprintf(filename,"%sL_FDR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r"))==NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }
if (fread(&l_fdr_buf,L_FDR_SIZE,1,fp0)!=1) {
  if(feof(fp0))
    return (FAIL);
  printf("File L_FDR_FILE0 read error!\n");
  }

fclose(fp0);


/*extract sf.sw_id and sf.asp_ver*/
/*load into fdr012[12]		software id and version*/
for (i=0;i<13;i++) s[i]=0;
strncpy(s,sf.sw_id,strlen(sf.sw_id));
strcat(s,sf.asp_ver);
strncpy(l_fdr_buf.fdr012,s,strlen(s));

/*extract sf.file_name*/
/*load into fdr014[16]		file name*/
strncpy(l_fdr_buf.fdr014,sf.image_id,strlen(sf.image_id));

/*write L_FDR_FILE1 */
if (fwrite(&l_fdr_buf,L_FDR_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
