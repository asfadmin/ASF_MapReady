
static char sccsid_t_fdt_cmp_c[] =
    "@(#)t_fdt_cmp.c	1.3 96/04/09 20:48:51";

/* t_fdt_cmp(sp,pp,fp1) ------------------------------------------

**********************************************************************
* t_fdt_cmp.c  "Leader File Descriptor"			             *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "procdec.h"
#include "procfil.h"
#include "t_fdt.h"
#include "scene_file.h"

extern char DEF_PATH[80];	/* default files */

#define T_FDT_SIZE	720

extern SCENE_FILE	sf;


t_fdt_cmp(sp,pp,fp1)
    TAPE_SEG_PTR	sp;
    PP_BLOCK_PTR	pp;
    FILE		*fp1;
{

FILE *fp0;
T_FDT_FILE t_fdt_buf;

int i;
char	filename[80],s[13];

/* open the initialized file 'T_FDT_FILE0' for reading */
sprintf(filename,"%sT_FDT_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r"))==NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }
if (fread(&t_fdt_buf,T_FDT_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File T_FDT_FILE0 read error!\n");
  }

fclose (fp0);


/*extract sf.sw_id and asp_ver*/
/*load into fdt012[12]		software id and version*/
for (i=0;i<13;i++) s[i] = 0;	/*set string to all null */
strncpy(s,sf.sw_id,strlen(sf.sw_id));
strcat(s,sf.asp_ver);
strncpy(t_fdt_buf.fdt012,s,strlen(s));

/*extract sf.file_name*/
/*load into fdt014[16]		file name*/
strncpy(t_fdt_buf.fdt014,sf.image_id,strlen(sf.image_id));

/*Open T_FDT_FILE1 for writing*/
if (fwrite(&t_fdt_buf,T_FDT_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
