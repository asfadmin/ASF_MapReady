static char sccsid_t_cdr_cmp_c[] =
    "@(#)t_cdr_cmp.c	1.3 96/04/09 20:48:50";

/* t_cdr_cmp(sp,pp,fp1) ------------------------------------------

**********************************************************************
*t_cmp_cmp.c fills in the CEOS trailer file calibration data record  *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "t_cdr.h"
#include "scene_file.h"
#include "procfil.h"
#include "procdec.h"

extern char DEF_PATH[80];	/* default files */


#define T_CDR_SIZE	276	


t_cdr_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{



FILE *fp0;
T_CDR_FILE t_cdr_buf;

char	filename[80];

/*Open T_CDR_FILE0 */
sprintf(filename,"%sT_CDR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&t_cdr_buf,T_CDR_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File T_CDR_FILE0 read error\n");
  }

fclose(fp0);


/*NO CALIBRATION VALUES NEEDED*/


/*write T_CDR_FILE1*/
if(fwrite(&t_cdr_buf,T_CDR_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
