static char sccsid_l_rcd_cmp_c[] =
    "@(#)l_rcd_cmp.c	1.3 96/04/09 20:48:49";

/* l_rcd_cmp(sp,pp,fp1) ---------------------------------------

**********************************************************************
*l_rcd_cmp.c fills in the CEOS leader radiometric compensation data  *
*record                                                              *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "l_rcd.h"
#include "scene_file.h"
#include "procfil.h"
#include "procdec.h"

extern char DEF_PATH[80];	/* default files */


#define L_RCD_SIZE	36	


l_rcd_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{



FILE *fp0;
L_RCD_FILE l_rcd_buf;

char	filename[80];

/*Open L_RCD_FILE0 */
sprintf(filename,"%sL_RCD_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_rcd_buf,L_RCD_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_RCD_FILE0 read error\n");
  }

fclose(fp0);

/*NO RADIOMETRIC COMPENSATION TABLE VALUES NEEDED*/


/*write L_RCD_FILE1*/
if(fwrite(&l_rcd_buf,L_RCD_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
