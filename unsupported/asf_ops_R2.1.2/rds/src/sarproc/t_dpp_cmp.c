static char sccsid_t_dpp_cmp_c[] =
    "@(#)t_dpp_cmp.c	1.3 96/04/09 20:48:50";

/* t_dpp_cmp(sp,pp,fp1) --------------------------------------

**********************************************************************
*t_dpp_cmp.c fills in the CEOS trailer file detail processing        *
* parameters record                                                  *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "t_dpp.h"
#include "scene_file.h"
#include "procfil.h"
#include "procdec.h"

extern char DEF_PATH[80];	/* default files */


#define T_DPP_SIZE	1540


t_dpp_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{



FILE *fp0;
T_DPP_FILE t_dpp_buf;

char	filename[80];

/*Open T_DPP_FILE0 */
sprintf(filename,"%sT_DPP_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&t_dpp_buf,T_DPP_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File T_DPP_FILE0 read error\n");
  }

fclose (fp0);


/*NO VALUES NEEDED*/


/*write T_DPP_FILE1*/
if(fwrite(&t_dpp_buf,T_DPP_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
