static char *sccs = "@(#)ims_makeNullVdf.c	5.1  03/17/96";
/********************************************************************
*
*  Name: make_null_vdf
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine creates a null volume directory file from
*       the leading vol. directory record on the tpae (the first
*       record in the vol. directory file at the start of the
*       tape).  this is then put at the end of the tape.
*
*
*  Input Parameters:
*
*  Name          Type        Description
*  nvd_name      char        Input null vdf file name (save it here)
*  pnt_vdf       pnt_vol_desc_rec_t  the values from the original
*                               vdf file: first file of tape.
*  debug         short       If true, print debug info.
*  desc          int         File descriptor to put file into.
*
*********************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include "../lincl/defs.h"

/******************************************************************************
**
** make_null_vdf ()
**
******************************************************************************/

int make_null_vdf (
		pnt_vdf_file_t pnt_vdf,
    char *nvd_name,
    short debug,
    int desc,
    IMS_MSG_STRUCT *msgDesc)
{
int             ouf_desc, istat, i;
static int      nbytes=360;

    /* open the new null vol dir file, rfm=var, mrs=360*/
    if(  nvd_name[0]  !=  '\0' ){
        ouf_desc = open( nvd_name, O_WRONLY | O_CREAT | O_TRUNC );
        i = chmod( nvd_name, 0700 );
    }
    else  ouf_desc = -1;

    /*  change the vol con VDR contents to null vol VDR.*/
    pnt_vdf->pnt_vdr->secsub = 63;
    for (i=0; i<=15; i++)
        pnt_vdf->pnt_vdr->logi_vol[i] = ' ';
    for (i=0; i<=15; i++)
        pnt_vdf->pnt_vdr->volsetid[i] = ' ';
    pnt_vdf->pnt_vdr->totvol[0] = ' ';
    pnt_vdf->pnt_vdr->totvol[1] = '1';
    pnt_vdf->pnt_vdr->phyfirst[0] = ' ';
    pnt_vdf->pnt_vdr->phyfirst[1] = '1';
    pnt_vdf->pnt_vdr->phylast[0]  = ' ';
    pnt_vdf->pnt_vdr->phylast[1]  = '1';
    pnt_vdf->pnt_vdr->phycurr[0]  = ' ';
    pnt_vdf->pnt_vdr->phycurr[1]  = '1';
    for (i=0; i<=3; i++)
        pnt_vdf->pnt_vdr->fstfile[i] = ' ';
    for (i=0; i<=2; i++)
        pnt_vdf->pnt_vdr->preslogi[i] = ' ';
    pnt_vdf->pnt_vdr->preslogi[3] = '2';
    for (i=0; i<=2; i++)
        pnt_vdf->pnt_vdr->logvolnum[i] = ' ';
    pnt_vdf->pnt_vdr->logvolnum[3]= '2';
    for (i=0; i<=7; i++)
        pnt_vdf->pnt_vdr->logvoldate[i] = ' ';
    for (i=0; i<=7; i++)
        pnt_vdf->pnt_vdr->logvoltime[i] = ' ';
    for (i=0; i<=11; i++)
        pnt_vdf->pnt_vdr->country[i] = ' ';
    for (i=0; i<=7; i++)
        pnt_vdf->pnt_vdr->agency[i] = ' ';
    for (i=0; i<=11; i++)
        pnt_vdf->pnt_vdr->facility[i] = ' ';
    for (i=0; i<=3; i++)
        pnt_vdf->pnt_vdr->pointers[i] = ' ';
    for (i=0; i<=3; i++)
        pnt_vdf->pnt_vdr->numrecdir[i] = ' ';
    for (i=0; i<=3; i++)
        pnt_vdf->pnt_vdr->totlogi[i] = ' ';
    for (i=0; i<=188; i++)
        pnt_vdf->pnt_vdr->spare[i] = ' ';

    /* write null vol vdr to output file*/
    if(  ouf_desc  !=  -1  ){
        istat = write ( ouf_desc, pnt_vdf->pnt_vdr, nbytes);
        if (istat<0){
            istat = IMS_ERROR;
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not write to NDF file '%s'. %s",
                nvd_name, strerror (errno));
        }
    }
    istat = write ( desc, pnt_vdf->pnt_vdr, nbytes);
    if (istat<0){
        istat = -10;
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not write NDF file to target. %s",
            strerror (errno));
    }

end:
    if(  istat  <  0 )  istat = IMS_ERROR;
    else  istat = IMS_OK;
    return(istat);
}   /*  make_null_vdf   */
