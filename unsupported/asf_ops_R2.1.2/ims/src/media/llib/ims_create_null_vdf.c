static char *sccs = "@(#)ims_create_null_vdf.c	5.1  03/17/96";
/********************************************************************
*
*  Name: ims_create_null_vdf
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine creates a null volume directory file from
*       the leading vol. directory record on the tpae (the first
*       record in the vol. directory file at the start of the
*       tape).  this is based on ims_writeNullVdf, but takes the
*       vdf name as input, reads the first record, and uses this
*       to create the null file.  the file is saved, but not put
*       on tape.
*
*  Input Parameters:
*
*  Name          Type        Description
*  nvd_name      char        Input null vdf file name (save it here)
*  vdf_name      char        vdf file name.
*  debug         short       If true, print debug info.
*
*
*  Return Status from Procedure:
*
*  Name          Type       Description
*  istat         int        Return status, =  IMS_ERROR or IMS_OK.
*
*  Modification History:
*
*   Date:   11 Jan 1989  9:29:00     Revision:   1.0    Author:  INGRID
*   Date:   14 Mar 1989 12:48:26     Revision:   1.1    Author:  INGRID
*   Date:   25 Aug 1989 12:30:56     Revision:   1.2    Author:  INGRID
*   Date:   05 Sep 1989 17:52:24     Revision:   1.3    Author:  INGRID
*   Date:   25 Sep 1989 11:02:32     Revision:   1.4    Author:  INGRID
*   Date:   03 Oct 1989  8:50:46     Revision:   1.5    Author:  INGRID
*   Date:   18 Jul 1990 10:03:10     Revision:   2.0    Author:  DBMAN
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

#include "../lincl/defs.h"
#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>

char    msg_string[150];
char    buf[513];
long    rsize;


int  create_null_vdf( char * vdf_name, char * nvd_name,
    short  debug, IMS_MSG_STRUCT * msgDesc )
{
int             ouf_desc, in_desc,istat, i;
static int      nbytes=360;
pnt_recvdr_t            pnt_vdr;

int  cvt2int( unsigned char * );


    if (debug)
		{
			(void) printf ("\nCreating NDF File '%s'.\n", nvd_name);
		}

    /* open the new null vol dir file, rfm=var, mrs=360*/
    if(  nvd_name[0]  !=  '\0' ){
        ouf_desc = open( nvd_name, O_WRONLY | O_CREAT | O_TRUNC );
        i = chmod( nvd_name, 0700 );
    }
    else  ouf_desc = -1;

    /* open the vdf file  */
    in_desc = open( vdf_name, O_RDONLY );
    if(  in_desc  <=  0  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Cannot open VDF file '%s'. %s",
            vdf_name, strerror (errno));
        return( IMS_ERROR );
    }

    /* now read the first record  */
    nbytes = read( in_desc, buf, 12 );
    if(  nbytes  <=  0  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Cannot read VDF file '%s'. %s",
            vdf_name, strerror (errno));
        return( IMS_ERROR );
    }
    rsize =  cvt2int( (unsigned char *) &buf[8]) - 12;
    if ( rsize>0 ) {
        nbytes = read( in_desc, (buf+12), rsize );
        if(  nbytes  <=  0  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Cannot read VDF file '%s'. %s",
                vdf_name, strerror (errno));
            return( IMS_ERROR );
        }
        if ( nbytes != rsize) {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Read error in VDF file '%s'.",
                vdf_name);
            return( IMS_ERROR );
        }
        nbytes += 12;
    }
    istat = close( in_desc );
    if(  istat  <  0  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Cannot close VDF file '%s'.",
            vdf_name, strerror (errno));
        return( IMS_ERROR );
    }

    /*
    ** Now vdr record in buf - set pointer
    ** lint: pointer cast may result in improper alignment
    ** The structure definitions are identical.
    */
    pnt_vdr = (pnt_recvdr_t) buf;

    /*  change the vol con VDR contents to null vol VDR.*/
    pnt_vdr->secsub = 63;
    for (i=0; i<=15; i++)
        pnt_vdr->logi_vol[i] = ' ';
    for (i=0; i<=15; i++)
        pnt_vdr->volsetid[i] = ' ';
    pnt_vdr->totvol[0] = ' ';
    pnt_vdr->totvol[1] = '1';
    pnt_vdr->phyfirst[0] = ' ';
    pnt_vdr->phyfirst[1] = '1';
    pnt_vdr->phylast[0]  = ' ';
    pnt_vdr->phylast[1]  = '1';
    pnt_vdr->phycurr[0]  = ' ';
    pnt_vdr->phycurr[1]  = '1';
    for (i=0; i<=3; i++)
        pnt_vdr->fstfile[i] = ' ';
    for (i=0; i<=2; i++)
        pnt_vdr->preslogi[i] = ' ';
    pnt_vdr->preslogi[3] = '2';
    for (i=0; i<=2; i++)
        pnt_vdr->logvolnum[i] = ' ';
    pnt_vdr->logvolnum[3]= '2';
    for (i=0; i<=7; i++)
        pnt_vdr->logvoldate[i] = ' ';
    for (i=0; i<=7; i++)
        pnt_vdr->logvoltime[i] = ' ';
    for (i=0; i<=11; i++)
        pnt_vdr->country[i] = ' ';
    for (i=0; i<=7; i++)
        pnt_vdr->agency[i] = ' ';
    for (i=0; i<=11; i++)
        pnt_vdr->facility[i] = ' ';
    for (i=0; i<=3; i++)
        pnt_vdr->pointers[i] = ' ';
    for (i=0; i<=3; i++)
        pnt_vdr->numrecdir[i] = ' ';
    for (i=0; i<=3; i++)
        pnt_vdr->totlogi[i] = ' ';
    for (i=0; i<=188; i++)
        pnt_vdr->spare[i] = ' ';

    /* write null vol vdr to output file*/
    istat = IMS_OK;
    if(  ouf_desc  !=  -1  ){
        istat = write ( ouf_desc, pnt_vdr, nbytes);
        if (istat<0){
            istat = IMS_ERROR;
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Could not write to NDF file '%s'. %s",
                nvd_name, strerror (errno));
        }
        else istat = IMS_OK;
        istat = close( ouf_desc );
        if(  istat  <  0  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Cannot close NDF file '%s'. %s",
                nvd_name, strerror (errno));
            return( IMS_ERROR );
        }
        else  istat = IMS_OK;
    }
    return(istat);
}   /*  create_null_vdf   */
