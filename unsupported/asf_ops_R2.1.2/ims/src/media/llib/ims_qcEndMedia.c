static char *sccs = "@(#)ims_qcEndMedia.c	5.2  04/02/97";
/* *******************************************************************
*
*  Name: EndMedia
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine dismounts the media and writes the temporary file
*    to the report file.
*
*  Input Parameters:
*
*   Name       Type        Description
*   report_typ  char        Type of report (brief,standard,full)
*   media_typ   char        Type of media (tape,disk,next)
*   title_buf   char        Title buffer
*   rptfile     FILE        Pointer to report file
*   tmp1file    FILE        Pointer to temporary file 1
*   tmp2file    FILE        Pointer to temporary file 2
*
*  Output Parameters:
*
*   Name       Type        Description
*   istat       int         Return status, error = IMS_ERROR
*
*  Modification History:
*
*  Date:   18 Jul 1990 10:22:26    Revision:   2.0   Author:   DBMAN
*
******************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>

#define ERR -1
#define EOFS 0
#define TITLINES 3
#define HDRLINES 1
#define TXTLINES 6
#define DESCLINES 33
#define DIRLINES 1

int EndMedia( msgDesc, report_typ,media_typ,title_buf,rptfile,
            tmp1file, tmp2file, time_stamp )
IMS_MSG_STRUCT  *  msgDesc;
char    report_typ[];
char    media_typ[];
char    title_buf[][76];
char    time_stamp[];
FILE    *rptfile, *tmp1file, *tmp2file;
{

int    istat, mstat;
char    *cstat;
static  char    text_hdr[1][45]= {
    "                                 TEXT RECORD" };
static char desc_hdr[1][54]= {
    "                      SAR DATA FILE DESCRIPTOR RECORD" };
static char dir_hdr[1][44]= {
   "                                  DIRECTORY" };
char    text[6][76];
char    desc[33][76];
char    dir_buf[1][76];

int i, newpage;
int dirfile;
int openflag = O_RDONLY;
char  name[128];

void  WrtReport( FILE *, int, char [][], int, char [][], int,
        char [][],int );

/* *************************************************************** */

/* copy text records to report */

if( strcmp (report_typ,"Brief") != 0) {
    istat = fclose (tmp1file);
    tmp1file = NULL;
    if( istat != EOFS ) {
        strcpy( name, "/tmp/qt1_" );
        strcat( name, time_stamp );
        strcat( name, ".txt" );
        tmp1file = fopen (name,"r");
        if( tmp1file == NULL) {
            (void)  ims_msg( msgDesc, IMS_ERROR,
                "Error opening temporary report file #1 for read.");
            return ( IMS_ERROR );
        }
    }
    else{
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Error closing temporary report file #1 for write.");
        return ( IMS_ERROR );
    }
}
if( strcmp (report_typ, "Full") == 0){
    mstat = fclose (tmp2file);
    tmp2file = NULL;
    if( mstat != EOFS){
        strcpy( name, "/tmp/qt2_" );
        strcat( name, time_stamp );
        strcat( name, ".txt" );
        tmp2file = fopen (name,"r");
        if( tmp2file == NULL){
            (void)  ims_msg( msgDesc, IMS_ERROR,
                "Error opening temporary report file #2 for read.");
            return ( IMS_ERROR );
        }
    }
    else{
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Error closing temporary report file #2 for write.");
        return ( IMS_ERROR );
    }
}
newpage = TRUE;

if( strcmp (report_typ,"Brief") != 0){
    while (istat != EOFS && mstat != EOFS){
        /*  write text and descriptor records to report */

        if( strcmp (report_typ,"Brief") != 0){
            /* read and write groups of text records */

            for (i=0; i <= (TXTLINES - 1); i++){
                cstat = fgets (text[i], 76, tmp1file);
                (void) strncpy (text[i]+(strlen(text[i])-1), "\0", 1);
                if( cstat == NULL){
                    istat = EOFS;
                    if( i != 0){
                        (void)  ims_msg( msgDesc, IMS_ERROR,
                        "Error reading temporary report file #1.");
                        return ( IMS_ERROR );
                    }
                    goto direct;
                }
            }
            WrtReport (rptfile,newpage,title_buf,TITLINES,
                text_hdr,HDRLINES, text,TXTLINES);
            newpage = FALSE;
            if( strcmp (report_typ, "Full") == 0){
                /* copy descriptor records to report */

                WrtReport (rptfile,newpage,title_buf,TITLINES,
                                desc_hdr,HDRLINES,desc_hdr,HDRLINES);

                /* read and write groups of file descr. records */

                for (i=0; i <= (DESCLINES - 1); i++){
                    cstat = fgets (desc[i], 76, tmp2file);
                    (void) strncpy (desc[i]+(strlen(desc[i])-1),
                        "\0", 1);
                    if( cstat == NULL){
                        mstat = EOFS;
                        if( i != 0){
                            (void)  ims_msg( msgDesc, IMS_ERROR,
                            "Error reading temporary report file #2.");
                            return ( IMS_ERROR );
                        }
                        goto direct;
                    }
                }
                WrtReport (rptfile,newpage,title_buf,TITLINES,
                    desc_hdr,HDRLINES, desc, DESCLINES);
                newpage = TRUE;
            }
        }
    }
}

direct: ;
/*  write disk directory to report */
if( strcmp (report_typ,"Brief") != 0  &&  tmp1file  !=  NULL  )
    (void) fclose( tmp1file );
tmp1file = NULL;
if( strcmp (report_typ, "Full") == 0  &&  tmp2file  !=  NULL  )
    (void) fclose( tmp2file );
tmp2file = NULL;

/*  delete all temporary files  */
strcpy( name, "/tmp/qt1_" );
strcat( name, time_stamp );
strcat( name, ".txt" );
istat = remove( name );
strcpy( name, "/tmp/qt2_" );
strcat( name, time_stamp );
strcat( name, ".txt" );
istat = remove( name );

return( IMS_OK );
}   /*  EndMedia  */
