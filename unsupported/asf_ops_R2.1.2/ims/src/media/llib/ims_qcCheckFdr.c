static char *sccs = "@(#)ims_qcCheckFdr.c	5.1  03/17/96";
/* ******************************************************************
*
*  Name: CheckFDR
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine extracts information from FDR records.
*
*  Input Parameters:
*
*  Name            Type        Description
*   rec_buf         int         Record buffer
*   report_typ      char        Type of report
*   tmp2file        FILE        Pointer to temporary file 2
*   filenum         int         File number on media
*   numprod         int         Product number on media
*   class           char        File class
*
*  Output Parameters:
*
*  Name             Type        Description
*   fdrrecknts      struct      Array of record structures from FDR
*
*  Modification History:
*
*  Date:   08 Jan 1990 14:53:48    Revision:   1.0    Author:   CAROL
*  Date:   18 Jul 1990 10:17:38    Revision:   2.0    Author:   DBMAN
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

struct rec_str {        /* record structure template */
    int r_numrec;
    short   r_io_err;
    char    r_class[4];
    int r_lenrec;
};

void    CheckFDR (rec_buf,report_typ,tmp2file,filenum,numprod,class,
            fdrrecknts)
int rec_buf[];
FILE    *tmp2file;
int filenum, numprod;
char    class[], report_typ[];
struct rec_str  fdrrecknts[];
{

struct fdrrec {
    char    info[720];
    };
struct fdrrec *fdr;

char    temp[29];
/********************************************************************/


fdr = (struct fdrrec *) rec_buf;

/* save number and length of record types in file */

if( strcmp (class, "LDR") == 0 || strcmp (class, "TLR") == 0)
    {
    fdrrecknts[0].r_numrec = 1;
    fdrrecknts[0].r_lenrec = 720;
    (void) strcpy (fdrrecknts[0].r_class, "FDR");

    (void) strncpy (temp,fdr->info+180,6);
    (void) strncpy (temp+6, "\0", 1);
    (void) sscanf (temp,"%d",&fdrrecknts[1].r_numrec);
    (void) strncpy (temp,fdr->info+186,6);
    (void) sscanf (temp,"%d",&fdrrecknts[1].r_lenrec);
    (void) strcpy (fdrrecknts[1].r_class, "DSR");
/*  printf ("DSR: %d %d\n",fdrrecknts[1].r_numrec,
        fdrrecknts[1].r_lenrec);  */

    (void) strncpy (temp,fdr->info+192,6);
    (void) sscanf (temp,"%d",&fdrrecknts[2].r_numrec);
    (void) strncpy (temp,fdr->info+198,6);
    (void) sscanf (temp,"%d",&fdrrecknts[2].r_lenrec);
    (void) strcpy (fdrrecknts[2].r_class, "MPR");
/*  printf ("MPR: %d %d\n",fdrrecknts[2].r_numrec,
        fdrrecknts[2].r_lenrec);  */

    (void) strncpy (temp,fdr->info+204,6);
    (void) sscanf (temp,"%d",&fdrrecknts[3].r_numrec);
    (void) strncpy (temp,fdr->info+210,6);
    (void) sscanf (temp,"%d",&fdrrecknts[3].r_lenrec);
    (void) strcpy (fdrrecknts[3].r_class, "PPR");
/*  printf ("PPR: %d %d\n",fdrrecknts[3].r_numrec,
        fdrrecknts[3].r_lenrec);  */

    (void) strncpy (temp,fdr->info+216,6);
    (void) sscanf (temp,"%d",&fdrrecknts[4].r_numrec);
    (void) strncpy (temp,fdr->info+222,6);
    (void) sscanf (temp,"%d",&fdrrecknts[4].r_lenrec);
    (void) strcpy (fdrrecknts[4].r_class, "ADR");
/*  printf ("ADR: %d %d\n",fdrrecknts[4].r_numrec,
        fdrrecknts[4].r_lenrec);  */

    (void) strncpy (temp,fdr->info+228,6);
    (void) sscanf (temp,"%d",&fdrrecknts[5].r_numrec);
    (void) strncpy (temp,fdr->info+234,6);
    (void) sscanf (temp,"%d",&fdrrecknts[5].r_lenrec);
    (void) strcpy (fdrrecknts[5].r_class, "RDR");
/*  printf ("RDR: %d %d\n",fdrrecknts[5].r_numrec,
        fdrrecknts[5].r_lenrec);  */

    (void) strncpy (temp,fdr->info+240,6);
    (void) sscanf (temp,"%d",&fdrrecknts[6].r_numrec);
    (void) strncpy (temp,fdr->info+246,6);
    (void) sscanf (temp,"%d",&fdrrecknts[6].r_lenrec);
    (void) strcpy (fdrrecknts[6].r_class, "RCR");
/*  printf ("RCR: %d %d\n",fdrrecknts[6].r_numrec,
        fdrrecknts[6].r_lenrec);  */

    (void) strncpy (temp,fdr->info+252,6);
    (void) sscanf (temp,"%d",&fdrrecknts[7].r_numrec);
    (void) strncpy (temp,fdr->info+258,6);
    (void) sscanf (temp,"%d",&fdrrecknts[7].r_lenrec);
    (void) strcpy (fdrrecknts[7].r_class, "DQR");
/*  printf ("DQR: %d %d\n",fdrrecknts[7].r_numrec,
        fdrrecknts[7].r_lenrec);  */

    (void) strncpy (temp,fdr->info+264,6);
    (void) sscanf (temp,"%d",&fdrrecknts[8].r_numrec);
    (void) strncpy (temp,fdr->info+270,6);
    (void) sscanf (temp,"%d",&fdrrecknts[8].r_lenrec);
    (void) strcpy (fdrrecknts[8].r_class, "DHR");
/*  printf ("DHR: %d %d\n",fdrrecknts[8].r_numrec,
        fdrrecknts[8].r_lenrec);  */

    (void) strncpy (temp,fdr->info+276,6);
    (void) sscanf (temp,"%d",&fdrrecknts[9].r_numrec);
    (void) strncpy (temp,fdr->info+282,6);
    (void) sscanf (temp,"%d",&fdrrecknts[9].r_lenrec);
    (void) strcpy (fdrrecknts[9].r_class, "RSR");
/*  printf ("RSR: %d %d\n",fdrrecknts[9].r_numrec,
        fdrrecknts[9].r_lenrec);  */

    (void) strncpy (temp,fdr->info+288,6);
    (void) sscanf (temp,"%d",&fdrrecknts[10].r_numrec);
    (void) strncpy (temp,fdr->info+294,6);
    (void) sscanf (temp,"%d",&fdrrecknts[10].r_lenrec);
    (void) strcpy (fdrrecknts[10].r_class, "DEM");
/*  printf ("DEM: %d %d\n",fdrrecknts[10].r_numrec,
        fdrrecknts[10].r_lenrec);  */

    (void) strncpy (temp,fdr->info+300,6);
    (void) sscanf (temp,"%d",&fdrrecknts[11].r_numrec);
    (void) strncpy (temp,fdr->info+306,6);
    (void) sscanf (temp,"%d",&fdrrecknts[11].r_lenrec);
    (void) strcpy (fdrrecknts[11].r_class, "RPU");
/*  printf ("RPU: %d %d\n",fdrrecknts[11].r_numrec,
        fdrrecknts[11].r_lenrec);  */

    (void) strncpy (temp,fdr->info+312,6);
    (void) sscanf (temp,"%d",&fdrrecknts[12].r_numrec);
    (void) strncpy (temp,fdr->info+318,6);
    (void) sscanf (temp,"%d",&fdrrecknts[12].r_lenrec);
    (void) strcpy (fdrrecknts[12].r_class, "ANR");
/*  printf ("ANR: %d %d\n",fdrrecknts[12].r_numrec,
        fdrrecknts[12].r_lenrec);  */

    (void) strncpy (temp,fdr->info+324,6);
    (void) sscanf (temp,"%d",&fdrrecknts[13].r_numrec);
    (void) strncpy (temp,fdr->info+330,6);
    (void) sscanf (temp,"%d",&fdrrecknts[13].r_lenrec);
    (void) strcpy (fdrrecknts[13].r_class, "DPR");
/*  printf ("DPR: %d %d\n",fdrrecknts[13].r_numrec,
        fdrrecknts[13].r_lenrec);  */

    (void) strncpy (temp,fdr->info+336,6);
    (void) sscanf (temp,"%d",&fdrrecknts[14].r_numrec);
    (void) strncpy (temp,fdr->info+342,6);
    (void) sscanf (temp,"%d",&fdrrecknts[14].r_lenrec);
    (void) strcpy (fdrrecknts[14].r_class, "CDR");
/*  printf ("CDR: %d %d\n",fdrrecknts[14].r_numrec,
        fdrrecknts[14].r_lenrec);  */

    (void) strncpy (temp,fdr->info+348,6);
    (void) sscanf (temp,"%d",&fdrrecknts[15].r_numrec);
    (void) strncpy (temp,fdr->info+354,6);
    (void) sscanf (temp,"%d",&fdrrecknts[15].r_lenrec);
    (void) strcpy (fdrrecknts[15].r_class, "GCP");
/*  printf ("GCP: %d %d\n",fdrrecknts[15].r_numrec,
        fdrrecknts[15].r_lenrec);  */

    (void) strncpy (temp,fdr->info+420,6);
    (void) sscanf (temp,"%d",&fdrrecknts[16].r_numrec);
    (void) strncpy (temp,fdr->info+426,6);
    (void) sscanf (temp,"%d",&fdrrecknts[16].r_lenrec);
    (void) strcpy( fdrrecknts[16].r_class, "FAC");
/*  printf ("FAC: %d %d\n",fdrrecknts[16].r_numrec,
        fdrrecknts[16].r_lenrec);  */
    }


else if( strcmp (report_typ, "Full") == 0){
    /* build and write descriptor records from fields within
        FDR record */
    (void) fprintf (tmp2file,"Product: %3d             File: %3d",
        numprod, filenum);

    (void) strncpy (temp, fdr->info+16, 12);
    (void) strncpy (temp+12, fdr->info+28, 2);
    (void) strncpy (temp+12, "-", 1);
    (void) strncpy (temp+14, "\0", 1);
    (void) fprintf (tmp2file,"        SAR Standard:  %s\n \n",temp);

    (void) fprintf (tmp2file, "SAMPLE GROUP DATA\n \n");
    (void) strncpy (temp, fdr->info+216, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Bits per Sample:    %s\n",temp);

    (void) strncpy (temp, fdr->info+220, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Samples per Pixel:  %s\n",temp);

    (void) strncpy (temp, fdr->info+224, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Bytes per Pixel:    %s\n \n",temp);

    (void) fprintf (tmp2file, "SAR RELATED DATA IN RECORD\n \n");
    (void) strncpy (temp, fdr->info+232, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"No. of SAR Channels:           %s\n",
        temp);

    (void) strncpy (temp, fdr->info+236, 8);
    (void) strncpy (temp+8, "\0", 1);
    (void) fprintf (tmp2file,"Lines per Channel:         %s\n",temp);

    (void) strncpy (temp, fdr->info+244, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Left Border Pixels per Line:   %s\n",
        temp);

    (void) strncpy (temp, fdr->info+248, 8);
    (void) strncpy (temp+8, "\0", 1);
    (void) fprintf (tmp2file,"SAR Pixels per Line:       %s\n",temp);

    (void) strncpy (temp, fdr->info+256, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Right Border Pixels per Line:  %s\n",
        temp);

    (void) strncpy (temp, fdr->info+260, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Top Border Lines:              %s\n",
        temp);

    (void) strncpy (temp, fdr->info+264, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Bottom Border Lines:           %s\n",
        temp);

    (void) strncpy (temp, fdr->info+268, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,
        "Interleaving:                   %s\n \n",temp);

    (void) fprintf (tmp2file, "RECORD DATA IN FILE\n \n");
    (void) strncpy (temp, fdr->info+272, 2);
    (void) strncpy (temp+2, "\0", 1);
    (void) fprintf (tmp2file,
        "Physical Records per Line:               %s\n",temp);

    (void) strncpy (temp, fdr->info+274, 2);
    (void) strncpy (temp+2, "\0", 1);
    (void) fprintf (tmp2file,
        "Physical Records per Multichannel Line:  %s\n",temp);

    (void) strncpy (temp, fdr->info+276, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,
        "Bytes of Prefix Data per Record:       %s\n",temp);

    (void) strncpy (temp, fdr->info+280, 8);
    (void) strncpy (temp+8, "\0", 1);
    (void) fprintf (tmp2file,"Bytes of SAR Data per Record:      %s\n",
        temp);

    (void) strncpy (temp, fdr->info+288, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,
        "Bytes of Suffix Data per Record:       %s\n \n", temp);

    (void) fprintf (tmp2file, "SAR DATA PIXEL DESCRIPTION\n \n");
    (void) strncpy (temp, fdr->info+400, 28);
    (void) strncpy (temp+28, "\0", 1);
    (void) fprintf (tmp2file,"SAR Data Format Type:        %s\n",temp);

    (void) strncpy (temp, fdr->info+432, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Left Fill Bits within Pixel:      %s\n",
        temp);

    (void) strncpy (temp, fdr->info+436, 4);
    (void) strncpy (temp+4, "\0", 1);
    (void) fprintf (tmp2file,"Right Fill Bits within Pixel:     %s\n",
        temp);

    (void) strncpy (temp, fdr->info+440, 8);
    (void) strncpy (temp+8, "\0", 1);
    (void) fprintf (tmp2file,"Maximum Data Range of Pixel:  %s\n",temp);
    }

return;
}   /*  CheckFDR    */
