#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       REQQ_append.c
 
Description:    contains REQQ_append(), which appends to the REQQ file
                NOTE:  file pointer passed must be fopen()ed in "r+" mode.  

External Functions Defined:
REQQ_append()
 
Notes:
        This file written with a 4-character tab setting. 
 
==============================================================================*/
#pragma ident   "@(#)REQQ_append.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.REQQ_append.c"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>         /* for strftime()    */
#include <aps_log_msg.h>

#include "list_handling.h"
#include "crt_grs_reqq.h"

/*==============================================================================
Function:      REQQ_append()

Description:   Given pointers to a point in the doubly sorted list of paths
                 and rows, this function advances to the next point in the
                 list, and returns the proper pointers.  It also increments
                 the count to tell you what point number you are at, provided
                 you start with count = -1 at the dummy pathnode head structure

Parameters:
    Input Parameters:
    dar          long                  the darid we are currently processing

    head         * struct pathnode     pointer to the head of the list structure

    observe_period
                 * char[]              double pointer to string representing
                                         the observation period specified in
                                         the dar.  is double pointer because
                                         it was assigned and then passed
                                         around between a few different 
                                         functions.  used in each record

    Output Parameters:
    none, except for file created

Returns:       void

Creator:       Brian J Griglak

Creation Date: Thu Jul 24 14:30:20 PDT 1997
==============================================================================*/

void REQQ_append(
    FILE *reqq_fp,     /* reqq file pointer      */
         /* NOTE:  file pointer passed must be fopen()ed in "r+" mode. */
    long  dar,         /* input dar.             */
    PATH  *head,       /* input grs list.        */
    int   outphase,    /* reqq_phase number.     */
    int   outimage,    /* number of images (number of requests) for each   */
                       /* GRS in the input grs list (head)                 */

    char  *observe_period[], 
        /*
        -- Note:  observe_period[] has n entries, where n = outimage.  Each of 
        --        these entries is a time bracket in this format:  
        --        "yyyymmddYYYYMMDD"      which is the desired REQQ format.  
        */

    FILE  *logfp )
{

    int     return_code ;
    char    outrecord[62] ;
    char    innumber[6];
    char    outreqqid[9];
    int     pathval, rowval;
    int     innum, outnum = 0 ;
    PATH    *p;
    ROW     *r;
    int     image;

    /* position to END OF FILE to be able to append:  */
    return_code = fseek(reqq_fp, 0, SEEK_END) ;
    if ( return_code != 0 )
    {
        printf ("%s(%d):  fseek() TO EOF FAILED.\n", __FILE__, __LINE__) ;
        printf ("Error in software.  Get programmers.  \n") ;
        printf ("Error in software.  Get programmers.  \n") ;
        printf ("Error in software.  Get programmers.  \n") ;
        return ;
    }

    /*
    --- Now add remaining list nodes to file in record format.
    */
    p = head -> next;
    if (p == NULL)
        r = NULL;
    else
        r = p -> head;

    (void) strcpy(outreqqid, "        \0");

    /* for each node in the list:  */
    while (p != NULL)
    {
        pathval = p -> value;
        rowval = r -> value;
        for (image = 0; image < outimage; image++)
        {
            reqqid_encode(dar, pathval, rowval, outphase, image, outreqqid);
            outreqqid[8] = '\0';
            (void) sprintf(outrecord, 
                "%s  %s%03d%03dSAR                  1       ",
                outreqqid, observe_period[image], pathval, rowval);
            if (strcmp(observe_period[image], "Bad image time  ") != 0)
            {
                /* WRITE REC to output REQQ file.  */
                (void) fprintf( reqq_fp, "%s", outrecord);
                outnum++;
#ifdef TEST_ONLY
                (void) printf("%s", outrecord );
#endif
            }
        }

        list_next(&p, &r);
        (void) delete_node(head, pathval, rowval);
    }

    /*
    --- Read the REQQ header and extract the number of records in it.  
    --- This is a 5-character field offset of 41 from start of the file.  
    */
    return_code = fseek(reqq_fp, 41, SEEK_SET) ;
    if ( return_code != 0 )
    {
        printf ("%s(%d):  fseek() TO NRECS FAILED.\n", __FILE__, __LINE__) ;
        printf ("Error in software.  Get programmers.  \n") ;
        printf ("Error in software.  Get programmers.  \n") ;
        printf ("Error in software.  Get programmers.  \n") ;
        return ;
    }

    (void) fscanf(reqq_fp, "%5c", innumber) ;
    innumber[5] = '\0';
    innum  = atoi(innumber);

    /* note to log file:   */
    (void) printf( 
        "\nNumber of records in REQQ before GRS for DAR %d added = %d\n",
        dar, innum) ;
    (void) fprintf( logfp, 
        "Number of records in REQQ before GRS for DAR %d added = %d\n",
        dar, innum) ;

    /*
    --- Update header of new REQQ to correctly indicate number of records
    --- and the date of creation.
    */
    if ( fseek( reqq_fp, 41, SEEK_SET)  !=  0 )
    {
        printf ("%s(%d):  SECOND fseek() TO NRECS FAILED.\n", 
            __FILE__, __LINE__ ) ;
        printf ("Error in software.  Get programmers.  \n") ;
        printf ("Error in software.  Get programmers.  \n") ;
        printf ("Error in software.  Get programmers.  \n") ;
        return ;
    }
    /* update number of records in REQQ file:  */
    (void) fprintf( reqq_fp, "%05d", outnum+innum );

    /* note to log file:   */
    (void) printf(
        "For DAR %d, number of GRS records added to REQQ file = %d\n",
        dar, outnum ) ;
    (void) fprintf(logfp, 
        "For DAR %d, number of GRS records added to REQQ file = %d\n",
        dar, outnum ) ;

    return;
}
