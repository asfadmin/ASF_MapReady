#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   ascii_file_creator.c

Description:    modules used to create the library lib_fileutils.a

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)ascii_file_creator.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.ascii_file_creator.c"

#include <stdio.h>
#include <stddef.h>  /* for NULL */
#include <string.h>

#include "db_sybint.h"
#include "dapps_list.h"
/* #include "stgs_defs.h" */
#include "db_dtk.h"
#include "fa_defs.h"
#include "file_utilities.h"
#include "GENconversions.h" /* for global variable definitions */

#define FA_FILE_CREATION_OK              1
#define FA_NULL_FIRST_DTK_RECS          -1

#define FA_RECORD_CREATION_OK            2
#define FA_RECORD_CREATION_FAILED       -2

#define PRINT_DIAG
#undef  PRINT_DIAG


/*==============================================================================
Function:       file_create_record

Description:    

Parameters:     

Returns:        
        int
        >= 0 normal:
                FA_RECORD_CREATION_OK       (value: 2)
 
        < 0 ERROR:  
                FA_RECORD_CREATION_FAILED   (value: -2  )

Creator:        Miguel Siu

Creation Date:  Tue Oct 24 10:49:57 PDT 1995

Notes:      Adapted from routine stgs_file_create_record from Ted Pavlovitch
==============================================================================*/
static int 
file_create_record( int record_dest_type    , VALUE_DEFS *FA_filedefs ,
                             DB_RECORD **dtkrec , FILE *fp , char *buf )
{

    int     j;
    int     *ip;
    int     buffer_print_flag ;
    int     return_code ;

    #if defined (PRINT_DIAG)
        printf("file record creator entered to create record type:%d\n", 
            record_dest_type);
    #endif

    buffer_print_flag = 0 ;
    for( j=0 ; FA_filedefs[j].destination_code != 0 ; j++ )
    {
        #if defined (PRINT_DIAG)
            printf("j=%d  destination_code=%d \n",
                j,FA_filedefs[j].destination_code);
            printf("press return to continue\n");
            getchar();
        #endif

        /*
        -- Process only those definitions that match the destination type.
        -- (ie: process only FILE_HEADER  or  FILE_RECORD or FILE_TRAILER)
        */
        if(FA_filedefs[j].destination_code != record_dest_type )
            continue;


        switch (FA_filedefs[j].source_code)
        {
        case DTK_NEWLINE:/*********************************************/
                         /* print current buffer and go to a new line */
            fprintf(fp,"%s",buf);
            fprintf(fp,"\n") ;
            break ;
            
        case DTK_DEFAULT_STRING:/*********************************************/
                         /* copy string to user-determined destination   */
                         /* NOTE: this was intended to allow a string    */
                         /* to be placed into value def array            */
                         /* and have one less level of indirection.      */
                         /* the DTK_DEFAULT looked unnecessarily         */
                         /* complicated.                                 */
                         /* the entry (ip) here, is a pointer to a char, */
                         /* instead of a pointer to a pointer to a  char.*/

            ip = FA_filedefs[j].destination.pointer ; 
            if (FA_filedefs[j].length == 0)
                FA_filedefs[j].length = strlen((char *) ip) + 1 ;
            strncpy(&buf[FA_filedefs[j].offset] ,(char *) ip , 
                FA_filedefs[j].length);

            break;

        case DTK_DEFAULT:/*********************************************/
                         /* copy string to user-determined destination*/
                         /* NOTE: went back to the unorthodox notation*/
                         /* (char *)*ip  in order to support MSGE     */

            ip = FA_filedefs[j].destination.pointer ; 
            if (FA_filedefs[j].length == 0)
                FA_filedefs[j].length = strlen((char *) *ip) + 1 ;
            strncpy(&buf[FA_filedefs[j].offset] ,(char *) *ip , 
                FA_filedefs[j].length);

            break;


        case DTK_RECORD:/*********************************************/
                        /* copy or convert dtk DB_RECORD field into  */
                        /* user-determined destination.  Conversion  */
                        /* routine must monitor overwriting errors   */

            #if defined (PRINT_DIAG)
                printf("conversion address:%8.8x \n", 
                    (FA_filedefs[j].conversion));
                printf("offset:%d   length:%d \n", 
                FA_filedefs[j].offset , FA_filedefs[j].length );
            #endif

            if( FA_filedefs[j].conversion != NULL )
            {
                if( FA_filedefs[j].length == 0 
                                /* destination length MUST be specified */
                ||  !(FA_filedefs[j].conversion)
                    (dtkrec[FA_filedefs[j].destination.index] , 
                    &buf[FA_filedefs[j].offset] , FA_filedefs[j].length))

                    return(FA_RECORD_CREATION_FAILED);   
                    /* error return from conversion function */
            }
            else
            {
                if (FA_filedefs[j].length == 0)
                    FA_filedefs[j].length = 
                    strlen( (char *)dtkrec[FA_filedefs[j].destination.index]) + 1;


                strncpy(&buf[FA_filedefs[j].offset] ,  
                    (char *) dtkrec[FA_filedefs[j].destination.index] , 
                    FA_filedefs[j].length);

                #if defined (PRINT_DIAG)
                    printf("non conversion string attribute:%s\n",
                        dtkrec[FA_filedefs[j].destination.index]);
                    printf("offset:%d   length:%d \n", 
                        FA_filedefs[j].offset , FA_filedefs[j].length );
                #endif
            }
            break;


        case DTK_FUNCTION:/*********************************************/
                        /* call a function to supply values into field */
                        /* Example:  file_creation_date_ns()           */
                        /* Routine must monitor overwriting errors     */

            #if defined (PRINT_DIAG)
                printf("conversion address:%8.8x \n", 
                    (FA_filedefs[j].conversion));
                printf("offset:%d   length:%d \n", 
                FA_filedefs[j].offset , FA_filedefs[j].length );
            #endif

            /* 
            -- the function is:  FA_filedefs[j].conversion   
            -- the destination of the output is &buf[FA_filedefs[j].offset]
            -- the length of the output is      FA_filedefs[j].length 
            */
            if( FA_filedefs[j].conversion == NULL 
            ||  FA_filedefs[j].length == 0 )
                return(FA_RECORD_CREATION_FAILED);   

            /* function call:  */
            return_code = (FA_filedefs[j].conversion) (
                NULL,                            /* unused pointer.      */
                &buf[FA_filedefs[j].offset] ,    /* output destination.  */
                FA_filedefs[j].length   ) ;      /* output length.       */
            if( return_code != TRUE )
                return(FA_RECORD_CREATION_FAILED);   

            break;


        case NON_DTKATTR:/*********************************************/
                         /* convert into user-determined destination  */

            if (FA_filedefs[j].length == 0)
                    return(FA_RECORD_CREATION_FAILED);   
                    /* destination length MUST be specified */

            ip = FA_filedefs[j].destination.pointer;

            #if defined (PRINT_DIAG)
                printf("non_dtkattr:%d\n", (int  *)ip);
            #endif

            if( !(FA_filedefs[j].conversion)(
                (int *)ip , &buf[FA_filedefs[j].offset] , 
                FA_filedefs[j].length ))
                return(FA_RECORD_CREATION_FAILED);  
                /* error return from conversion function */
            break;


        default:        /*********************************************/
            continue;

        } /* end switch */

        buffer_print_flag++ ;

        #if defined (PRINT_DIAG)
        printf("b:%s\n",buf);
        #endif

   }  /* end for */ 

    /*  
    -- write record to output file  
    */
    if (buffer_print_flag) 
        fprintf(fp,buf);


    return FA_RECORD_CREATION_OK;

}  /* end function file_create_record */ 

/*==============================================================================
Function:       ascii_file_creator.c

Description:    Creates an ASCII file from a FA_FILEDEF type description

Parameters:     

Returns:        
        int
        >= 0 normal:
                FA_FILE_CREATION_OK         (value:  1  )
 
        < 0 ERROR:  
                FA_NULL_FIRST_DTK_RECS      (value: -1  )
            file_create_record code:
                FA_RECORD_CREATION_FAILED   (value: -2  )


Creator:        Miguel Siu

Creation Date:  Tue Oct 24 10:44:33 PDT 1995

Notes:      Adapted from the original stgs_file_creator.c by Ted Pavlovitch
==============================================================================*/
int ascii_file_creator( int         print_header_flag,
                  llist         *dtk_list1 , 
                  llist         *dtk_list2 ,
                  FA_FILEDEF    *FA_file,
                  FILE          *fp )
{
    int i;
    int return_code;
    DB_RECORD   **dtk_list1_rec = NULL ;
    DB_RECORD   **dtk_list2_rec = NULL ;
    cursor       dtk_list1_ptr , dtk_list2_ptr ;
    char         buf[2048] ;/* this should be dynamically allocated !! */

    fa_number_of_records = 0;
 
    if( dtk_list1 != NULL )
    fa_number_of_records = NUMELTS(dtk_list1) ;

    if( dtk_list2 != NULL )
    fa_number_of_records = fa_number_of_records + NUMELTS(dtk_list2);

    if (print_header_flag == PRINT_HEADER_ONLY)
        fa_number_of_records = 0 ;

    #if defined (PRINT_DIAG)
        printf("file creator entered-number of db_recs:%d\n",
            fa_number_of_records);
    #endif

    if( dtk_list1 != NULL )
    dtk_list1_rec = (DB_RECORD **)FIRST(  dtk_list1 ,   dtk_list1_ptr ); 

    if( dtk_list2 != NULL )
    dtk_list2_rec = (DB_RECORD **)FIRST(  dtk_list2 ,   dtk_list2_ptr ); 

    /*
    -- 0 recs in a list results in a file with no 
    -- data records  retain this code to remember FA_NULL_FIRST_DTK_RECS
    -- if needed later.  
    --      if( dtk_list1_rec == NULL && dtk_list2_rec == NULL )
    --          return FA_NULL_FIRST_DTK_RECS ;
    */

    #if defined (PRINT_DIAG)
        printf("file header length:%d file data record length:%d \n" ,
                FA_file->header_length , FA_file->record_length );
    #endif
        
    for ( i=0 ; i< FA_file->header_length ; i++ )
            buf[i] = ' ';

    buf[FA_file->header_length    ] = '\0';
    buf[FA_file->header_length + 1] = '\0';
 
    if( dtk_list1_rec != NULL )
    {

        /* write header using first dtk_list1 record */

        return_code = file_create_record( 
            FILE_HEADER , FA_file->field_def , 
            dtk_list1_rec , fp , buf);

    }
    else
    {

        /* 
        -- write header using first dtk_list2 record.  
        -- header may be printed with a NULL dtk_list2_rec
        -- Make sure the value def array does not read 
        -- from the dtk rec.  
        */
        return_code = file_create_record( 
            FILE_HEADER , FA_file->field_def , 
            dtk_list2_rec , fp , buf );

    }


    if( return_code < 0 )
        return return_code ;    /* return_code < 0 is an error  */



    /* HEADER IS COMPLETE */
    if (print_header_flag == PRINT_HEADER_ONLY)
            return(FA_FILE_CREATION_OK) ;


    if( dtk_list1_rec != NULL )
    {
        for(dtk_list1_rec = (DB_RECORD **)FIRST(dtk_list1 , dtk_list1_ptr);
            dtk_list1_rec != NULL;
            dtk_list1_rec = (DB_RECORD **)NEXT(dtk_list1 , dtk_list1_ptr))

        {

           for ( i=0 ; i<FA_file->record_length ; i++ )
           buf[i] = ' ';

           buf[FA_file->record_length     ] = '\0';
           buf[FA_file->record_length + 1 ] = '\0';

           fa_dtkid = CAST_DTK_DTKID    dtk_list1_rec[DTK_DTKID];
           strcpy(fa_file_start_time , 
                (char *)CAST_DTK_STRTTIME dtk_list1_rec[DTK_STRTTIME]);
           strcpy(fa_file_stop_time , 
                (char *)CAST_DTK_STOPTIME dtk_list1_rec[DTK_STOPTIME]);

            #if defined (PRINT_DIAG)
               printf("ascii_file_creator--- strttime:%s  stoptime:%s \n",
                fa_file_start_time,fa_file_stop_time);
               printf("press return to continue\n");
               getchar();
            #endif

            /* WRITE DTK_LIST1 DATA RECORD TO FILE_RECORD */

            return_code = file_create_record( 
                FILE_RECORD , FA_file->field_def ,
                dtk_list1_rec , fp , buf );

            if( return_code < 0 )
                return return_code ;    /* return_code < 0 is an error  */


            /* DTK_LIST1 DATA RECORD IS COMPLETE */

        } /* end for */

        /* ALL DTK_LIST1 DATA RECORDS COMPLETED */ 

    } /*  end if  */



    if( dtk_list2_rec != NULL )
    {
        for(dtk_list2_rec = (DB_RECORD **)FIRST(dtk_list2 , dtk_list2_ptr);
            dtk_list2_rec != NULL;
            dtk_list2_rec = (DB_RECORD **)NEXT(dtk_list2 , dtk_list2_ptr))

        {

            for ( i=0 ; i<FA_file->record_length ; i++ )
            buf[i] = ' ';

            buf[FA_file->record_length     ] = '\0';
            buf[FA_file->record_length + 1 ] = '\0';

            fa_dtkid =  CAST_DTK_DTKID    dtk_list2_rec[DTK_DTKID];
            strcpy(fa_file_start_time , 
                (char *)CAST_DTK_STRTTIME dtk_list2_rec[DTK_STRTTIME]);
            strcpy(fa_file_stop_time , 
                (char *)CAST_DTK_STOPTIME dtk_list2_rec[DTK_STOPTIME]);


            /* WRITE DTK_LIST2 DATA RECORD TO FILE_RECORD */

            return_code = file_create_record( FILE_RECORD , FA_file->field_def ,
                                                 dtk_list2_rec , fp , buf );

            if( return_code < 0 )
                return return_code ;    /* return_code < 0 is an error  */


            /* DTK_LIST2 DATA RECORD IS COMPLETE */

        } /* end for */


        /* ALL DTK_LIST2 DATA RECORDS COMPLETED */ 

    } /*  end if  */


    return_code = file_create_record( FILE_TRAILER , FA_file->field_def ,
                                                 NULL, fp , buf );
    if( return_code < 0 )
        return return_code ;    /* return_code < 0 is an error  */

    return(FA_FILE_CREATION_OK) ;

}  /* end of ascii_file_creator */
