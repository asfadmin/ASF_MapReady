#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   apspath.c   

Description:    
    Functions in support of creating full path and filenames for
    APS executables.

External Functions Defined:
    char * aps_fullpath(int id, char *filename) 
    
File Scope Functions:
    static int create_aps_pathnames()
    
External Variables Defined:
    
File Scope Variables:

    static APS_PATH_TABLE aps_path_table =
    static char default_root_envname
    static int aps_path_table_size 
    
Notes:
    ******
    THE VALUES IN apspath.h FOR THE INDEXES INTO aps_path_table[]
    MUST CORRESPOND (NUMERICALLY) TO THE DEFINITION OF 
        static APS_PATH_TABLE aps_path_table[] 
    IN THIS FILE
    ******

    Programs calling the aps_fullpath function must free the memory
    to the returned pointer


==============================================================================*/
#pragma ident   "@(#)apspath.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.apspath.c"

#include <stdio.h>
#include <stdlib.h>     /* for getenv(), malloc()              */
#include <string.h>     /* used by the aps_pathname2filename() */
#include <unistd.h>

#include "dapps_defs.h"	/* for OK and ERROR */
#include "apspath.h"

typedef
    struct _APS_PATH_TABLE
    {
        char *name ;
        char *full_path ;
        int full_path_size ;
    } APS_PATH_TABLE ;

/*
--  THE VALUES IN apspath.h FOR THE INDEXES INTO aps_path_table[]
--  MUST CORRESPOND (NUMERICALLY) TO THE DEFINITION OF 
--      static APS_PATH_TABLE aps_path_table[] 
--  IN THIS FILE
*/
static APS_PATH_TABLE aps_path_table[] =
{
    {"tmp",                                 NULL, 0},   /*  0 */
    {"nsv",                                 NULL, 0},   /*  1 */
    {"nof",                                 NULL, 0},   /*  2 */
    {"nof",                                 NULL, 0},   /*  3 */
    {"cvrg",                                NULL, 0},   /*  4 */
    {"mapper",                              NULL, 0},   /*  5 */
    {"stoicfiles",                          NULL, 0},   /*  6 */
    {"stoicfiles/current",                  NULL, 0},   /*  7 */
    {"FTP/upw",                             NULL, 0},   /*  8 */
    {"FTP/stmp",                            NULL, 0},   /*  9 */
    {"FTP/gtmp",                            NULL, 0},   /* 10 */
    {"asf_files",                           NULL, 0},   /* 11 */
    {"nasda_files",                         NULL, 0},   /* 12 */
    {"csa_files",                           NULL, 0},   /* 13 */
    {"esa_files",                           NULL, 0},   /* 14 */
    {"logs",                                NULL, 0},   /* 15 */
    {"reports",                             NULL, 0},   /* 16 */
    {"csa_files/svf",                       NULL, 0},   /* 17 */
    {"csa_files/rar",                       NULL, 0},   /* 18 */
    {"csa_files/reception",                 NULL, 0},   /* 19 */
    {"csa_files/archive",                   NULL, 0},   /* 20 */
    {"esa_archive/shaq",                    NULL, 0},   /* 21 */
    {"esa_archive/shqp",                    NULL, 0},   /* 22 */
    {"esa_archive/mpsg",                    NULL, 0},   /* 23 */
    {"nasda_archive/opl1",                  NULL, 0},   /* 24 */
    {"nasda_archive/reqr",                  NULL, 0},   /* 25 */
    {"nasda_archive/opln",                  NULL, 0},   /* 26 */
    {"nasda_archive/reqm",                  NULL, 0},   /* 27 */
    {"nasda_archive/reqa",                  NULL, 0},   /* 28 */
    {"nasda_archive/msgn",                  NULL, 0},   /* 29 */
    {"csa_archive/reception_request_asf",   NULL, 0},   /* 30 */
    {"csa_archive/reception_schedule_asf",  NULL, 0},   /* 31 */
    {"csa_archive/reception_request_mcm",   NULL, 0},   /* 32 */
    {"csa_archive/reception_schedule_mcm",  NULL, 0},   /* 33 */
    {"asf_files/wos",                       NULL, 0},   /* 34 */
    {"asf_files/E1SV",                      NULL, 0},   /* 35 */
    {"asf_files/E2SV",                      NULL, 0},   /* 36 */
    {"asf_files/J1SV",                      NULL, 0},   /* 37 */
    {"asf_files/A1SV",                      NULL, 0},   /* 38 */
    {"asf_files/R1SV",                      NULL, 0},   /* 39 */
    {"wff_files/wos",                       NULL, 0},   /* 40 */
    {"wff_files/areq",                      NULL, 0},   /* 41 */
    {"wff_files/E1SV",                      NULL, 0},   /* 42 */
    {"wff_files/E2SV",                      NULL, 0},   /* 43 */
    {"wff_files/R1SV",                      NULL, 0},   /* 44 */
    {"wff_archive/ares",                    NULL, 0},   /* 45 */
    {"config/xlate",                        NULL, 0},   /* 46 */
    {"config/Xresource",                    NULL, 0},   /* 47 */
    {"FA_error_files",                      NULL, 0},   /* 48 */
    {"FA_input_files",                      NULL, 0},   /* 49 */
    {"ims_files",                           NULL, 0},   /* 50 */
    {"ims_files/ADDM",                      NULL, 0},   /* 51 */
    {"ims_files/MDDM",                      NULL, 0},   /* 52 */
    { NULL,                                 NULL, 0}
} ;

static int aps_path_table_size 
    = sizeof(aps_path_table) / ((sizeof(char *) * 2) + sizeof(int)) - 1 ;

static char default_root_envname[] = "APS_DATA" ;


/*==============================================================================
Function:       create_aps_pathnames

Description:    
    create a table of pathnames describing the location
    of APS data files.  Return OK, return ERROR.  

Creator:        Ron Green

Creation Date:  11/01/1994

Notes:      
==============================================================================*/
static int create_aps_pathnames()
{
    int i ;
    int base_path_length ;

    char *base_path ;

    /* check for default environment variable */
    base_path = (char *) getenv(default_root_envname) ;

    if (base_path)
    {
        /* check last char for / */
        i = strlen(base_path) - 1 ;  
        if (base_path[i] == '/'  && i > 0)
            base_path[i] = NULL ;

        /* check for existence of the directory before using it */
        if (access(base_path, F_OK) != 0)
        {

            printf(
"\n\n%s(%d):  ERROR IN INSTALLATION:  directory '%s' DOESN'T EXIST...\n", 
                __FILE__, __LINE__, base_path) ;
            printf("Cannot use any directory path.   \n" ) ;
            printf("TO FIX:  create the directory.   \n\n\n" ) ;

            fprintf(stderr,
"\n\n%s(%d):  ERROR IN INSTALLATION:  directory '%s' DOESN'T EXIST...\n", 
                __FILE__, __LINE__, base_path) ;
            fprintf(stderr, "Cannot use any directory path.   \n" ) ;
            fprintf(stderr, "TO FIX:  create the directory.   \n\n\n" ) ;

            /* error return code.  */
            return (ERROR) ;

        }
    }
    else 
    {
        /******************************************************************
         *                                                                *
         * undefined APS_DATA is actually an installation ERROR.          *
         * a default should not be used.  the program should not continue *
         * with any directory path.                                       *
         *                                                                *
         ******************************************************************/
        printf(
"\n\n%s(%d):  ERROR IN INSTALLATION:  Environment Variable: '%s' undefined...\n", 
            __FILE__, __LINE__, default_root_envname) ;
        printf("TO FIX:  You must setenv %s\n", 
            default_root_envname) ;
        printf( "No directory path for needed APS files can be used.\n\n\n" ) ;

        fprintf(stderr, 
"\n\n%s(%d):  ERROR IN INSTALLATION:  Environment Variable: '%s' undefined...\n", 
            __FILE__, __LINE__, default_root_envname) ;
        fprintf(stderr, "TO FIX:  You must setenv %s\n", 
            default_root_envname) ;
        fprintf(stderr, 
            "No directory path for needed APS files can be used.\n\n\n" ) ;

        /* error return code.  */
        return (ERROR) ;
    }

    base_path_length = strlen(base_path) ;

    i = 0 ;
    while (aps_path_table[i].name)
    {
        /* full_path size is length of path + slash + name */
        aps_path_table[i].full_path_size = 
            strlen(aps_path_table[i].name) + base_path_length + 1 ;

        /* allocate space to hold the full_path + null */
        aps_path_table[i].full_path = 
            (char *) malloc(aps_path_table[i].full_path_size + 1) ;

        sprintf(aps_path_table[i].full_path, "%s/%s", 
            base_path, aps_path_table[i].name) ;

#ifdef DEBUG
        printf("PATHNAME: %s\n", aps_path_table[i].full_path) ;
#endif
        i++ ;
    }

	return (OK) ;
}



/*==============================================================================
Function:       char * aps_fullpath

Description:    
    This function creates and returns the full pathname of APS path 
    and filename.  If the filename is null it returns only the APS pathname

Parameters:     int id, char *filename 

Returns:        pointer to full pathname and file (if requested)

Creator:        Ron Green

Creation Date:  11/01/1994

Notes:      
Examples of usage:  
    You use the #defines found in the include file apspath.h.  These 
    key into aps_path_table[], which is in this file.  

    #include apspath.h
    ...
    report_dir = aps_fullpath (APS_REPORTS, NULL);
    aps_fullpath_csh_filename = aps_fullpath(APS_TEMP, csh_filename) ;

    APS_REPORTS and APS_TEMP are found in apspath.h.  

    ******
    THE VALUES IN apspath.h FOR THE INDEXES INTO aps_path_table[]
    MUST CORRESPOND (NUMERICALLY) TO THE DEFINITION OF 
        static APS_PATH_TABLE aps_path_table[] 
    IN THIS FILE
    ******

    Programs calling the aps_fullpath function must free the memory
    to the returned pointer


==============================================================================*/
char * aps_fullpath(int id, char *filename) 
{
    char    *fullname ;
    int     return_code ;
    int     filename_length = 0 ;

    /* 
    -- the last element in aps_path_table is a NULL entry and 
    -- cannot be used, therefore aps_path_table_size - 1 is the 
    -- max id allowed, not aps_path_table_size.  
    */
    if ((id < 0) || (id > aps_path_table_size - 1))
        return(NULL) ;

    if (!aps_path_table[id].full_path)
	{
        return_code = create_aps_pathnames() ;
		if ( return_code < 0 )
			return(NULL) ;
	}

    if (filename)
        filename_length = strlen(filename) ;

    fullname = (char *) malloc(
        aps_path_table[id].full_path_size + 1 + filename_length + 1)  ;

    if (!fullname)
        return(NULL) ;

    if (filename)
    {
        sprintf(fullname, "%s/%s", 
            aps_path_table[id].full_path, filename) ;
    }
    else
        sprintf(fullname, "%s", aps_path_table[id].full_path) ;
    
    return(fullname) ;
}

/* small driver to test table */
#ifdef DEBUG
main(int argc, char *argv[])
{
    int i ;
    while (aps_path_table[i].name)
    {
        printf("APS PATH: %s\n", aps_fullpath(i, NULL)) ;
        printf("APS FILENAME: %s\n", aps_fullpath(i, "filename")) ;
        i++ ;
    }
}
#endif


/*==============================================================================
Function:       aps_pathname2filename

Description:    truncates a full pathname to its file name

Parameters:     
Type        Name                Definition
char        *pathname           pathname to be truncated  
char        *file_name          resulting file name 

Returns:
Type          Name              Definition
*char                           pointer to the new file name = success  
                                NULL = error

Creator:        Miguel Siu

Creation Date:  Wed Jun 28 11:02:10 PDT 1995

Notes:      Calling routines should use  free(truncated_name);  to release 
            memory allocated by this routine.
==============================================================================*/
char * aps_pathname2filename (char  *pathname)
{
    char *name_start;
    char *newname; 
    int  newname_length = 0;
    int  slash = '/';

    if (!pathname)
        return (NULL);

    name_start = strrchr(pathname, slash);

    if (name_start == NULL)
    {
        /*
        -- this is already a file_name
        */
        name_start = pathname;
    }
    else
    {
        /*
        -- increment pointer to the start of the file name
        */
        name_start++;
    }

    newname_length = strlen(name_start);

    newname = (char *) malloc(newname_length + 1)  ;
    if (!newname)
        return(NULL);

    sprintf(newname, "%s", name_start);
 
#ifdef PRINT_DIAG
    printf("IN aps_pathname2filename, original:%s \nnew:%s\n\n",
        pathname, newname);
#endif
    return(newname) ;
}

