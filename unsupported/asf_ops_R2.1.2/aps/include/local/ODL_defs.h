#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   ODL_defs.h
Description:    
    Definitions for ODL files created from a dtk llist of DB_RECORDs
Creator:    Lawrence Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)ODL_defs.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.ODL_defs.h"


#ifndef __ODL_DEFS_H__
#define __ODL_DEFS_H__

#include "fa_defs.h"     /* this file only adds what is not in fa_defs.h  */

#include <stdio.h>
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */

/*
-- SOURCE AND DESTINATION DEFINITIONS.
*/
#define ODL_CONTROL          1
#define ODL_DEFAULT          2
#define ODL_DTK_RECORD       3
#define ODL_FILE_HEADER      4
#define ODL_FILE_TRAILER     5
#define ODL_FILE_RECORD      6
#define ODL_FILE_SUBRECORD   7

typedef
    union _SOURCE_TYPE
    {
        int     index ;
        void    *pointer ;
    } SOURCE_TYPE;


typedef 
    struct _ODL_CREATE_VALUE_DEFS 
    {
        int                 source_code;
        int                 destination_code;
        char                *keyword;
        int                 (*conversion)();
        EQUIV_TABLE         *conversion_table ;
        SOURCE_TYPE         source;
    } ODL_CREATE_VALUEDEFS ;

typedef
    struct _ODL_CREATE_FILENAMES
    {
        char                   *create_file_type_name ;
        ODL_CREATE_VALUEDEFS   *create_file_valuedefs ;
 
    } ODL_CREATE_FILENAME ;

#endif   /*   __ODL_DEFS_H__    */
