#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		main.c

Description:	main routine for a test driver for the multi-user library.

==============================================================================*/
#pragma ident	"@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.main.c"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "timeconv.h"

#include "mu.h"
#include "aps_defs.h"

#include "db_active_planning_activities.h"
#include "db_active_dar_activities.h"
#include "db_active_single_activities.h"

extern int  init_vec_lib();         /* initializes vector lib with stoicfile */
extern int  init_vec_lib_exit(int); /* prints message, exits if stoic error */

extern          DBPROCESS*      opendb(char*, char*);


/*==============================================================================
Function:       usage_exit()
 
Description:    prints usage and exits with an error code.
 
Creator:        Lawrence Stevens
 
Creation Date:  Tue Dec 24 22:06:23 PST 1996
 
Notes:
    This file was written with a 4-character tab setting.  If you don't use
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to
    browse.
==============================================================================*/
static usage_exit(char *progname, int code )
{
(void)printf( 
	"usage:  %s  [-U sybase_Userid] -P sybase_Password \n", progname );
(void)printf( 
	"\nThis program is a test driver for the multi-user library.  It\n");
(void)printf( 
	"reads from the database indicated by environment variable $APSDB\n\n");

exit(code) ;

}

/*==============================================================================
Function:       userstr()

Description:    puts a user promp all into one string.  

Creator:        Lawrence Stevens

Creation Date:  Sun Dec  8 20:06:07 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int userstr(char *str)
{
    /* end of line character <cr> is a ten.     */
    #define EOL     10
 
    int     j;
    char    c;
 
    j = 0;
    for(;;)
    {
        c = getchar();
        if( c == EOF )
            return EOF ;
        if( c == EOL )
            break ;
        *(str+j++) = c;
    }

    /* terminate the users string   */
    str[j] = '\0';

    return 0 ;

}

int main( int argc, char  *argv[] ) 
{

DBPROCESS       *APS_dbproc;

char    *env_sybase_dbname = NULL ;
char    *env_sybase_userid = NULL ;
char    *Sybase_dbname = NULL ;
char    *Sybase_userid = NULL ;
char    *Sybase_password = NULL ;

int     return_code ;
int     j, k ;

llist       *perm_list = NULL ;
llist       *perm_list2 = NULL ;
llist       *perm_list3 = NULL ;
int         darid ;
char        perm_command[256] ;

int         permission_id ;

int         mu_argc = 0 ;
int         previous_mu_argc = 0 ;
char        mu_argv[10][50] ;
char        previous_mu_argv_0[50] ;
char        *progname ;

/* get the Sybase_userid (optional) and Sybase_password (mandatory)    */
/* int  getopt(int argc, char * const *argv, char *optstring);  */
extern  char    *optarg;
 
int     c;      /* used as return character from getopt()       */
 
int     Pflag = 0;  /* used to check for mandatory password     */
int     Uflag = 0;  /* used to check for optional Sybase_userid */
 
char    flag_list[100] = "P:U:";  /* list of flags for getopts  */


#ifdef VECTOR_LIB_INIT
/* initialize the vector library; exit with a message if an error occurs   */
return_code = init_vec_lib();
if(return_code)
    init_vec_lib_exit(return_code);
#endif   /*  VECTOR_LIB_INIT  */

/* find pointer to last occurrence of '/', if any, in the string:  */
progname = strrchr( argv[0], '/' ) ;
if( progname == NULL )
    progname = argv[0] ;
else
    progname ++ ;

while ((c = getopt(argc, argv, flag_list)) != EOF)
    switch (c)
    {
        case 'P':
            if(Pflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Pflag++;
            Sybase_password = optarg ;
            break;
        case 'U':
            if(Uflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Uflag++;
            Sybase_userid = optarg ;
            break;
        case '?':
            usage_exit(progname, APS_EXIT_ERROR);
            break;
        default:
            /* do  nothing  */
            break;
    }
/*
--     -P is manditory
*/
if(Pflag <= 0)
    usage_exit(progname, APS_EXIT_ERROR);

if(Uflag == 0)
{
    /* Sybase_userid not supplied in command line.      */
    /* obtain from the environment:                     */
    env_sybase_userid = getenv("APS_SYBASE_USERID");
    if(env_sybase_userid == NULL)
    {
        /* userid not supplied at all   */
        (void)printf("environment variable APS_SYBASE_USERID not set.\n");
        (void)printf("Use -U in command line or setenv. \n");
        exit(APS_EXIT_ERROR);
    }
    else
    {
        /* use the environment Sybase_userid.   */
        Sybase_userid = env_sybase_userid ;
    }
}

/* get the database name from the environment   */
env_sybase_dbname = getenv("APSDB");
if(env_sybase_dbname == NULL)
{
    /* database name not supplied   */
    (void)printf("Environment variable APSDB not set.\n");
    (void)printf("Use setenv APSDB dbname. \n");
    exit(APS_EXIT_ERROR);
}
 
Sybase_dbname = env_sybase_dbname ;
 
/* now open the database:   */
/* db_open will handle the errors.       */
APS_dbproc = db_open(Sybase_dbname, progname, Sybase_userid, Sybase_password,
    NULL,error_handler_exit,&return_code);
if(return_code != DB_OPEN_OK)
{
    db_open_errs(return_code, Sybase_dbname, Sybase_userid);
    (void)fflush(stdout) ;
    (void)system("banner ERROR");
    exit(1);
}

(void)printf("%s Compiled %s %s\n", progname, __DATE__, __TIME__ ) ;

(void)printf("%s(%d):  Multi-User Test Driver reading from DB: %s", 
	__FILE__, __LINE__, Sybase_dbname ) ;

for( k = 0 ; k < 1 ; j++ )
{
(void)printf("\n\n#############################################################\n") ;
if( previous_mu_argc == 0 )
{
    (void)printf( "Type 'help', permission command, or 'exit'.\n");
}
(void)printf("permission cmd> ");
return_code = userstr(perm_command);
if( return_code == EOF )
{
    (void)printf("\n%s:  exiting on EOF\n", progname ) ;
    exit(0) ;
}

mu_argc = sscanf( perm_command, "%s %s %s %s %s %s %s",
    mu_argv[0], mu_argv[1], mu_argv[2], mu_argv[3], mu_argv[4], 
    mu_argv[5], mu_argv[6] ) ;

if( mu_argc <= 0 )
{
    (void)printf("\n") ;
    continue ;
}

if( strcmp( mu_argv[0], "!!" ) == 0 )
{
    if( previous_mu_argc == 0 )
    {
        (void)printf("You don't have a previous command\n" ) ;
        continue ;
    }
    mu_argc = previous_mu_argc ;
    (void)strcpy( mu_argv[0], previous_mu_argv_0 ) ;
}

if( strcmp( mu_argv[0], "exit" ) == 0 
||  strcmp( mu_argv[0], "quit" ) == 0 
||  strcmp( mu_argv[0], "bye" ) == 0 )
{
    (void)printf("\n") ;
    exit(0) ;
}

if( strcmp( mu_argv[0], "help"  ) == 0 
||  strcmp( mu_argv[0], "?"     ) == 0 
||  strcmp( mu_argv[0], "usage" ) == 0 )
{
    (void)printf("usage: \n" ) ;
    (void)printf("  handles commands like this:\n" ) ;
    (void)printf("    request planning <mu_activity_id> <strt> <stop> <station>\n");
    (void)printf("                [ <permission_id> ]\n" ) ;
    (void)printf("    request DAR <mu_activity_id> <darid>\n" ) ;
    (void)printf("                [ <permission_id> ]\n" ) ;
    (void)printf("    request single <mu_activity_id>\n" ) ;
    (void)printf("                [ <permission_id> ]\n" ) ;
    (void)printf("    terminate <activity_type> <mu_activity_id> <permission_id>\n");
    (void)printf("    validate <activity_type> <mu_activity_id> <permission_id>\n");
    (void)printf("    retrieve \n" ) ;
    (void)printf("    help\n");
    (void)printf("    exit\n");
    continue ;
}
else
{
    (void)strcpy( previous_mu_argv_0, mu_argv[0]) ;
    previous_mu_argc = mu_argc ;
}

/*
--  now handle commands like this:
--      request planning mu_activity_id strt stop station
--              [ permission_id ]
--      request DAR mu_activity_id darid
--              [ permission_id ]
--      request single mu_activity_id
--              [ permission_id ]
--      terminate activity_type mu_activity_id permission_id
--      validate activity_type mu_activity_id permission_id
--      retrieve 
*/
for ( j = 0; j < mu_argc ; j++ )
    (void)printf("%s ", mu_argv[j] ) ;
(void)printf("\n") ;

if( strcmp(mu_argv[0], "request") == 0 )
{  /* permission request     */
    if( mu_argc < 2 )
    {
        (void)printf("%s(%d): %d arguments - need more.\n",  __FILE__, __LINE__, 
            mu_argc ) ;
        continue ;
    }
    perm_list = create_dyn_llist() ;
    if( strcmp(mu_argv[1], "planning") == 0 )
    {   /*  
        --    0       1       2            
        --  request planning mu_activity_id 
        --            3          4        5             6
        --          strttime stoptime station_id [ permission_id ]
        */
        if( mu_argc < 6 )
        {
            (void)printf("%s(%d): %d arguments - need more.\n",  __FILE__, __LINE__, 
                mu_argc ) ;
            continue ;
        }
        permission_id = 0 ;
        if( mu_argc == 7)
        {
            return_code = sscanf( mu_argv[6], "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                (void)printf("%s(%d): %s can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, mu_argv[6] ) ;
                continue ;
            }
        }
        return_code = mu_permission_request( APS_dbproc, permission_id, 
            mu_argv[2],     /*  mu_activity_id               */
            mu_argv[1],     /*  activity_type                */
            perm_list,   /*  blocking_permission_list     */
            mu_argv[3],     /*  strttime                     */
            mu_argv[4],     /*  stoptime                     */
            mu_argv[5],     /*  station_id                   */
            0 ) ;        /*  darid                        */
        if( return_code < 0 )
        {
            (void)printf("%s(%d):  request planning error code %d\n", 
                __FILE__, __LINE__, return_code ) ;
            (void)printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
            continue ;
        }
        else if (return_code == 0 )
        {
            (void)printf("%s(%d):  request planning DENIED \n", 
                __FILE__, __LINE__ ) ;
            (void)printf("%s(%d):  Blocking planning perms:\n", __FILE__, __LINE__ ) ;
            db_print_list( perm_list, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;
            continue ;
        }
        else
        {
            (void)printf("%s(%d):  request planning GRANTED permission_id = %d \n", 
                __FILE__, __LINE__, return_code ) ;
            continue ;
        }
    }  /*  request planning     */
    else if( strcmp(mu_argv[1], "DAR") == 0 )
    {   /*  request DAR   
        --    0      1     2            3        4
        -- request DAR mu_activity_id darid [permission_id]
        */
        if( mu_argc < 4 )
        {
            (void)printf("%s(%d): %d arguments request DAR - need more.\n",  
                __FILE__, __LINE__, mu_argc ) ;
            continue ;
        }
        return_code = sscanf( mu_argv[3], "%d", &darid ) ;
        if( return_code != 1 )
        {
            (void)printf("%s(%d): %s can't be scanned into an int for darid\n",
                __FILE__, __LINE__, mu_argv[3] ) ;
            continue ;
        }
        permission_id = 0 ;
        if( mu_argc == 5)
        {
            return_code = sscanf( mu_argv[4], "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                (void)printf("%s(%d): %s can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, mu_argv[4] ) ;
                continue ;
            }
        }
        return_code = mu_permission_request( APS_dbproc, permission_id, 
            mu_argv[2],     /*  mu_activity_id               */
            mu_argv[1],     /*  activity_type                */
            perm_list,   /*  blocking_permission_list     */
            NULL,        /*  strttime                     */
            NULL,        /*  stoptime                     */
            NULL,        /*  station_id                   */
            darid ) ;    /*  darid                        */
        if( return_code < 0 )
        {
            (void)printf("%s(%d):  request DAR error code %d\n", 
                __FILE__, __LINE__, return_code ) ;
            (void)printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
            continue ;
        }
        else if (return_code == 0 )
        {
            (void)printf("%s(%d):  request DAR DENIED \n", 
                __FILE__, __LINE__ ) ;
            (void)printf("%s(%d):  Blocking DAR perms:\n", __FILE__, __LINE__ ) ;
            db_print_list( perm_list, APS_CDEFS(ACTIVE_DAR_ACTIVITIES) ) ;
            continue ;
        }
        else
        {
            (void)printf("%s(%d):  request DAR GRANTED permission_id = %d \n", 
                __FILE__, __LINE__, return_code ) ;
            continue ;
        }
    }  /*  request DAR     */
    else if( strcmp(mu_argv[1], "single") == 0 )
    {   /*  
        -- request single     
        --    0      1        2              3
        -- request single mu_activity_id [permission_id]
        */
        if( mu_argc < 3 )
        {
            (void)printf("%s(%d): %d arguments request single - need more.\n",  
                __FILE__, __LINE__, mu_argc ) ;
            continue ;
        }
        permission_id = 0 ;
        if( mu_argc == 4)
        {
            return_code = sscanf( mu_argv[3], "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                (void)printf("%s(%d): %s can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, mu_argv[3] ) ;
                continue ;
            }
        }
        return_code = mu_permission_request( APS_dbproc, permission_id, 
            mu_argv[2],     /*  mu_activity_id               */
            mu_argv[1],     /*  activity_type                */
            perm_list,   /*  blocking_permission_list     */
            NULL,        /*  strttime                     */
            NULL,        /*  stoptime                     */
            NULL,        /*  station_id                   */
            0 ) ;        /*  darid                        */
        if( return_code < 0 )
        {
            (void)printf("%s(%d):  request single error code %d\n", 
                __FILE__, __LINE__, return_code ) ;
            (void)printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
            continue ;
        }
        else if (return_code == 0 )
        {
            (void)printf("%s(%d):  request single DENIED \n", 
                __FILE__, __LINE__ ) ;
            (void)printf("%s(%d):  Blocking single perms:\n", __FILE__, __LINE__ ) ;
            db_print_list( perm_list, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES) ) ;
            continue ;
        }
        else
        {
            (void)printf("%s(%d):  request single GRANTED permission_id = %d \n", 
                __FILE__, __LINE__, return_code ) ;
            continue ;
        }
    }  /*  request single     */
    else
    {
        (void)printf("%s(%d):  %s %s bad second argument\n", __FILE__, __LINE__, 
            mu_argv[0], mu_argv[1] ) ;
        continue ;
    }
}
else if( strcmp(mu_argv[0], "terminate") == 0 )
{   /* 
    -- permission terminate     
    --        0        1               2              3
    --      terminate activity_type mu_activity_id permission_id
    */
    if( mu_argc < 4 )
    {
        (void)printf("%s(%d): %d arguments terminate - need more.\n",  
            __FILE__, __LINE__, mu_argc ) ;
        continue ;
    }
    return_code = sscanf( mu_argv[3], "%d", &permission_id ) ;
    if( return_code != 1 )
    {
        (void)printf("%s(%d): %s can't be scanned into int permission_id\n",
            __FILE__, __LINE__, mu_argv[3] ) ;
        continue ;
    }
    return_code = mu_permission_terminate( APS_dbproc,
        permission_id,
        mu_argv[2],        /*  activity_id    */
        mu_argv[1] ) ;     /*  activity_type  */
    if( return_code < 0 )
    {
        (void)printf("%s(%d):  permission terminate error code %d\n", 
            __FILE__, __LINE__, return_code ) ;
        (void)printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
        continue ;
    }
    else
    {
        (void)printf("%s(%d):  permission terminate return code %d\n", 
            __FILE__, __LINE__, return_code ) ;
        continue ;
    }
} /* permission terminate        */
else if( strcmp(mu_argv[0], "validate") == 0 )
{   /* 
    -- permission validate     
    --        0        1               2            3
    --      validate activity_type mu_activity_id permission_id
    */
    if( mu_argc < 4 )
    {
        (void)printf("%s(%d): %d arguments validate - need more.\n",  
            __FILE__, __LINE__, mu_argc ) ;
        continue ;
    }
    return_code = sscanf( mu_argv[3], "%d", &permission_id ) ;
    if( return_code != 1 )
    {
        (void)printf("%s(%d): %s can't be scanned into int permission_id\n",
            __FILE__, __LINE__, mu_argv[3] ) ;
        continue ;
    }
    return_code = mu_permission_validate( 
        permission_id,
        mu_argv[2],        /*  activity_id    */
        mu_argv[1] ) ;     /*  activity_type  */
    if( return_code < 0 )
    {
        (void)printf("%s(%d):  permission validate error code %d\n", 
            __FILE__, __LINE__, return_code ) ;
        (void)printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
        continue ;
    }
    else
    {
        (void)printf("%s(%d):  permission validate return code %d\n", 
            __FILE__, __LINE__, return_code ) ;
        continue ;
    }
} /* permission validate        */
else if( strcmp(mu_argv[0], "retrieve") == 0 )
{   /* 
    -- permission retrieve  
    --    0     
    --  retrieve 
    */
    perm_list  = create_dyn_llist() ;
    perm_list2 = create_dyn_llist() ;
    perm_list3 = create_dyn_llist() ;
    return_code = mu_permission_retrieve( APS_dbproc,
        perm_list,       /* planning list  */
        perm_list2,      /* dar list       */
        perm_list3 ) ;   /* single list    */
    if( return_code < 0 )
    {
        (void)printf("%s(%d):  permission retrieve error code %d\n", 
            __FILE__, __LINE__, return_code ) ;
        (void)printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
        continue ;
    }
    else
    {
        (void)printf("%s(%d):  permission retrieve count = %d\n", 
            __FILE__, __LINE__, return_code ) ;
        (void)printf("Planning perms:  ---------------------------------------\n" ) ;
        db_print_list( perm_list, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;
        (void)printf("\nDAR perms:  ---------------------------------------\n" ) ;
        db_print_list( perm_list2, APS_CDEFS(ACTIVE_DAR_ACTIVITIES) ) ;
        (void)printf("\nSingle perms:  ---------------------------------------\n" ) ;
        db_print_list( perm_list3, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES) ) ;
        continue ;
    }
}
else
{
    (void)printf("%s(%d):  %s bad first argument\n", __FILE__, __LINE__, mu_argv[0] ) ;
}
}  /* end for promp.  */
exit(0) ;

}
