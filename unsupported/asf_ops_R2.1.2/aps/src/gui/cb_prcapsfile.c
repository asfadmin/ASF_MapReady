#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       cb_prcapsfile.c

Description:

External Functions Defined:
                cb_fill_fileproc_list
                cb_process_all_files
                cb_stop_file_processing

File Scope Functions:
                getList_subprocess_done
                processFile_subprocess
                create_perm_data_string
                create_list_of_files
                file_processing_cleanup
                list_files_not_processed
                free_fp_file_item
                free_unused_perms
                fp_set_signals
                fp_sighandler

External Variables Defined:

File Scope Variables:

Notes:

==============================================================================*/
#pragma ident   "@(#)cb_prcapsfile.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_prcapsfile.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <values.h>

#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include "UxXt.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_exe_names.h"
#include "aps_extern.h"
#include "mu_utilities.h"
#include "apspath.h"
#include "apsfiledef.h"

#define CONTEXT_MACRO_ACCESS
#include "vc_prcapsfile.h"
#undef CONTEXT_MACRO_ACCESS
#include "cb_prcapsfile.h"
#include "prcapsfile_main.h"
#include "vc_msgbox.h"
#include "subprocess.h"
#include "gui_utils.h"
#include "gui_mu_utils.h"


/*==============================================================================
    Macro/Constant Definitions
==============================================================================*/

#define TMP_FILENAME_PREFIX "prcapsfile_tmp"        /* for tmp storage of set */

/* returns from internal functions to another internal function */
#define FP_OK_RETURN        0
#define FP_OPEN_ERR         -50     /* errno is set on return */
#define FP_FMT_ERR          -55
#define FP_READ_ERR         -60     /* errno is set on return */

/* how to set the pertinent signals */
#define FP_IGNORE_SIG       0
#define FP_HANDLE_SIG       1
#define FP_RESTORE_SIG      2

#define NO_TYPE             -1      /* indicates "no" file type */

#define BEGIN_CONTEXT(widget) \
    _UxCAPSFileProcessing          *UxSaveCtx; \
    UxSaveCtx = UxAPSFileProcessingContext; \
    UxAPSFileProcessingContext = \
            (_UxCAPSFileProcessing *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
    } \
    UxAPSFileProcessingContext = UxSaveCtx;

/*==============================================================================
    Function Declarations
==============================================================================*/
extern void     popup_message();
static void     fp_sighandler( int signal );

/*==============================================================================
    Global Variable Declarations
==============================================================================*/
extern XtAppContext UxAppContext ;

extern char     display_string[] ;

static int      *permArray ;    /* array of active file perm.s */
static int      numPerms = 0 ;  /* number of active permissions */
static int      num_fileTypes ; /* number of file processing file types */

static int      fp_stop_flag = FALSE;   /* TRUE if STOP button was activated */
static int      SignalCaught = 0;
static int      fp_active_childPid = 0;
static int      fp_processing_done = FALSE; /* indicates need to clean up
                                            -- last completed job
                                            */
static int      list_not_processed = FALSE; /* TRUE if have a list, but
                                            -- haven't started processing */
/* this retains its value until the gui process dies */
static char     *tmpFileList_fullpathFileRoot = NULL;
/* this changes values, but it's storage space is retained until the gui dies */
static char     *tmpFileList_fullpathFilename = NULL; /* storage is alloc'ed */

typedef
    struct FILEPROC_FILES
    {
        char    *filename;          /* filename portion, no directory elements*/
        char    *file_fulldirpath;  /* full directory path, no filename */
    } FILEPROC_FILES;

static FILEPROC_FILES   *fpFilePtr=NULL;/* ptr to the fp file rec in curr item*/
static llist    *fileproc_files = NULL; /* list of files to be processed */
static cursor   filesListPtr=NULL;      /* ptr to the current list item */
static int      num_fileproc_files = 0; /* number of items in the list */

/* save original values of the interrupts (if NULL, use SIG_DFL) */
static void     (*origINT)() = SIG_ERR;
static void     (*origTERM)() = SIG_ERR;
static void     (*origQUIT)() = SIG_ERR;



/*==============================================================================
Function:       fp_set_signals

Description:    sets all pertinent signals

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  12/05/95

Notes:
==============================================================================*/
static void
fp_set_signals( int action )
{
    void    (*disposition)();

    if (action == FP_RESTORE_SIG)
    {
        if (origINT != SIG_ERR)
        {
            (void) signal( SIGINT, origINT );
            (void) signal( SIGTERM, origTERM );
            (void) signal( SIGQUIT, origQUIT );
        }
    }
    else
    {
        switch (action)
        {
            case FP_HANDLE_SIG:
                disposition = fp_sighandler;
                break;
            default:
                disposition = SIG_IGN;
                break;
        }

        if (origINT == SIG_ERR)
        {
            origINT = signal( SIGINT, disposition );
            origTERM = signal( SIGTERM, disposition );
            origQUIT = signal( SIGQUIT, disposition );
        }
        else
        {
            (void) signal( SIGINT, disposition );
            (void) signal( SIGTERM, disposition );
            (void) signal( SIGQUIT, disposition );
        }
    }

    return;
}


/*==============================================================================
Function:       list_files_not_processed

Description:    lists the unprocessed files in the tmp files.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  11/30/95

Notes:
==============================================================================*/
static void
list_files_not_processed()
{
    FILE    *tmpFile_fp ;
    char    buf[FP_MAX_FILE_LINE + 1];
    char    *bufPtr;
    char    *fileName;
    char    *filePath;
    char    printBuf[BUFSIZ] ;
    int     printLen = 0 ;
    char    tmpBuf[BUFSIZ] ;
    int     tmpLen = 0 ;
    int     i ;

    /* if was "stopped", ie, caught a signal */
    if (!SignalCaught)
    {
        /* list files as not processed (if no files, do not list anything) */
        (void) sprintf( printBuf, "***Stopped***\n" ) ;
        printLen = strlen( printBuf ) ;
        for (i = 0 ; reports_inbound[i].type != NULL ; i++)
        {
            (void) sprintf( tmpFileList_fullpathFilename, "%s_%s",
                    tmpFileList_fullpathFileRoot, reports_inbound[i].type ) ;
            if ((tmpFile_fp = fopen( tmpFileList_fullpathFilename, "r" ))
                    != NULL)
            {
                while (fgets( buf, FP_MAX_FILE_LINE + 1, tmpFile_fp ) != NULL)
                {
                    if ((bufPtr = strrchr( buf, NEWLINE )) != NULL)
                        *bufPtr = STREND;

                    if ((fileName = strtok(  buf, FP_FILE_SEPARATORS )) != NULL
                            && (filePath = strtok( NULL, FP_FILE_SEPARATORS ))
                            != NULL)
                    {
                        /* add file to message for gui message window */
                        (void) sprintf( tmpBuf, "    %s%s not processed\n",
                                filePath, fileName );
                        if (printLen + (tmpLen = strlen( tmpBuf )) > BUFSIZ)
                        {
                            gui_display_message_widget( scrolledText_procfile,
                                printBuf );
                            printBuf[0] = STREND ;
                            printLen = 0 ;
                        }
                        (void) strcat( printBuf, tmpBuf ) ;
                        printLen += tmpLen ;
                    }
                }

                (void) fclose( tmpFile_fp );
            }
        }
        if (tmpLen > 0)
        {
            gui_display_message_widget( scrolledText_procfile,
                printBuf );
        }
    }

    return;
}



/*==============================================================================
Function:       file_processing_cleanup

Description:    called when file processing is terminating: either
                processing has ended or stop was activated OR a signal
                was caught.  Readies to begin file processing over again
                unless a signal was caught, then causes aps to EXIT.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  11/16/95

Notes:          Doesn't clear the scrolled messages, in case user is still
                reading them (these get cleared when begin getting a new
                list of files to be processed).

==============================================================================*/
static void
file_processing_cleanup()
{
BEGIN_CONTEXT( APSFileProcessing )

    int     i ;

    fp_set_signals( FP_IGNORE_SIG );    /* disable signals */

    /* release all permissions */
    for (i = 0 ; numPerms && reports_inbound[i].type != NULL ; i++)
    {
        if (permArray[i] != 0)
        {
            (void) gui_free_permission( permArray[i],
                    reports_inbound[i].mu_activityid,
                    MU_SINGLE_ACTIVITY_TYPE ) ;
            numPerms-- ;
            permArray[i] = 0 ;
        }
    }

    /* if created a list of files, but have not yet started processing them */
    if (list_not_processed == TRUE)
    {
        list_files_not_processed();
        list_not_processed = FALSE;
    }

    if (SignalCaught) /* EXITing, so can skip rest of cleanup */
    {
        /* restore signals and then reissue interrupt */
        fp_set_signals( FP_RESTORE_SIG );
        (void) kill( getpid(), SignalCaught );  /* EXITs */
    }

    /*
    -- reset initialized global values to original values
    */

    /* unlink all the tmp files */
    for (i = 0 ; reports_inbound[i].type != NULL ; i++)
    {
        (void) sprintf( tmpFileList_fullpathFilename, "%s_%s",
                tmpFileList_fullpathFileRoot, reports_inbound[i].type ) ;
        (void) unlink( tmpFileList_fullpathFilename );
    }

    if (fileproc_files != NULL)
    {
        DEL_LIST( fileproc_files );
        fileproc_files      = NULL;
        fpFilePtr           = NULL;
        filesListPtr        = NULL;
        num_fileproc_files  = 0;
    }

    fp_stop_flag = FALSE;
    SignalCaught = 0;
    fp_active_childPid = 0;
    fp_processing_done = FALSE;

    /*
    -- restore the gui to creation state
    */

    XtSetSensitive( getList_menuBar, True ) ;
    XtSetSensitive( pushButton_process, False ) ;
    XtSetSensitive( pushButton_stop, False ) ;
    XtVaSetValues( scrolledList_reports_inbound,
        XmNitemCount, 0,
        NULL );

    fp_set_signals( FP_RESTORE_SIG );

END_CONTEXT

    return;
}



/*==============================================================================
Function:       fp_sighandler

Description:    executed when all the files are locked but there is no
                get or process subprocess running (for aps quitting).
                Restores the files to clean up from unfinished fp processing,
                restores signals to their original value, then reissues
                signal so self will now do what it was intended to do.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  12/05/95

Notes:
==============================================================================*/
static void
fp_sighandler( int signal )
{
BEGIN_CONTEXT( APSFileProcessing )

    fp_set_signals( FP_IGNORE_SIG );    /* disable signals */

    SignalCaught = signal;

    if (fp_active_childPid != 0)
    {
        /* delay handling until child has died */
        (void) sprintf( display_string,
            "Will stop when done with current file" );
        popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
            display_string, XtGrabNone ) ;
    }
    else
    {
        /* handle signal right away */
        file_processing_cleanup();  /* EXITs */
    }

    /* should be exiting, so disable all */
    XtSetSensitive( pushButton_stop, False ) ;
    XtSetSensitive( getList_menuBar, False ) ;
    XtSetSensitive( pushButton_process, False ) ;

END_CONTEXT

    return;
}



/*==============================================================================
Function:       free_fp_file_item

Description:    on deletion, frees elements within a file list item
                and the item itself.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  11/30/95

Notes:
==============================================================================*/
static void
free_fp_file_item( FILEPROC_FILES *item )
{
    free( item->file_fulldirpath );
    free( item->filename );
    free( item );

    return;
}



/*==============================================================================
Function:       create_list_of_files

Description:    reads the tmp files and creates the list of files for display

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  12/02/95

Notes:          If returns an error code, it has already popped up an error
                message and "reset file processing for next invocation."
==============================================================================*/
static int
create_list_of_files()
{
    FILE            *tmpFile_fp;
    char            buf[FP_MAX_FILE_LINE + 1];
    char            *bufptr_filePath;   /* ptr into buf at file path */
    char            *bufptr_fileName;   /* ptr into buf at file name */
    char            *filePath;          /* malloc'ed file path for list item */
    char            *fileName;          /* malloc'ed file name for list item */

BEGIN_CONTEXT( APSFileProcessing )

    int     i ;

    if (fileproc_files != NULL)     /* should have been freed before here */
        DEL_LIST( fileproc_files );
    fileproc_files = create_dyn_llist();

    for (i = 0 ; reports_inbound[i].type != NULL ; i++)
    {
        (void) sprintf( tmpFileList_fullpathFilename, "%s_%s",
                tmpFileList_fullpathFileRoot, reports_inbound[i].type ) ;

        if ((tmpFile_fp = fopen( tmpFileList_fullpathFilename, "r" )) == NULL)
        {
            if (errno == ENOENT)
                continue ;
            else
            {
                (void)sprintf( display_string,
                    "can't list files to process (fopen errno: %d)\n    file: %s\n",
                    errno, tmpFileList_fullpathFilename );
                popup_message( XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone);
                /* reset for next invocation: as if had completed file proc. */
                file_processing_cleanup();
                return (FP_OPEN_ERR);
            }
        }

        while (fgets( buf, FP_MAX_FILE_LINE + 1, tmpFile_fp ) != NULL)
        {
            if ((bufptr_fileName = strtok(  buf, FP_FILE_SEPARATORS )) == NULL
                    || (bufptr_filePath =
                    strtok( NULL, FP_FILE_SEPARATORS )) == NULL)
            {
                (void) sprintf( display_string,
                    "incorrectly formatted line in %s;\n",
                    tmpFileList_fullpathFilename );
                popup_message( XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone);
                /* reset for next invocation: as if had completed file proc. */
                (void) fclose( tmpFile_fp );
                file_processing_cleanup();
                return (FP_FMT_ERR);
            }

            fileName = (char *) malloc( strlen( bufptr_fileName ) + 1 );
            (void) strcpy( fileName, bufptr_fileName );
            filePath = (char *) malloc( strlen( bufptr_filePath ) + 1 );
            (void) strcpy( filePath, bufptr_filePath );

            /*
            -- add this file to file list
            */
            fpFilePtr = (FILEPROC_FILES *) malloc( sizeof (FILEPROC_FILES) );
            fpFilePtr->filename = fileName;
            fpFilePtr->file_fulldirpath = filePath;
            APPEND( fileproc_files, fpFilePtr, free_fp_file_item, fpFilePtr );
            num_fileproc_files++;
        }

        if (!feof( tmpFile_fp ))  /*not eof; error occurred while reading file*/
        {
            (void)sprintf( display_string,
                "can't list files to process (fgets errno: %d)\n    file: %s",
                errno, tmpFileList_fullpathFilename );
            popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone);
            /* reset for next invocation: as if had completed file proc. */
            (void) fclose( tmpFile_fp );
            file_processing_cleanup();
            return (FP_READ_ERR);
        }

        (void) fclose( tmpFile_fp );
    }
END_CONTEXT

    return (FP_OK_RETURN);
}



/*==============================================================================
Function:       create_perm_data_string

Description:    Creates the data to be used as input by the child
                process a character "stream" of:

                    the number of permissions
                    for each permission: the type, a space, the perm. id

Parameters:     type: type string or NULL for "all"

Returns:        the created "stream" of data, or NULL if no perms are found

Creator:        Teresa McKillop

Creation Date:  01/17/97

Notes:          the caller needs to FREE a non-NULL returned string
==============================================================================*/
static char *
create_perm_data_string( char *type )
{
    static int  maxintLen = 0;
    char        *dataStr ;
    int         numPermsInStr ;
    int         strLen ;
    int         i ;

    if (!maxintLen)
    {
        int     tmpint = MAXLONG ;

        for ( ; tmpint > 0 ; maxintLen++ )
            tmpint /= 10 ;
    }

    /*
    -- calculate the space required for:
    --      the length of the number of perms and a newline
    --      for each perm: the type and perm id length and a space and a newline
    */
    numPermsInStr   = (type) ? 1 : numPerms ;
    strLen =    maxintLen + 1
                + numPermsInStr * (MAX_RPT_TYPE_LEN + maxintLen + 2) ;

    if ((dataStr = calloc( 1, strLen )) != NULL)
    {
        for (i = 0 ; reports_inbound[i].type != NULL ; i++)
        {
            if (!type || !strcmp( type, reports_inbound[i].type))
            {
                if (permArray[i] != 0)
                {
                    if (*dataStr == STREND)
                        (void) sprintf( dataStr, "%d\n", numPermsInStr ) ;

                    (void) sprintf( dataStr, "%s%s %d\n",
                            dataStr, reports_inbound[i].type, permArray[i] ) ;
                }
                if (type)   /* done with requested type */
                    break ; /* break out of for-loop through reports_inbound */
            }
        }

        if (*dataStr == STREND) /* no valid perm.s for the desired types */
        {
            free( dataStr ) ;
            dataStr = NULL ;
        }
    }

    return (dataStr) ;
}


/*==============================================================================
Function:       free_unused_perms

Description:    Before processing begins on individual files, for each
                file type for which a permission has been obtained, if
                there are no files to process for that type, releases
                the permission.

Parameters:     None

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/20/96

Notes:
==============================================================================*/
static void
free_unused_perms()
{
    FILE        *fp ;
    int         i ;

    /*
    -- if there is no tmp file for a file type, release its permission
    */

    for (i = 0 ; reports_inbound[i].type != NULL ; i++)
    {
        if (permArray[i] != 0)
        {
            (void) sprintf( tmpFileList_fullpathFilename, "%s_%s",
                    tmpFileList_fullpathFileRoot, reports_inbound[i].type ) ;
            if ((fp = fopen( tmpFileList_fullpathFilename, "r" )) == NULL &&
                    errno == ENOENT)
            {
                /* free this perm, any errors, ignore for now */
                if (gui_free_permission( permArray[i],
                        reports_inbound[i].mu_activityid,
                        MU_SINGLE_ACTIVITY_TYPE ) > 0)
                {
                    permArray[i] = 0 ;
                    numPerms-- ;
                }
            }
            else if (fp != NULL)
                (void)fclose( fp ) ;
        }
    }

    return ;
}


/*==============================================================================
Function:       getList_subprocess_done

Description:    executed when the get list subprocess finishes.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  11/30/95

Notes:
==============================================================================*/
/* ARGSUSED1 */
static void
getList_subprocess_done( PROCESS_INFO *process, void *dummyarg )
{
    XmStringTable   str_list;
    int             i;

BEGIN_CONTEXT( APSFileProcessing )

    list_not_processed = TRUE;

    XtSetSensitive( pushButton_stop, True ) ;
    /* clear the message window */
    XmTextSetString( scrolledText_procfile, EMPTY_STR );

    fp_active_childPid = 0; /* reset since we're "inactive" */

    /* if stop requested, signal caught, or error occurred in the get subproc */
    if (fp_stop_flag == TRUE
        || SignalCaught
        || process->exit_status != APS_EXIT_OK)
    {
        if (process->exit_status < 0)   /* some signal other than SignalCaught*/
        {
            if (process->exit_status == APSGUI_EXIT_COREDUMP)
            {
                (void) sprintf( display_string,
                        "can't get list of files: subprocess CORE DUMPED" ) ;
                popup_message(XmDIALOG_ERROR, "APS:ERROR",
                        display_string, XtGrabNone);
            }
            else
            {
                (void) sprintf( display_string,
                        "can't get list of files: subprocess caught SIGNAL = %d",
                        -(process->exit_status) ) ;
                popup_message(XmDIALOG_ERROR, "APS:ERROR",
                        display_string, XtGrabNone);
            }
        }
        file_processing_cleanup();  /* if SignalCaught, EXITs */
        return;
    }

    /* free up any unused permissions */
    free_unused_perms() ;

    fp_set_signals( FP_HANDLE_SIG );

    /*
    -- read in the set of files and create the list
    */

    if (create_list_of_files() < 0)
        return;

    /*
    -- display the list (nothing "selected")
    */

    if (num_fileproc_files != 0)
    {
        str_list =
            (XmStringTable) XtMalloc(num_fileproc_files * sizeof (XmString));

        for (i=0, fpFilePtr =
                (FILEPROC_FILES *) FIRST(fileproc_files, filesListPtr)
            ; fpFilePtr != NULL
            ; fpFilePtr = (FILEPROC_FILES *) NEXT(fileproc_files, filesListPtr))
        {
            (void)sprintf( display_string, "%-*s %s",
                MAX_INBOUND_DEFAULT_FNAME_LEN, fpFilePtr->filename,
                fpFilePtr->file_fulldirpath );
            str_list[i++] = XmStringCreateLocalized( display_string );
        }
        XtVaSetValues( scrolledList_reports_inbound,
            XmNitems, str_list,
            XmNitemCount, num_fileproc_files,
            NULL );
        /* free no-longer-needed allocated memory */
        for (i = 0 ; i < num_fileproc_files ; i++)
            XmStringFree( str_list[i] );
        XtFree( (char *) str_list );

        XtSetSensitive( pushButton_process, True ) ;
    }
    else /* nothing to process, reset for next invocation: as if received STOP*/
    {
        list_not_processed = FALSE;
        (void) sprintf( display_string, "No files are ready for processing" );
        popup_message( XmDIALOG_INFORMATION, "APS:INFO",
            display_string, XtGrabNone);
        /* get ready for next time */
        file_processing_cleanup();
        fp_set_signals( FP_RESTORE_SIG );
        return;
    }

END_CONTEXT

    return;
}



/*==============================================================================
Function:       processFile_subprocess

Description:    Controls the subprocesses that do the actual file
                processing.  If gets here because a subprocess has
                just completed, releases the file type's permission
                and removes its tmp file.  If there are more types
                to be processed, starts up the next one.  Or if
                the last type has just finished, restores everything
                to be ready for the next file processing request.

Parameters:     currProcess: (not used) process info for the completed process
                        (NULL if called directly/no completed process)
                currTypeIndex: index of the file type who's processing has
                        just completed (or NO_TYPE, if no completed process)

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/30/95

Notes:
==============================================================================*/
static void
processFile_subprocess( PROCESS_INFO *currProcess, int currTypeIndex )
{
BEGIN_CONTEXT( APSFileProcessing )

    static char     *writeData = NULL ; /* permission data for child's stdin */

    PROCESS_INFO    *newProcess ;

    XtSetSensitive( pushButton_stop, False ) ;

    if (fp_stop_flag == TRUE || SignalCaught)
    {
        file_processing_cleanup();  /* if signal, EXITs */
        return;
    }

    if (currTypeIndex >= 0) /*if not negative, a subprocess has just completed*/
    {
        /* release this permissions & remove the tmp file */
        if (permArray[currTypeIndex] != 0)
        {
            (void) gui_free_permission( permArray[currTypeIndex],
                    reports_inbound[currTypeIndex].mu_activityid,
                    MU_SINGLE_ACTIVITY_TYPE ) ;
            numPerms-- ;
            permArray[currTypeIndex] = 0 ;

            (void) sprintf( tmpFileList_fullpathFilename, "%s_%s",
                    tmpFileList_fullpathFileRoot,
                    reports_inbound[currTypeIndex].type ) ;
            (void) unlink( tmpFileList_fullpathFilename );
        }
    }

    while (++currTypeIndex < num_fileTypes) /* there's another type to do */
    {
        if (permArray[currTypeIndex] != 0)  /* if have the permission */
        {
            /* create the string of "stdin" data for the children */
            if (writeData != NULL)
                free( writeData ) ;
            writeData = create_perm_data_string(
                    reports_inbound[currTypeIndex].type ) ;
            if (writeData == NULL)  /* couldn't form the writeData */
            {
                (void) sprintf( display_string, "%s\n%s",
                        "Can't process any other files",
                        "Can't send the permissions to the subprocesses" ) ;
                popup_message(  XmDIALOG_ERROR, "APS:ERROR",
                        display_string, XtGrabNone ) ;
                /* reset for next invocation: as if had completed file proc. */
                file_processing_cleanup();
                return ;
            }

            (void) sprintf( display_string, "%s -U %s -P %s -t %s %s",
                    DO_LIST_CMD, userid, password,
                    reports_inbound[currTypeIndex].type,
                    tmpFileList_fullpathFileRoot );
            newProcess = create_process( display_string, NULL, TRUE, writeData,
                    gui_display_message_widget, scrolledText_procfile,
                    processFile_subprocess, (void *) currTypeIndex );
            if (newProcess == NULL)
            {
                /* if this one error'ed, assume all the rest will also: return*/
                (void)sprintf( display_string, "%s\n%s",
                        "can't create the file processor",
                        "Too many processes running?" ) ;
                popup_message( XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone);
                /* reset for next invocation: as if had completed file proc. */
                file_processing_cleanup();
                return ;
            }

            if (start_process( newProcess ))
            {
                /*
                -- can't start (can't get pipes), message already popped up;
                -- if this one error'ed, assume all the rest will also. so
                -- restore to beginning of this callback; try again later.
                */
                destroy_process( newProcess );
                file_processing_cleanup();
                return;
            }

            /* subprocess successfully started */
            fp_active_childPid = newProcess->childPid;
            if ( list_not_processed == TRUE )
                list_not_processed = FALSE;
            XtSetSensitive( pushButton_stop, True ) ;
            break ; /* out of while there are files to process */
        }
    }

    if (currTypeIndex >= num_fileTypes) /* finished all the file types */
    {
        fp_processing_done = TRUE;

        if (fp_active_childPid != 0)
        {
            /* put a DONE message in the message area and in a popup window */
            (void) sprintf( display_string,
                "*** DONE *** processing the files" ) ;
            gui_display_message_widget( scrolledText_procfile,
                display_string );
            popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
                display_string, XtGrabNone ) ;
        }

        fp_active_childPid = 0;

        /* clear out displayed list of files */
        XtVaSetValues( scrolledList_reports_inbound,
            XmNitemCount, 0,
            NULL );

        XtSetSensitive( getList_menuBar, True ) ;
    }

END_CONTEXT

    return;
}



/*==============================================================================
Function:       cb_stop_file_processing

Description:    When stop is requested: deactivate the stop pushbutton,
                set the stop flag, and kill any active child process.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  12/02/95

Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_stop_file_processing( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

    XtSetSensitive( pushButton_stop, False ) ;

    if (fp_stop_flag == TRUE)
    {
        /* already requested a stop, waiting for child to die */
        (void) sprintf( display_string,
            "Will stop when done with current file" );
        popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
            display_string, XtGrabNone ) ;
        return;
    }

    if (fp_active_childPid == 0)
    {
        /* not in the middle of child processing, do a stop/cleanup */
        file_processing_cleanup();
        return;
    }

    /* send interrupt signal to any file processing child process */
    (void) kill( fp_active_childPid, SIGINT );

    fp_stop_flag = TRUE;    /* set indicator */

    XtSetSensitive( pushButton_stop, True ) ;

END_CONTEXT

    return;
}



/*==============================================================================
Function:       cb_fill_fileproc_list

Description:    To begin new file processing, gets the set of "unprocessed"
                files.  This is accomplished by a subprocess.  When the
                subprocess dies, the get list done function is called to
                create and display the list of files.

                If the list exists, then we're in the middle of file
                processing and this routine does nothing.

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  11/13/95

Notes:
==============================================================================*/
/* ARGSUSED2 */
void
cb_fill_fileproc_list( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( APSFileProcessing )

    static char     fileRoot[APS_MAX_FILENAME_LENGTH] = EMPTY_STR;
    static char     *writeData = NULL ; /* permission data for child's stdin */

    PROCESS_INFO    *process ;
    char            *inboundType = (char *) client_data;
    int             i ;

    /* initialize the global permission variables */
    if (permArray == NULL)
    {
        for (i = 0 ; reports_inbound[i].type != NULL ; i++)
            ;   /* deliberately empty */
        num_fileTypes = i ;

        if ((permArray = calloc( num_fileTypes, sizeof(int) )) == NULL)
        {
            popup_message(  XmDIALOG_ERROR, "APS:ERROR",
                    "Can't alloc space (for permissions array)", XtGrabNone ) ;
            /* reset for next invocation: as if had completed file processing */
            file_processing_cleanup();
            return ;
        }
    }

    fp_set_signals( FP_RESTORE_SIG );

    XtSetSensitive( getList_menuBar, False ) ;
    XtSetSensitive( pushButton_process, False ) ;   /*disable until have list*/
    XtSetSensitive( pushButton_stop, False ) ;      /*disable until have list*/

    /* initialize global variables only once */

    if (*fileRoot == STREND)
    {
        (void)sprintf( fileRoot, "%s.%d",
            TMP_FILENAME_PREFIX, (int) getpid() );
    }

    if (fp_processing_done == TRUE)
    {
        file_processing_cleanup();  /* just in case wasn't called yet */
        XtSetSensitive( getList_menuBar, False ) ;
        XtSetSensitive( pushButton_process, False ) ;
        XtSetSensitive( pushButton_stop, False ) ;
    }
    else if (fp_active_childPid != 0)   /* if we're currently processing */
        return;

    /*
    -- find and "lock" a new list of files
    */

    if (SignalCaught)
        file_processing_cleanup();  /* EXITs */

    if (tmpFileList_fullpathFileRoot == NULL)
    {
        tmpFileList_fullpathFileRoot = aps_fullpath( APS_TEMP,
            fileRoot );
        tmpFileList_fullpathFilename = malloc(
            strlen( tmpFileList_fullpathFileRoot ) + MAX_RPT_TYPE_LEN + 2 ) ;
        tmpFileList_fullpathFilename[0] = STREND ;
    }

    XmTextSetString( scrolledText_procfile,
        "Getting list of files to process, *** PLEASE WAIT ***" );

    /* get and "lock" the new fp files */
    if (strcmp( inboundType, INBOUND_ALL_TYPES ) == 0)
    {
        /* get permissions for ALL files */
        for (i = 0 ; reports_inbound[i].type != NULL ; i++)
        {
            if ((permArray[i] = gui_get_single_permission(
                    reports_inbound[i].mu_activityid )) <= 0)
            {
                /* couldn't get permission, msg already done, go to next type */
                permArray[i] = 0 ;
                continue ;
            }

            numPerms++ ;
        }

        /* form the get command for ALL files */
        (void) sprintf( display_string, "%s %s",
            GET_LIST_CMD, tmpFileList_fullpathFileRoot );

        /* create the string of "stdin" data for the child */
        if (writeData != NULL)
            free( writeData ) ;
        if ((writeData = create_perm_data_string( NULL )) == NULL)
        {
            (void) sprintf( display_string, "%s\nOR\n%s",
                "No permissions retrieved",
                "Can't send them to the subprocess" ) ;
            popup_message(  XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone ) ;
            /* reset for next invocation: as if had completed file proc. */
            file_processing_cleanup();
            return ;
        }
    }
    else
    {
        /* get permissions for this file type */
        for (i = 0 ; reports_inbound[i].type != NULL ; i++)
        {
            /* if this entry is not the one for this file type, skip it */
            if (strcmp( inboundType, reports_inbound[i].type ) != 0)
                continue ;

            if ((permArray[i] = gui_get_single_permission(
                    reports_inbound[i].mu_activityid )) <= 0)
            {
                /* error popup already done in subroutine */
                permArray[i] = 0 ;
                /* reset for next invocation: as if had completed file proc. */
                file_processing_cleanup();
                return ;
            }

            numPerms = 1 ;
            break ;
        }

        /* form the get command for this file type */
        (void) sprintf( display_string, "%s -t %s %s",
            GET_LIST_CMD, inboundType, tmpFileList_fullpathFileRoot );

        /* create the string of "stdin" data for the child */
        if (writeData != NULL)
            free( writeData ) ;
        if ((writeData = create_perm_data_string( inboundType )) == NULL)
        {
            (void) sprintf( display_string,
                "Can't send the permission to the subprocess" ) ;
            popup_message(  XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone ) ;
            /* reset for next invocation: as if had completed file proc. */
            file_processing_cleanup();
            return ;
        }
    }

    process = create_process( display_string, NULL, FALSE, writeData,
        NULL, NULL, getList_subprocess_done, NULL );
    if (process == NULL)
    {
        (void)sprintf( display_string,
            "can't get list of files\n Too many processes running?" ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
        /* reset for next invocation: as if had completed file processing */
        file_processing_cleanup();
        return ;
    }

    if (start_process( process ))
    {
        /*
        -- can't get pipes, error message already popped up;
        -- reset for next invocation: as if received STOP
        */
        destroy_process( process );
        file_processing_cleanup();
        return;
    }
    fp_active_childPid = process->childPid;

END_CONTEXT

    return;
}



/*==============================================================================
Function:       cb_process_all_files

Description:    callback for starting to process the set of files

Parameters:

Returns:

Creator:        Teresa McKillop

Creation Date:  11/13/95

Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_process_all_files( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

    XtSetSensitive( pushButton_stop, False ) ;

    XtSetSensitive( pushButton_process, False ) ;   /*disable activating again*/

    if (num_fileproc_files == 0)    /* nothing to process */
    {
        (void) sprintf( display_string, "No files are ready for processing" );
        popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
            display_string, XtGrabNone ) ;
        /* reset for next invocation: as if received STOP */
        file_processing_cleanup();
        return;
    }

    /*
    -- for all the files ready to be processed, start the processor for
    -- each type of file (processor stops at end of a file if a signal
    -- is caught)
    */

    processFile_subprocess( (PROCESS_INFO *) NULL, NO_TYPE ) ;

END_CONTEXT

    return;
}



/*==============================================================================
Function:               cb_quit_file_processing
 
Description:    
        This function pops down the File Processing interface.  If permissions 
		have not been released, the user is queried as to whether or not he/she
        wants to continue.
 
Parameters:             Standard X Callback parameters
 
Returns:                None
 
Creator:                Philip Yurchuk
 
Creation Date:  3/2/97
 
Notes:          
==============================================================================*/
/* ARGSUSED1 */
void 
cb_quit_file_processing(Widget widget, XtPointer client_data, XtPointer cbs)
{

	extern Widget apsfileproc_form ;

BEGIN_CONTEXT ( widget )
 
  	if (numPerms > 0)
	{
		/* 
		-- If there are outstanding permissions, query the user to see if he or 
		-- she wishes to continue.
		*/

	  	(void) sprintf( question, "Pressing the QUIT button merely dismisses \nthe window, and does not release permissions, \nwhich still exist.  \nDo you wish to continue?" ) ;

		if (AskUser(widget, question, NO) == YES)
		{
			XtPopdown(XtParent(apsfileproc_form)) ;
		}
	}
	else
		XtPopdown(XtParent(apsfileproc_form)) ;

END_CONTEXT
 
}

