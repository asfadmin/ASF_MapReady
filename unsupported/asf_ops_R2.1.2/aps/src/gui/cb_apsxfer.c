#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_apsxfer.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_apsxfer.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apsxfer.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <errno.h>
 
#include <sys/types.h>
#include <sys/file.h>

#include "UxXt.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_exe_names.h"
#include "apsfiledef.h"
#include "apspath.h"
#include "aps_extern.h"
#include "gui_utils.h"
#include "subprocess.h"

#include "cb_apsxfer.h"
#include "vc_crtapsfile.h"

#include "xlate.h"
#include <odldef.h>

extern void	popup_message() ;

extern char display_string[] ;

#define FA_AND_IMS				1	
#define IMS_ONLY				2
#define FA_ONLY					3	
#define CMD_STRING_LEN			512	


/*==============================================================================
Function:		aps_getPMFKeyWordValue

Description:	This routine extracts and returns the value of a specified
				keyword from the pmf file.

Parameters:		char* keyword
				char* targetAggregate: The name of the object that contains 
										the target keyword.
				char* pmfFileName: The name of the pmf file name.

Returns:     	(char* ) the value of the specified keyword

Creator:		Q. Sun	

Creation Date:	02/02/96

Notes:		
==============================================================================*/
char *
aps_getPMFKeyWordValue(	char* keyword, 
						char* targetAggregate, 
						char* pmfFileName)
{
	char *keyword_value = NULL;
	FILE *fd = NULL;
	AGGREGATE rootAggregate = NULL;
	AGGREGATE currAggregate = NULL;
	PARAMETER currParameter = NULL;
	VALUE currValue = NULL;
	unsigned int milliseconds = 0;

	/*
	** Open the Metadata file for read.
	*/
	if ((fd = fopen (pmfFileName, "r")) == (FILE *) NULL)
	{
		return(NULL);
	}

	/*
	** Allocate the structure for the root node of the ODL tree.
	*/
	if ((rootAggregate = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
															(AGGREGATE) NULL)
	{
		(void) fclose(fd);
		return(NULL);
	}

	/*
	** Call the ODL function to read and parse the Metadata file
	** into the ODL tree.
	*/
    if ((ReadLabel (fd, rootAggregate)) == 0)
	{
		RemoveAggregate(rootAggregate); 
		(void) fclose(fd);
		return(NULL);
	}
	if ((currAggregate = FindAggregate(rootAggregate, targetAggregate)) 
														== (AGGREGATE) NULL)
	{
		RemoveAggregate(rootAggregate); 
		(void) fclose(fd);
		return(NULL);
	}
	
	if ((currParameter = FindParameter(currAggregate, keyword)) == NULL)
	{
		RemoveAggregate(currAggregate); 
		RemoveAggregate(rootAggregate); 
		(void) fclose(fd);
		return(NULL);
	}

	if ((currValue = FirstValue (currParameter)) == NULL)
	{
		RemoveParameter(currParameter); 
		RemoveAggregate(currAggregate); 
		RemoveAggregate(rootAggregate); 
		(void) fclose(fd);
		return(NULL);
	}

	switch (currValue->item.type)
	{
		/*
		case TV_INTEGER:
			(void) printf ("keyword_value = %d\n",
					currValue->item.value.integer.number);
			break;
		*/
		case TV_SYMBOL:
			if ((keyword_value = 
						malloc(strlen(currValue->item.value.string)+1)) == NULL)
				return (NULL);
			else
			{
				(void) sprintf(keyword_value, "%s",
						currValue->item.value.string);
			}
			break;
		case TV_STRING:
			if ((keyword_value = 
						malloc(strlen(currValue->item.value.string)+1)) == NULL)
				return (NULL);
			else
			{
				(void) sprintf(keyword_value, "%s",
						currValue->item.value.string);
			}
			break;
		case TV_DATE_TIME:
			milliseconds = (unsigned int)
					(currValue->item.value.date_time.nanoseconds / 1000000);
			if ((keyword_value = malloc (ASF_TIME_STR_LENGTH+1)) == NULL)
				return (NULL);
			else
			{
				(void) sprintf(keyword_value, "%04u-%03uT%02u:%02u:%02u.%03u",
					currValue->item.value.date_time.year,
					currValue->item.value.date_time.doy,
					currValue->item.value.date_time.hours,
					currValue->item.value.date_time.minutes,
					currValue->item.value.date_time.seconds,
					milliseconds);
			}
			break;
		default:
			RemoveValue(currValue); 
			RemoveParameter(currParameter); 
			RemoveAggregate(currAggregate); 
			RemoveAggregate(rootAggregate); 
			(void) fclose(fd);
			return(NULL);
	}
	/*
	** Free the entire ODL tree and close the file.
	*/
	RemoveValue(currValue); 
	RemoveParameter(currParameter); 
	RemoveAggregate(currAggregate); 
	RemoveAggregate(rootAggregate); 
	(void) fclose(fd);

	return (keyword_value);
}

/*==============================================================================
Function:		transfer_done	

Description:	This function is invoked when the file transfer to the 
				external agency is completed.

Parameters:		

Returns:     	

Creator:		Norbert Piega	

Creation Date:	mm/dd/yyyy

Notes:		
==============================================================================*/
/* ARGSUSED1 */
static void
transfer_done(PROCESS_INFO *process,  REPORT *aps_file) 
{
	switch (process->exit_status)
	{
	case APS_EXIT_OK :
		(void) unlink(process->target_filename);
		(void) sprintf(display_string, 
            "APS File Transfer:\n\n"
            "External agency file transfer completed successfully.\n\n"
            "The following file was transferred:\n"
            "%s\n", process->original_filename);
		popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
			XtGrabNone);
		break ;
	case APS_EXIT_ERROR :
		(void) sprintf(display_string, 
            "APS File Transfer:\n\n"
            "External agency file transfer failed!\n\n"
            "The following file was NOT transferred:\n"
            "%s\n\n"
            "The following was the intermediate file created:\n"
            "%s\n\n",
            process->original_filename,
            process->target_filename);
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		break ;
	case APSGUI_EXIT_COREDUMP : /* process core dumped */
		(void) sprintf(display_string, 
            "APS File Transfer:\n\n"
            "External agency file transfer, Signal Caught CORE DUMPED!\n\n"
            "The following file was NOT transferred:\n"
            "%s\n\n"
            "The following was the intermediate file created:\n"
            "%s\n\n",
            process->original_filename,
            process->target_filename);
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		break ;
	default :	/* process caught a signal, but no core dump */
		(void) sprintf(display_string, 
            "APS File Transfer:\n\n"
            "External agency file transfer, caught SIGNAL (signal = %d)!\n\n"
            "The following file was NOT transferred:\n"
            "%s\n\n"
            "The following was the intermediate file created:\n"
            "%s\n\n",
			-(process->exit_status),
            process->original_filename,
            process->target_filename);
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		break ;
	}
}	


/*==============================================================================
Function:		archive_done	

Description:	This function is invoked when the IMS/DADS file archiving is
				completed.

Parameters:		

Returns:     	none

Creator:		Q. Sun

Creation Date:	02/02/96

Notes:		
==============================================================================*/
/* ARGSUSED1 */
static void
archive_done(PROCESS_INFO *process, REPORT *aps_file) 
{
	char *fname = NULL;


	switch (process->exit_status)
	{
	case APS_EXIT_OK :
		fname = malloc(strlen(process->target_filename) 
						+ strlen(PMF_EXTENSION) + 2);
		if (fname != NULL)	
		{
			(void) sprintf(fname, "%s.%s",
					process->target_filename, PMF_EXTENSION);
			(void) unlink(fname);
			free(fname);
			fname = NULL;
		}
	
		fname = malloc(strlen(process->target_filename) 
						+ strlen(DATAFILE_EXTENSION) + 2);
		if (fname != NULL)
		{	
			(void) sprintf(fname, "%s.%s", process->target_filename, 
									DATAFILE_EXTENSION);
			(void) unlink(fname);
			free(fname);
		}

		(void) sprintf(display_string, 
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving completed successfully.\n\n"
            "The following files were archived:\n"
			"Data File name:      %s\n"
			"Meta Data file name: %s.%s\n",
			process->original_filename,
			process->original_filename, PMF_EXTENSION);
		popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
			XtGrabNone);

		break ;
	case APS_EXIT_ERROR :
		(void) sprintf(display_string, 
			"APS File Transfer:\n\n"
			"IMS/DADS file archiving failed!\n\n"
            "The following files were NOT archived:\n"
			"Data File name:      %s\n"
			"Meta Data file name: %s.%s\n\n"
            "The following were the intermediate files created:\n"
			"Data File name:      %s.%s\n"
			"Meta Data file name: %s.%s\n\n",
			process->original_filename,
			process->original_filename, PMF_EXTENSION,
            process->target_filename, DATAFILE_EXTENSION,
            process->target_filename, PMF_EXTENSION);

		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

		break ;
	case APSGUI_EXIT_COREDUMP : /* process core dumped */
		(void) sprintf(display_string, 
			"APS File Transfer:\n\n"
			"IMS/DADS file archiving Signal Caught CORE DUMPED!\n\n"
            "The following files were NOT archived:\n"
			"Data File name:      %s\n"
			"Meta Data file name: %s.%s\n\n"
            "The following were the intermediate files created:\n"
			"Data File name:      %s.%s\n"
			"Meta Data file name: %s.%s\n\n",
			process->original_filename,
			process->original_filename, PMF_EXTENSION,
            process->target_filename, DATAFILE_EXTENSION,
            process->target_filename, PMF_EXTENSION);

		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

		break ;
	default :	/* process caught a signal, but no core dump */
		(void) sprintf(display_string, 
			"APS File Transfer:\n\n"
			"IMS/DADS file archiving caught a SIGNAL (signal = %d)!\n\n"
            "The following files were NOT archived:\n"
			"Data File name:      %s\n"
			"Meta Data file name: %s.%s\n\n"
            "The following were the intermediate files created:\n"
			"Data File name:      %s.%s\n"
			"Meta Data file name: %s.%s\n\n",
			-(process->exit_status),
			process->original_filename,
			process->original_filename, PMF_EXTENSION,
            process->target_filename, DATAFILE_EXTENSION,
            process->target_filename, PMF_EXTENSION);

		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

		break ;
	}
}	



/*==============================================================================
Function:		ask_to_send_file

Description:	Ask to confirm send file to both external agency and IMS/DADS

Parameters:		Widget (widget activating function)
            	filename - filename to send
            	aps_file - file description

Returns:     	YES/NO

Creator:		Ron Green

Creation Date:	04/21/1995

Notes:		
==============================================================================*/
static int
ask_to_send_file(widget, filename, aps_file)
	Widget widget ;
	char *filename ;
	REPORT *aps_file ;
{
	(void) sprintf(question, 
		"Transfer file to both external agency and IMS/DADS?\n\n"
		"File Type: %s\n"
		"File Name: %s\n",
		aps_file->type, filename) ;

	return (AskUser(XtParent(widget), question, NO)) ;
}


/*
==============================================================================
Function:		ask_to_archive_file

Description:	Ask to confirm send file to IMS/DADS

Parameters:		Widget (widget activating function)
            	filename - filename to archive 
            	aps_file - file description

Returns:     	YES/NO

Creator:		Q. Sun

Creation Date:	02/01/1996

Notes:		
==============================================================================*/
static int
ask_to_archive_file(Widget widget, char* filename, REPORT* aps_file)
{
	(void) sprintf(question, 
		"Transfer file to IMS/DADS only?\n\n"
		"File Type: %s\n"
		"File Name: %s\n",
		aps_file->type, filename) ;

	return (AskUser(XtParent(widget), question, NO)) ;
}


/*==============================================================================
Function:		create_ims_filename()

Description:	Create the temporary file name to be used for IMS archiving. 
            
Parameters:		

Returns:     	

Creator:		

Creation Date:	02/02/1996

Notes:		
==============================================================================*/
static char *
create_ims_filename(REPORT *aps_file, char *pmf_filename)
{
	char *file_creation_time = NULL;
	char *tmpFileName = NULL;
	char *fullpath_filename = NULL;

	if ((file_creation_time = aps_getPMFKeyWordValue(
									PMF_CREATION_TIME_KEYWORD,
									PMF_AGGREGATE_NAME, 
									pmf_filename)) == NULL)
 	{
		(void) sprintf(display_string, "APS File Transfer:\n\n"
				"Can't retrieve FILE_CREATION_TIME from the PMF file %s\n", 
				pmf_filename) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
			display_string, XtGrabNone) ;
		return(NULL);
	}

	/* 
	** Create the IMS/DADS archive file name
	** to be used in calling ims_archive().
	*/
	if ((tmpFileName = malloc(	strlen(aps_file->type) + 
								strlen(file_creation_time) + 2)) == NULL) 
		return(NULL); 
	else
	{
		(void) sprintf(tmpFileName, "%s_%s",
				aps_file->type, file_creation_time);
	}

	fullpath_filename = aps_fullpath(APS_TEMP, tmpFileName);

	free(file_creation_time); 
	free(tmpFileName);

	return(fullpath_filename);
}


/*==============================================================================
Function:		xlate_wos_to_wff_format()

Description:	Translate MWOS and AREQ files to the Walops format.
            
Parameters:		

Returns:     	

Creator:		

Creation Date:	02/02/1996

Notes:		
==============================================================================*/
int
xlate_wos_to_wff_format(char* infile, char* outfile)
{
	char *xlateTableName = NULL;
	int tmpi ;

	xlateTableName = getenv( APS_WOS_XLATE_TBL_ENVVAR ) ;
	if (xlateTableName)
	{
		(void) gui_trimstring( xlateTableName ) ;
		tmpi = strlen( xlateTableName ) ;
	}

	if (!xlateTableName || tmpi == 0)
	{
		(void) sprintf( display_string, 
						"APS File Translation:\n\n"
						"Environment Variable '%s' not set!",
						APS_WOS_XLATE_TBL_ENVVAR ) ;
		popup_message( XmDIALOG_ERROR, "APS:ENVIRONMENT ERROR", 
						display_string, XtGrabNone) ;
	}

	/*
	-- Note: NULL xlateTableName error  
	-- is caught in xlate() calling read_table()
	*/
	return( xlate(infile, outfile, xlateTableName) );
}


/*==============================================================================
Function:		xlate_sv_to_wff_format()

Description:	Translate predicted state vector file to the Walops format.
            
Parameters:		

Returns:     	

Creator:		

Creation Date:	02/02/1996

Notes:		
==============================================================================*/
int
xlate_sv_to_wff_format(char* infile, char* outfile)
{
    char *xlateTableName = NULL;
    int tmpi ;
 
    xlateTableName = getenv( APS_EPHEM_XLATE_TBL_ENVVAR ) ;
    if (xlateTableName)
    {
        (void) gui_trimstring( xlateTableName ) ;
        tmpi = strlen( xlateTableName ) ;
    }
 
    if (!xlateTableName || tmpi == 0)
    {
        (void) sprintf( display_string,
						"APS File Translation:\n\n"
						"Environment Variable '%s' not set!",
                        APS_EPHEM_XLATE_TBL_ENVVAR ) ;
        popup_message( XmDIALOG_ERROR, "APS:ENVIRONMENT ERROR",
                        display_string, XtGrabNone) ;
    }
 
    /*
    -- Note: NULL xlateTableName error  
    -- is caught in xlate() calling read_table()
    */
    return( xlate(infile, outfile, xlateTableName) );
}


/*==============================================================================
Function:		dce_xmit_file

Description:	Transfer a file to the external agency via FAIF using
				a DCE client program. 

Parameters:		

Returns:     	none

Creator:		Q. Sun

Creation Date:	02/05/1996

Notes:		
==============================================================================*/
static void
dce_xmit_file(Widget widget, XtPointer client_data, char* cmdString)
{
	int status ;
	REPORT   *aps_file ;
	PROCESS_INFO *process = NULL;
	int destination;
	char *original_filename = NULL;
	char *pmf_filename = NULL;
	char *destination_filename = NULL;
	char *ims_archive_filename = NULL;
	char *ims_data_filename = NULL;
	char *ims_metadata_filename = NULL;

	/*
	-- get the active file information we are to transmit 
	-- it is contained in the client data
	*/
	aps_file = (REPORT *) client_data ;

	/*
	-- get/create the local filename for the aps file/report
	-- a function must be specified for creating the local
	-- filename the format of the the functiion is 
	--    char *mk_local_filename()
	*/
	if (aps_file->mk_local_filename)
		original_filename = (*(aps_file->mk_local_filename))() ;
	else
	{
		(void) sprintf(display_string, 
			"APS File Transfer:\n\n"
			"INTERNAL ERROR\nMissing function to get/create local filename") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		return ;
	}

	/*
	-- Note the following check is set for send only (we are making
	-- sure the file exists to send). 
	-- 
	-- if we are ever to receive, make sure we're not overwriting an 
	-- existing file
	*/

	if (access(original_filename, F_OK) != 0)
	{
		(void) sprintf(display_string,
			"APS File Transfer:\n\n"
			"File Type: %s\nFile Name: %s\n\nDoes not exist!\n", 
			aps_file->type, original_filename) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
			display_string, XtGrabNone) ;
		free(original_filename) ;
		return ;
	}

	/* Derived the pmf file name from the report file name */
	pmf_filename = (char *) malloc (strlen(original_filename) 
									+ strlen(PMF_EXTENSION) + 2);
	(void) sprintf(pmf_filename, "%s.%s", original_filename, PMF_EXTENSION);
	if (access(pmf_filename, F_OK) != 0)
	{
		(void) sprintf(display_string,
			"APS File Transfer:\n\n"
			"File Type: %s PMF\nFile Name: %s\n\nDoes not exist!\n", 
			aps_file->type, pmf_filename) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
			display_string, XtGrabNone) ;
		free(original_filename) ;
		free(pmf_filename) ;
		return ;
	}

	/*
	** Prompt user for destination. If a file is sent to an external 
	** agency, it is also automatically sent to IMS/DADS. 
	*/
	destination = FA_AND_IMS;
	if (ask_to_send_file(widget, original_filename, aps_file) == NO)
	{
		if (ask_to_archive_file(widget, original_filename, aps_file) == NO)
		{
			free(original_filename) ;
			free(pmf_filename) ;
			return ;
		}
		else
			destination = IMS_ONLY ;
	}

	/* 
	** Create the IMS/DADS file name used to invoke the ims_archive() call.
	** If the file name is XXXX, the ims_archive() will look for the  
	** XXXX.D (for actual data) and XXXX.M (for meta data) files. 
	*/
	if ((ims_archive_filename = create_ims_filename(
									aps_file, pmf_filename)) == NULL)
	{
		(void) sprintf(display_string, "APS File Transfer:\n\n"
				"Can't create temporary file names for IMS/DADS archiving\n");
		popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
			display_string, XtGrabNone) ;
		free(original_filename) ;
		free(pmf_filename) ;
		return ;
	}
	ims_metadata_filename = (char *) malloc(strlen(ims_archive_filename) 
											+ strlen(PMF_EXTENSION) + 2);
	ims_data_filename = (char *) malloc(strlen(ims_archive_filename) 
											+ strlen(DATAFILE_EXTENSION) + 2);
	(void) sprintf(ims_data_filename, "%s.%s", 
			ims_archive_filename, DATAFILE_EXTENSION);
	(void) sprintf(ims_metadata_filename, "%s.%s", 
			ims_archive_filename, PMF_EXTENSION);

#ifdef PRINTDIAG
	(void) printf("ims_archive_filename  = %s\n", ims_archive_filename);
	(void) printf("ims_data_filename     = %s\n", ims_data_filename);
	(void) printf("ims_metadata_filename = %s\n", ims_metadata_filename);
#endif

	/* Create the IMS/DADS destination file */
	(void) unlink(ims_data_filename);
	if (link(original_filename, ims_data_filename) == ERROR)
	{
			(void) sprintf(display_string, "APS File Transfer:\n\n"
				"Can't create a temporary file for IMS/DADS archiving!\n"
				"File name: %s\n",
				ims_data_filename);
			popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
						  display_string, XtGrabNone) ;
			free(original_filename) ;
			free(pmf_filename) ;
			free(ims_data_filename) ;
			free(ims_metadata_filename) ;
			free(ims_archive_filename) ;
			return;
	}

	(void) unlink(ims_metadata_filename);
	if (link(pmf_filename, ims_metadata_filename) == ERROR)
	{
			(void) sprintf(display_string, "APS File Transfer:\n\n"
				"Can't create a temporary pmf file for IMS/DADS archiving!\n"
				"File name: %s\n",
				ims_metadata_filename);
			popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
				display_string, XtGrabNone) ;

			(void) unlink(ims_data_filename);

			free(original_filename) ;
			free(pmf_filename) ;
			free(ims_data_filename) ;
			free(ims_metadata_filename) ;
			free(ims_archive_filename) ;
			return;
	}

	/* 
	-- Create the destination filename for the external agency. 
	-- Use mk_remote_filename function in the aps_file
	-- definition to get the filename
	-- else if no function exists for creating a
	-- destination filename use the original APS report filename
	--
	*/
	if (destination == FA_AND_IMS)
	{
		if (aps_file->mk_remote_filename)
		{
			destination_filename = 
						(*(aps_file->mk_remote_filename))(pmf_filename) ;
		}
		else
			destination_filename = aps_fullpath(APS_TEMP, 
										(char *)basename(original_filename));

		if (destination_filename == NULL)
		{
        	(void) sprintf(display_string, "APS File Transfer:\n\n"
				"Can't create remote file name\n");
			popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
							display_string, XtGrabNone) ;

			(void) unlink(ims_data_filename);
			(void) unlink(ims_metadata_filename);

			free(original_filename) ;
			free(pmf_filename) ;
			free(ims_data_filename) ;
			free(ims_metadata_filename) ;
			free(ims_archive_filename) ;
			return;
		}

		/* now translate the file if necessary */
		if (aps_file->xlate_func)
		{
			status = (*(aps_file->xlate_func))(	original_filename, 
												destination_filename);

			/*
			** The translation function returns zero on success.
			*/
			if (status != OK)
			{
				(void) sprintf(display_string, 
						"APS File Transfer:\n\n"
						"Unable to translate file:\n%s\n\n", original_filename);

				switch (status)
				{
					case E_SAMEFILE:
						(void) sprintf(display_string+strlen(display_string),
								"Input and Output files must be different!\n");
						break;
					case E_INFILE:
						(void) sprintf(display_string+strlen(display_string),
								"Can't open the input file:\n%s\n", 
								original_filename);
						break;
					case E_OUTFILE:
						(void) sprintf(display_string+strlen(display_string),
								"Can't open the output file:\n%s\n",
								destination_filename);
						break;
					case E_TBLFILE:
						(void) sprintf(display_string+strlen(display_string),
								"Can't open the translation table!\n");
						break;
					case E_TBLFMT:
						(void) sprintf(display_string+strlen(display_string),
								"No entry found in the translation table!\n");
						break;
					default:
						(void) sprintf(display_string+strlen(display_string),
								"Unknown file translation return code: %d\n",
								status);
						break;
				}

				popup_message(XmDIALOG_ERROR, "APS:ERROR", 
								display_string, XtGrabNone) ;

				(void) unlink(ims_data_filename);
				(void) unlink(ims_metadata_filename);
				(void) unlink(destination_filename);

				free(original_filename) ;
				free(pmf_filename) ;
				free(destination_filename) ;
				free(ims_data_filename) ;
				free(ims_metadata_filename) ;
				free(ims_archive_filename) ;

				return ;
			}
		}
		else
		{	
			/* 
			** Make a copy of the report file to be transfered to the
			** external agency. 
			*/
			(void) unlink(destination_filename);
			if (link(original_filename, destination_filename) == ERROR)
			{
					(void) sprintf(display_string, "APS File Transfer:\n\n"
						"Can't make a destination file to transfer to external agency\n%s\n",
						destination_filename);
					popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer", 
						display_string, XtGrabNone) ;

					(void) unlink(ims_data_filename);
					(void) unlink(ims_metadata_filename);

					free(original_filename) ;
					free(pmf_filename) ;
					free(destination_filename) ;
					free(ims_data_filename) ;
					free(ims_metadata_filename) ;
					free(ims_archive_filename) ;

					return ;
			}
		}

		/* transmit the file to the external agency */
		(void) sprintf(display_string, "%s -f %s",	
				cmdString,
				destination_filename);

		process = (PROCESS_INFO *) create_process(display_string, &status, TRUE,
			NULL, gui_display_message_widget, scrolledText_create_report,
			transfer_done, aps_file) ;
		if (process != NULL)
		{
			(void) sprintf(process->target_filename, "%s",
					destination_filename);
			(void) sprintf(process->original_filename, "%s", original_filename);
			if (start_process(process) > OK)
				destroy_process(process);
		}
	} /* if (destination == FA_AND_IMS) */

	process = NULL;
	/* transmit the file to the IMS/DADS */
	(void) sprintf(display_string, "%s -f %s -d \"%s\" -U %s -P %s",	
							IMS_ARCHIVE_CMD,
							ims_archive_filename, 
							aps_file->metacode, 
							userid, password); 

	process = (PROCESS_INFO *) create_process(display_string, &status, TRUE,
			NULL, gui_display_message_widget, scrolledText_create_report, 
			archive_done, aps_file) ;
	if (process != NULL)
	{
		(void) sprintf(process->target_filename, "%s", ims_archive_filename);
		(void) sprintf(process->original_filename, "%s", original_filename);
		if (start_process(process) > OK)
			destroy_process(process);
	}

	/* 
	** free the created filenames 
	*/
	free(original_filename) ;
	free(pmf_filename) ;
	free(destination_filename) ;
	free(ims_data_filename) ;
	free(ims_metadata_filename) ;
	free(ims_archive_filename) ;
}


/*==============================================================================
Function:		cb_faif_filexfer

Description:	Transfer a file to the external agency via FAIF using
				a DCE client program. 

Parameters:		Standard X Parameters

Returns:     	none

Creator:		Q. Sun

Creation Date:	02/05/1996

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_faif_filexfer(	Widget widget, 
						XtPointer client_data, 
						XtPointer call_data)
{
	char cmdString[CMD_STRING_LEN]; 
	REPORT *aps_file = (REPORT *) client_data;

	(void) sprintf(cmdString, "%s -d %s -t %s",	
						FAIF_XMITCLIENT_CMD,
						aps_file->fa_destination, 
						aps_file->fa_filetype) ;
	dce_xmit_file(widget, client_data, cmdString);
}


/*==============================================================================
Function:		cb_hc_filexfer

Description:	Transfer a file to the Host Controller (HC) using
				a DCE client program. 

Parameters:		Standard X Parameters

Returns:     	none

Creator:		Q. Sun

Creation Date:	02/05/1996

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_hc_filexfer(Widget widget, 
					XtPointer client_data, 
					XtPointer call_data)
{
	char cmdString[CMD_STRING_LEN]; 
	REPORT *aps_file = (REPORT *) client_data;

	(void) sprintf(cmdString, "%s -t %s",	
						HC_XMITCLIENT_CMD,
						aps_file->type) ;
	dce_xmit_file(widget, client_data, cmdString);
}


