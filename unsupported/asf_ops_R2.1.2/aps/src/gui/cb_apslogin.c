#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_apslogin.c

Description:	Contains the callback functions for the APS login window

External Functions Defined:
				cb_init_userid()	- sets the initial login user id value
				cb_init_password()	- sets the initial password value
				cb_init_database()	- sets the initial database value

				cb_save_password()	- save the login password modification

				cb_map_login()		- finish login window after it's created
				cb_reset_login()	- reset values to the saved values

				cb_verify_login()	- check for a valid login
				cb_exit_login()		- exit from login (does not bring up aps)
	
File Scope Functions:
				get_database()		- get the login database value
				get_userid()		- get the login user id value
				save_login()		- save values for "reset"
				set_databaseEnv()	- set the database env var
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_apslogin.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apslogin.c"

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include "UxXt.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_extern.h"		/* extern: userid, password */
#include "gui_utils.h"
#define	CONTEXT_MACRO_ACCESS	1
#include "vc_apslogin.h"
#undef  CONTEXT_MACRO_ACCESS
#include "cb_apslogin.h"

#define BEGIN_CONTEXT(widget) \
	_UxCAPSLoginForm	*UxSaveCtx ; \
	UxSaveCtx = UxAPSLoginFormContext; \
	UxAPSLoginFormContext = (_UxCAPSLoginForm *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
	} \
	UxAPSLoginFormContext = UxSaveCtx ;

/*==============================================================================
	Function Declarations
==============================================================================*/
extern swidget	create_APSMainMenu( swidget _UxUxParent );
extern void		popup_message();

static void		save_login();
static void		set_databaseEnv();

/*==============================================================================
	Global Variable Declarations
==============================================================================*/
extern Widget	mainIface;
extern swidget	APSMainMenu;
extern char		display_string[];

static char		*databaseValue = NULL;		/* value part of database env var */
static Boolean	isSetPassword;				/* used in cb_save_password() */
/* this next is a part of the environment:
-- see putenv() before changing it or freeing it
 */
static char		*envDatabase = NULL;			/* database env var storage */
static char		*lastApplied_userid   = NULL;	/* save value for "reset" */
static char		*lastApplied_password = NULL;	/* save value for "reset" */
static char		*lastApplied_database = NULL;	/* save value for "reset" */

/*==============================================================================
Function:		cb_init_userid

Description:	initialize the userid "login" when the field is created

Parameters:		widget	-	the userid "login" widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Mon Jul 10 12:44:26 PDT 1995

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_init_userid(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	struct passwd		dummyStructPasswd;
	struct passwd		*ret_StructPasswd;
	char				tmpUserName[MAX_USERNAME_LEN];
	char				*tmpUserNameEnv;

	/*
	-- if no command line/resource userid: get the sybase userid,
	-- else get the effective userid
	*/
	if (!userid)
	{
		/* get the sybase userid env variable, if any */
		if ((tmpUserNameEnv = getenv( APS_USERID_ENVVAR )))
		{
			userid = XtNewString( tmpUserNameEnv );
		}
		else	/* get the effective user's name */
		{
			ret_StructPasswd = getpwuid_r( geteuid(), &dummyStructPasswd,
					tmpUserName, MAX_USERNAME_LEN );
			if (ret_StructPasswd)
			{
				userid = XtNewString( tmpUserName );
			}
		}
	}

	/* set the userid on the window */
	if (userid)
		XmTextFieldSetString( widget, userid );

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_init_password

Description:	initialize the password when the field is created

Parameters:		widget	-	the password widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Tue Jul 11 09:45:43 PDT 1995

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_init_password(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	isSetPassword = True;

	/* put the command line password to the window */
	if (password)
	{
		XmTextFieldSetString( widget, password );
	}

	isSetPassword = False;

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_init_database

Description:	sets the initial database variable when the field is created

Parameters:		widget	-	the database widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Mon Jul 10 12:44:40 PDT 1995

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_init_database(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	char				*tmpDatabase = NULL;

	/* get the env variable */
	tmpDatabase = getenv( APSDB_ENVVAR );

	/* if need a database and there is an environment var */
	if (!databaseValue && tmpDatabase)
	{
		databaseValue = XtNewString( tmpDatabase );
	}

	/* set the database in the env & on the window */
	if (databaseValue)
	{
		set_databaseEnv( tmpDatabase, &databaseValue );

		XmTextFieldSetString( widget, databaseValue );
	}

END_CONTEXT

	return;
}


/*==============================================================================
Function:		get_userid()

Description:	gets the string from the userid "login" field when the field
				is activated.

Parameters:		widget	-	the userid "login" widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Mon Jul 10 15:15:35 PDT 1995

Notes:			Empty-string userid is the same as no userid
==============================================================================*/
/* ARGSUSED1 */
static void
get_userid(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	char				*tmpUserName;

	if (strlen( tmpUserName = XmTextGetString( widget ) ))
	{
		if (userid)		XtFree( userid );	/* still points to freed mem */
		userid = XtNewString( tmpUserName );	/* save the value for aps */
	}
	else
	{
		if (userid)	
		{
			XtFree( userid );
			userid = NULL;
		}
	}

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_save_password

Description:	Gets and saves each modification to the password field; echo
				prints an asterisk in place of each character in the field.
				if isSetPassword is True, replaces the password on the window
				with "password"

Parameters:		widget	-	the password widget
				cbs		-	the text verification callback structure

Returns:		

Creator:		Teresa McKillop

Creation Date:	Thu Jul  6 17:11:29 PDT 1995

Notes:			Allows one letter at a time to be appended; disallows
				"pasting" of multiple letters (except when initializing).

				Allows erasures, but from the end of the password towards the
				beginning; "cuts" in the middle are not allowed.
==============================================================================*/
void
cb_save_password(
	Widget						widget,
	XtPointer					Dummy_client_data,
	XmTextVerifyCallbackStruct	*cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	int					origPasswordLen;	/* length of "password" on entry */
	char				*tmpPasswd;
	int					i;

	origPasswordLen = (password) ? strlen( password ) : 0;

	if (!isSetPassword && cbs->startPos < cbs->currInsert )	/* backspace */
	{
		if (password)	/* if part of the password is already saved */
		{
			cbs->endPos = origPasswordLen;		/* deletion must go to the end*/
			if (cbs->startPos)
			{
				password[cbs->startPos] = STREND; /*delete in the saved passwd*/
			}
			else	/* deleting the entire saved password */
			{
				XtFree(password);	/* still points to freed mem */
				password = NULL;
			}
		}
	}
	else if (!isSetPassword && (cbs->text->length > 1	/* multichar paste */
			|| cbs->currInsert < origPasswordLen))		/* insert mid-word */
	{
		/* disallow:
		 --		multichar "paste" operations
		 --		inserting chars in the middle of the password
		*/
		cbs->doit = False;
	}
	else if (cbs->text->length)
	{
		if (isSetPassword)
		{
			/* discard the old password */
			if (password)
				XtFree( password );
			tmpPasswd = XtMalloc( cbs->text->length + 1 );
			tmpPasswd[0] = STREND;
			origPasswordLen = 0;
		}
		else
		{
			tmpPasswd = XtMalloc( origPasswordLen + cbs->text->length + 1 );
			if (password)
			{
				/* ready the saved password so can concatenate the new letter */
				(void) strcpy( tmpPasswd, password );
				XtFree(password);	/* still points to freed mem */
			}
			else
			{
				tmpPasswd[0] = STREND;
			}
		}
		password = tmpPasswd;

		/* concatenate the new text to the saved password */
		(void) strncat( password, cbs->text->ptr , cbs->text->length );
		password[origPasswordLen + cbs->text->length] = STREND;

		/* echo asterisks instead of the typed-in password */
		for (i = 0 ; i < cbs->text->length; i++)
			cbs->text->ptr[i] = '*';
	}

END_CONTEXT

	return;
}


/*==============================================================================
Function:		get_database

Description:	gets the string from the database field when the field
				is activated.

Parameters:		widget	-	the database widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Mon Jul 10 15:27:47 PDT 1995

Notes:			Empty-string database is the same as no database
==============================================================================*/
/* ARGSUSED1 */
static void
get_database(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	char				*tmpDatabase;

	if (strlen( tmpDatabase = XmTextGetString( widget ) ))
	{
		if (databaseValue)	XtFree( databaseValue );	/*points to freed mem */
		databaseValue = XtNewString( tmpDatabase );	/* save the value for aps */

		/* get the env variable */
		tmpDatabase = getenv( APSDB_ENVVAR );

		/* set the new value in the env var */
		set_databaseEnv( tmpDatabase, &databaseValue );

		/* databaseValue may have changed in set_databaseEnv, so set on window*/
		XmTextFieldSetString( widget, databaseValue );
	}
	else
	{
		if (databaseValue)
		{
			XtFree( databaseValue );
			databaseValue = NULL;
		}
	}

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_map_login

Description:	do what is necessary to have a correct window when the window
				is mapped: save the values for "reset"

Parameters:		None

Returns:		

Creator:		Teresa McKillop

Creation Date:	Tue Jul 11 09:45:43 PDT 1995

Notes:			NOT a true callback: topLevelShell is not mapped, so
				map callback is not executed.  The only startup callback
				is the "create" one, which is called before the children
				of topLevelShell are created.
==============================================================================*/
void
cb_map_login()
{
BEGIN_CONTEXT( APSLoginForm )

	/* save the values for "reset" */
	save_login();

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_reset_login

Description:	restore the login values to the last applied ones.

Parameters:		None

Returns:		

Creator:		Teresa McKillop

Creation Date:	Tue Jul 11 09:56:08 PDT 1995

Notes:			
==============================================================================*/
/* ARGSUSED0 */
void
cb_reset_login(
	Widget		Dummy_widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	if (userid)		XtFree( userid );	/* still points to freed mem */
	userid = XtNewString( lastApplied_userid );	/* set for aps */
	XmTextFieldSetString( LoginTextField, userid );	/* set the window */

	if (password)	XtFree( password );		/* still points to freed mem */
	password = XtNewString( lastApplied_password );	/* set for aps */
	isSetPassword = True;
	XmTextFieldSetString( PasswordTextField, password );	/* set the window */
	isSetPassword = False;

	if ( databaseValue)	XtFree(  databaseValue );  /*still points to freed mem*/
	databaseValue = XtNewString( lastApplied_database );	/* set for aps */
	XmTextFieldSetString( DatabaseTextField, databaseValue ); /*set the window*/

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_verify_login

Description:	verify that the login is valid, and if so, apply the
				login values and bring up aps

Parameters:		widget	-	the login widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Mon Jul 10 12:44:26 PDT 1995

Notes:			this login is to get the Sybase login values, so verification
				consists of making sure the needed values are specified
==============================================================================*/
/* ARGSUSED1 */
void
cb_verify_login(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
BEGIN_CONTEXT( APSLoginForm )

	Widget				ShellWidget;
	Boolean				loginIsValid = True;

	/* Get the values */
	get_userid( LoginTextField, (XtPointer) NULL, (XtPointer) NULL );
	/* password is saved, letter-by-letter, so is already "gotten" */
	get_database( DatabaseTextField, (XtPointer) NULL, (XtPointer) NULL );

	/* Verify the login: ie, the necessary values are set */
	(void) sprintf( display_string,
			"APS Login:\n\nRequired values are missing:\n" );
	if (!userid)
	{
		(void) strcat( display_string, "    Login\n" );
		loginIsValid = False;
	}
	if (!password)
	{
		(void) strcat( display_string, "    Password\n" );
		loginIsValid = False;
	}
	if (!databaseValue)
	{
		(void) strcat( display_string, "    Database\n" );
		loginIsValid = False;
	}

	if (loginIsValid == True)
	{
		/* Apply the values */
		save_login() ;

		/* Popdown the login window */
		ShellWidget = gui_GetShellWidget( widget );
		XtPopdown( ShellWidget );

		/*----------------------------------------------------------------
		 * Create and popup the first window of the main menu.
		 * [NOTE: cannot create main menu until loginIsValid because it
		 * uses the validated login values to open the database (dbopen)]
		 *---------------------------------------------------------------*/
		/* have to reset mainIface BEFORE calling create_APSMainMenu()
		-- because it is referenced in cb_create_aps_interfaces() which
		-- is called in create_APSMainMenu()
		*/
		mainIface = NULL;
		mainIface = create_APSMainMenu((swidget)NO_PARENT); 

		/*
		** Display the main menu window.
		*/
		ShellWidget = gui_GetShellWidget( APSMainMenu );
		XtPopup( ShellWidget, XtGrabNone );
	}
	else
	{
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string,
				XtGrabNone );
	}

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_exit_login

Description:	exit aps

Parameters:		widget	-	the exit widget

Returns:		

Creator:		Teresa McKillop

Creation Date:	Tue Jul 11 10:12:49 PDT 1995

Notes:		
==============================================================================*/
/* ARGSUSED0 */
void
cb_exit_login(
	Widget		widget,
	XtPointer	Dummy_client_data,
	XtPointer	Dummy_cbs )
{
	exit( APS_EXIT_OK );
}


/*==============================================================================
Function:		save_login

Description:	saves the last applied login values for resetting

Parameters:		None

Returns:		

Creator:		Teresa McKillop

Creation Date:	Tue Jul 11 09:45:43 PDT 1995

Notes:			After XtFree(), the pointer still points t the freed memory
==============================================================================*/
static void
save_login()
{
BEGIN_CONTEXT( APSLoginForm )

	if (lastApplied_userid)		XtFree( lastApplied_userid );
	lastApplied_userid = XtNewString( userid );

	if (lastApplied_password)		XtFree( lastApplied_password );
	lastApplied_password = XtNewString( password );

	if (lastApplied_database)	XtFree( lastApplied_database );
	lastApplied_database = XtNewString( databaseValue );

END_CONTEXT

	return;
}


/*==============================================================================
Function:		set_databaseEnv

Description:	Sets the database environment var to the new window value

Parameters:		currEnvVarValue	-	current database env var setting
				newEnvVarValue	-	new window value for the env var

Returns:		

Creator:		Teresa McKillop

Creation Date:	Tue Jul 11 09:45:43 PDT 1995

Notes:			
==============================================================================*/
static void
set_databaseEnv(
	char *currEnvVarValue,
	char **newEnvVarValue )
{
	char				*tmpEnvVarValue = *newEnvVarValue;
	int					envVarLen;
	int					retval;

	/* if it is necessary, change the env var */
	if (tmpEnvVarValue
		&& (!currEnvVarValue || strcmp( currEnvVarValue, tmpEnvVarValue )))
	{
		/* new env var is: "APSDB_ENVVAR=tmpEnvVarValue" */
		envVarLen = strlen( APSDB_ENVVAR ) + strlen( tmpEnvVarValue ) + 2;

		/* if can't realloc, envDatabase == NULL */
		if ((envDatabase = realloc( envDatabase, envVarLen )))
		{
			(void) sprintf( envDatabase, "%s=%s", APSDB_ENVVAR,
					tmpEnvVarValue );
			retval = putenv( envDatabase );
		}

		/* if failed to set the new env var, restore to current */
		if (!envDatabase || retval)
		{
			XtFree( tmpEnvVarValue );	/* still points to freed mem */
			tmpEnvVarValue = XtNewString( currEnvVarValue );
			*newEnvVarValue = tmpEnvVarValue;
		}
	}

	return;
}
