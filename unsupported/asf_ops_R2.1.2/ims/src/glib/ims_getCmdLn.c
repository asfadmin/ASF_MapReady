static char *sccs = "@(#)ims_getCmdLn.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:        ims_getCmdLn.c
**
** Function:    This module reads contains functions that open a command file
**              and return command lines.
**
** Author:      J. Jacobson
**
** Date:        1/10/90
**
** Modified:    8/90 - H. Sayah
**              Replaced unix hashing functions with
**              local CDB hash functions: cdb_hashCreate(), cdb_hashDestroy(),
**              cdb_hashSearch().  This should provide a faster and more 
**              portable hashing capability.
**
**              1/16/91 Fix NULL last-argument problem.
**
**              2/9/95 - S. Hardman - R1B
**              Cleaned up the file, also removed the message descriptor
**              argument from getCmdLineTokens() because it wasn't being used.
**
******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_hash.h>
#include <ims_cmd.h>
#include <ims_confSubs.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_cmd.h.
** They are listed here for reference.
**
**	int ims_getCmdLine (int, char *[], IMS_CMD_CLASS [], int,
**		IMS_MSG_STRUCT *);
*/

/*
** Local Function Declaration.
*/
IMS_CMD_TOKENS *getCmdLineTokens (char *, int *, int);

/******************************************************************************
**
** ims_getCmdLine ()
**
** Get arguments from the command line.
**
******************************************************************************/

int ims_getCmdLine (
	int argc,
	char *argv[],
	IMS_CMD_CLASS cmdElm[], 
	int cmdElmCount,
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_CMD_TOKENS *cmd;
	int index;
	IMS_HASH_STRUCT *hashPtr;
	char *command;

	/*
	** Initialize lookup table for our list of file configuration
	** parameters.
	*/
	if (ims_openLookup (&hashPtr, cmdElm, cmdElmCount, msgDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** For now, get each directive from the command file.
	*/
	for (index = 1; index < argc; index++)
	{
		/* Now get the keyword and value tokens. */
		command = argv[index];
		if ((cmd = getCmdLineTokens (command, &index, argc)) ==
			(IMS_CMD_TOKENS *) NULL)
		{
			if (*command == '+' || *command == '-')
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Unknown command option '%s'.", command);
				return (IMS_ERROR);
			}
			else
			{
				ims_closeLookup (hashPtr, msgDesc);
				return (index);
			}
		}

		/*
		** Now, check to see if this is a valid keyword (i.e., look it
		** up to see if it's there and to get the type.  Put its value
		** in our command structure too.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		if ((IMS_CMD_CLASS *) ims_lookup (hashPtr, cmd->keyword, cmd->value,
			msgDesc) == (IMS_CMD_CLASS *) NULL)
		{
			ims_closeLookup (hashPtr, msgDesc);
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	/*
	** Now, destroy the hash table, cause our structure has already 
	** been populated.
	*/
	ims_closeLookup (hashPtr, msgDesc);

	return (index);
}


/******************************************************************************
**
** getCmdLineTokens ()
**
** Extract the usable bits of a command string, based
** on the command type.  Takes argc to check index
** against the number of arguments passed.
**
******************************************************************************/

IMS_CMD_TOKENS *getCmdLineTokens (
	char *command,
	int *index,
	int argc)
{
	char *p, *q;      /* Scratch pointers for scanning commandline. */
	static IMS_CMD_TOKENS standard;
	static int firstTime = IMS_TRUE;
	static char savedPassword[31];

	/*
	** First, decode command type, + or -.
	*/
	p = command;
	switch (*p)
	{
	case '-':
		/*
		** For now, just implement the old - stuff.
		** Note: there is no enforcement of P, p standards.
		*/
		standard.keyword[0] = *(p++);
		standard.keyword[1] = *(p++);
		standard.keyword[2] = '\0';
		break;   /* Break to common handling. */

	case '+':
		/* Get long keyword.  Check length later. */
		q = standard.keyword;
		while (*p != '\0')
		{
			*(q++) = *(p++);
		}
		*q = '\0';
		break;   /* Break to common handling. */

	default:
		return ((IMS_CMD_TOKENS *) NULL);
	}

	/*
	** Find the value.
	*/
	if (*p == '\0')
	{
		/*
		** Case:  The next argv is the one.  But first, check to see if
		**        we've exhausted our argument list.  If so, return a
		**        NULL value to the user.
		*/
		if (*index == argc - 1)
		{
			/* Case: The last arguement on the command line is NULL. */
			standard.value[0] = '\0';
			return ((IMS_CMD_TOKENS *) &standard);
		}

		/* Point to the start of the next arguement. */
		p++;

		/*
		** An afterthought:  If the char is not alpha, then treat
		** this as an argument too (force a restriction on the first
		** character of an argument, and you can handle -numbers.
		** The restriction:  It must be a-z, A-Z.).
		*/
		if ((*p == '-' || *p == '+') && isalpha (*(p+1)))
		{
			/* This is a null argument.  (Might be a flag.) */
			--p;
		}
		else
		{
			/* The next argv is our value, so adjust the index. */
			*index = *index + 1;
		}
	}

	/*
	** Now, if the option is a 'P', it gets special handling, that
	** is, it gets erased.  Also, if this function is called a second
	** time, then "remember" the password.
	*/
	if (strncmp (command, "-P", 2) == 0)
	{
		if (firstTime == IMS_TRUE)
		{
			(void) strncpy (standard.value, p, sizeof (standard.value));
			(void) strncpy (savedPassword, p,  sizeof (savedPassword));
			while (*p != ' ' && *p != '\n' && *p != '\0')
			{
				*(p++) = ' ';
			}
			firstTime = IMS_FALSE;
		}
		else
		{
			(void) strncpy (standard.value, savedPassword,
				sizeof (standard.value));
		}
	}
	else
	{
		(void) strncpy (standard.value, p, sizeof (standard.value));
	}

	return ((IMS_CMD_TOKENS *) &standard);
}
