static char *sccs = "@(#)ims_getCmd.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:        ims_getCmd.c
**
** Function:    This module contains functions that open a command file
**              and returns command lines.
**
** Author:      J. Jacobson
**
** Date:        1/10/90
**
** Modified:    7/11/90 - H. Sayah
**              Command parsing routines were removed and
**              were placed in ../ftlib/cdb_pvl.c.  The purpose of this
**              modification is to create a single parser for command line
**              and simple PVL language parsing.  The command line language
**              must be the same as the PVL subset language (KEYWORD = VALUE;).
**
**              8/90 - H. Sayah
**              Replaced unix hashing functions with
**              local cdb hash functions: cdb_hashCreate(), cdb_hashDestroy(),
**              cdb_hashSearch().  This should provide a faster and more 
**              portable hashing capability.
**
**              11/7/90 - H. Sayah
**              The getCmdTokens() routine was modified
**              to allow for quoted character string keyword values without 
**              syntax verification within quotation marks.
**
**              2/7/95 - S. Hardman - R1B
**              Cleaned up the file. Removed the ims_ prefix from getNextCmd()
**              and getCmdTokens() because they are local functions.
**
**              5/31/95 - S. Hardman - R1B
**              Fixed the problem in getNextCmd() where an even number of
**              '*' before an '/' would result in a terminator not found error.
**
**              10/5/95 - S. Hardman - R1B
**              Fixed the problem when spaces followed a comment at the
**              end of the command file.
**
** Notes:       Each specififcation is placed on a line by itself, i.e.,
**              specifications are separated by file-type line terminator.
**
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_hash.h>
#include <ims_cmd.h>
#include <ims_confSubs.h>
#include <ims_util.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_cmd.h.
** They are listed here for reference.
**
**	int ims_getFileParms (char *, IMS_CMD_CLASS [], int, IMS_MSG_STRUCT *);
*/

/*
** Local Functions.
*/
static int readCmdFile (char *, enum ims_cmdFileTypes, IMS_MSG_STRUCT *);
char *getNextCmd (char **, IMS_MSG_STRUCT *, char *);
IMS_CMD_TOKENS *getCmdTokens (char *, enum ims_cmdFileTypes,
	IMS_MSG_STRUCT *);

/*
** Local storage.
*/
static FILE *fp;                          /* Command-file pointer */
static char cmdfileName[IMS_MAXFILESPEC]; /* Remember name for msgs, etc. */

/*
** buffer to store extracted parameter/value information from the 
** file.  bufPtr points to the current working location of the buffer.
*/
static char *buffer;
static char *bufPtr;

/******************************************************************************
**
** ims_getFileParms ()
**
** Get commands from the parameter file.
**
******************************************************************************/

int ims_getFileParms (
	char *fileName,
	IMS_CMD_CLASS cmdElm [],
	int cmdElmCount,
	IMS_MSG_STRUCT *msgDesc)
{
	char *command;
	IMS_CMD_TOKENS *cmd;
	char cmdBuf[IMS_MAXCMDLEN];
	IMS_HASH_STRUCT *hashPtr;

	/*
	** Open the command file.
	*/
	if (readCmdFile (fileName, IMS_STANDARD, msgDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Initialize lookup table for our list of file configuration
	** parameters.
	*/
	if (ims_openLookup (&hashPtr,cmdElm, cmdElmCount, msgDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Extract the parameter-value pairs from the buffer.  buffer
	** got populated when we called the readCmdFile routine.   
	*/
	bufPtr = buffer;
	while ((command = getNextCmd (&bufPtr, msgDesc, cmdBuf)) != (char *) NULL)
	{
		if (ims_msgGetSeverity (msgDesc) != IMS_OK)
		{
			ims_closeLookup (hashPtr, msgDesc);
			return (ims_msgGetSeverity (msgDesc));
		}

		/*
		** Now, get the tokens keyword, value.
		*/
		if ((cmd = getCmdTokens (command, IMS_STANDARD, msgDesc)) ==
			(IMS_CMD_TOKENS *) NULL)
		{
			ims_closeLookup (hashPtr, msgDesc);
			return (ims_msgGetSeverity (msgDesc));
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

	if (ims_msgGetSeverity (msgDesc) != IMS_OK)
	{
		ims_closeLookup (hashPtr, msgDesc);
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Now, destroy the hash table, because our structure has already 
	** been populated. Also free the space allocated by buffer.
	*/
	ims_closeLookup (hashPtr, msgDesc);
	(void) free (buffer);

	return (IMS_OK);
}

/******************************************************************************
**
** readCmdFile ()
**
** Internal func, used to open a command file and setup
** internal data structures to describe the file format.
** Read parameters into a string buffer for later access.
**
******************************************************************************/

static int readCmdFile (
	char *fileName,
	enum ims_cmdFileTypes type,
	IMS_MSG_STRUCT *msgDesc)
{
	char lineBuf[IMS_MAXLINELEN+1];
	unsigned long index;
	char *ptr;
	int sizeBuf;
	int i;

	/*
	** Remember the file name.
	*/
	if (fileName != (char *) NULL)
	{
		(void) strncpy (cmdfileName, fileName, sizeof (cmdfileName));
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Commandfile name not specified.");
		return (IMS_ERROR);
	}

	/*
	** Open the command file.  If this fails, then exit.
	*/
	if ((fp = fopen (cmdfileName, "r")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open command file '%s'", cmdfileName);
		return (IMS_ERROR);
	}
 
	/* 
	** Allocate initial buffer storage area of 1024 bytes.  If more space
	** is required then we realloc later.
	*/
	if ((buffer = (char *) malloc(1024)) == (char *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for command file parameters.");
		return (IMS_FATAL);
	}
	bufPtr = buffer;
	sizeBuf = 1024;

	/*
	** Here we must extract the parameter/value list from the file.
	** Note, no parsing is done at this point. We extract everyThing, 
	** including the comments.
	*/
	while ((ptr = fgets (lineBuf, IMS_MAXLINELEN, fp)) != (char *)NULL)
	{
		/* 
		** Place the Line just extracted int the buffer. 
		** If more storage space is required then allocate the
		** space.
		*/
		i = 0;
		while (*ptr != '\0')
		{
			i++;
			if (i >= sizeBuf)
			{
				/* Need more space. */
				index = bufPtr - buffer;
				if ((buffer = (char *) realloc (buffer, 1024)) ==
					(char *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for command file parameters.");
					return (IMS_FATAL);
				}
				sizeBuf += 1024;
				bufPtr = buffer + index;
			}
			*bufPtr = *ptr;
			ptr++;
			bufPtr++;
		}
		*bufPtr = '\0';
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getNextCmd ()
**
** Entry point, used to get the next command from the command file.
**
** Returns a pointer to a command line.  Returns NULL
** on end of buffer.  Also, strip out any comments.
**
******************************************************************************/

char *getNextCmd (
	char **bufPtr,
	IMS_MSG_STRUCT *msgDesc,
	char *cmdBuf)
{
	char *q, *p;      /* Scratch character pointer. */
	int i;           /* temporary variables */
	int termFound;

	/*
	** Initialize variables.
	*/
	termFound = IMS_FALSE;
	cmdBuf[IMS_MAXCMDLEN-1] = '\0';
	cmdBuf[0] = '\0';
	q = cmdBuf;

	/*
	** Initialize message severity to IMS_OK so it can be tested on
	** return.
	*/
	(void) ims_msgSeverity (msgDesc, IMS_OK);

	/*
	** Get a new PVL expression from the buffer and place it in cmdBuf.
	** make sure bufPtr is not pointing to the end of buffer.
	*/

	/*
	** Get read of all leading blanks and new line characters.
	*/
	while ((**bufPtr == ' ') || (**bufPtr == '\n') ||
		(**bufPtr == '\r'))
	{
		(*bufPtr)++;
	}

	/*
	** No more commands in the buffer.
	*/
	if (**bufPtr == '\0') 
	{
		return ((char *) NULL);
	}

	/*
	** Search the buffer for the PVL expression to return. ";" is the
	** terminator.
	*/
	i = 0;
	p = *bufPtr;
	while ((termFound == IMS_FALSE) && (*p != '\0') &&
		(i < IMS_MAXCMDLEN - 1))
	{
		i++;
		switch (*p)
		{
		case ';':
			termFound = IMS_TRUE;
			break;

		case '\r':
		case '\n':
			/* Do nothing. */
			break;

		case '/':
			if (strncmp(p,"/*",2) == 0)
			{
				p++;
				while (*p != '\0')
				{
					p++;

					/* Check for comment terminator. */
					if ((*p == '*') && (*(p + 1) == '/'))
					{
						p++;
						break;
					}

					/* Check for embedded comment. */
					if ((*p == '/') && (*(p + 1) == '*'))
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
							"Embedded comments are not allowed. Command: '%s'",
							cmdBuf);
						return ((char *) NULL);
					}
				}

				if (*p == '\0')
				{
					*q = '\0';
					(void) ims_msg (msgDesc, IMS_ERROR, 
						"Comment was not terminated. Command: '%s'",
						cmdBuf);
					return ((char *) NULL);
				}
			}
			else
			{
				*q = *p;
				q++;
			}
			break;

		default:
			*q = *p;
			q++;
			break;
		}
		p++;
	}

	*bufPtr = p;
	*q = '\0';

	if (termFound == IMS_TRUE)
	{
		return (cmdBuf);
	}
	else if ((*p == '\0') &&
		((cmdBuf[0] == '\0') || (ims_isBlankString (cmdBuf))))
	{
		return ((char *) NULL);
	}
	else
	{
		if (i >= IMS_MAXCMDLEN-1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
			"Command line larger than maximum buffer size '%d'. Command: '%s'",
			IMS_MAXCMDLEN-1, cmdBuf);
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"Missing ';'.  Command: '%s'", cmdBuf);
		}

		return ((char *) NULL);
	}
}

/******************************************************************************
**
** getCmdTokens ()
**
** Extract the usable bits of a command string, based on the command type.
**
******************************************************************************/

IMS_CMD_TOKENS *getCmdTokens (
	char *command,
	enum ims_cmdFileTypes type,
	IMS_MSG_STRUCT *msgDesc)
{
	char *p;      /* Scratch pointer for scanning commandline. */
	char *q;      /* Scratch pointer for target. */
	static IMS_CMD_TOKENS standard;
	int seqSeen = IMS_FALSE;
	int quoteSeen = IMS_FALSE;
	int endCount = 0;

	/*
	** First, decode the command-file type (the language) and take the
	** appropriate action.
	*/
	seqSeen = quoteSeen = IMS_FALSE;
	endCount = 0;
	switch (type)
	{
	case IMS_STANDARD:
		/* Now, extract the keyword/value pairs. */
		p = command;

		/* Skip space. */
		while (*p && isspace (*p)) p++; 
		q = standard.keyword;
		for (;;)
		{
			if (*p == '\0')
			{
				break;
			}

			if (*p != '=' && !isspace (*p))
			{
				*(q++) = *(p++);
			}
			else
			{
				break;
			}
		}
		*q = '\0';

		/* Skip space, and check for '='. */
		while (*p && isspace (*p))
		{
			p++;
		}

		if (*p != '=')
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Invalid command syntax, no '='. Command: '%s'", command);
			return ((IMS_CMD_TOKENS *) NULL);
		}
		p++;

		/* Skip space. */
		while (*p && isspace (*p))
		{
			p++;
		}

		/* Just scan the value into value. */
		q = standard.value;

		/*
		** Is this a character string with quotion marks?
		** Note, any format of characters within quotation marks are
		** accepted.
		*/
		if (*p == '"')
		{
			quoteSeen = IMS_TRUE;
			*(q++) = *(p++);
		}
		else
		{
			/* Is this a sequence? */
			if (*p == '(')
			{
				seqSeen = IMS_TRUE;
				*(q++) = *(p++);
			}
		}

		while (*p != '\0')
		{   
			if (quoteSeen == IMS_FALSE)
			{
				if (seqSeen == IMS_FALSE)
				{
					switch (*p)
					{
					case ')':
						(void) ims_msg (msgDesc, IMS_ERROR,
						"Invalid command syntax, non-sequence closed. Command: '%s'",
							command);
						return ((IMS_CMD_TOKENS *) NULL);
					}
				}
				else
				{
					switch (*p)
					{
					case ')':
						if (endCount++)
						{
							(void) ims_msg (msgDesc, IMS_ERROR,
							"Invalid command syntax: sequence closed twice. Command: '%s'",
								command);
							return ((IMS_CMD_TOKENS *) NULL);
						}
					}
				}
				switch (*p)
				{
				case '=':
				case '{':
				case '}':
				case '(':
					(void) ims_msg (msgDesc, IMS_ERROR,
						"Command value contains a reserved symbol '=, {, }, ('.  Command: '%s'",
						command);
					return ((IMS_CMD_TOKENS *) NULL);

				case '/':
					if (*(p+1) == '*' || *(p-1) == '*')
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
							"Invalid command value syntax: '%s'", command);
						return ((IMS_CMD_TOKENS *) NULL);
					}
				}
			}
			*(q++) = *(p++);
		}

		*q = '\0';
		if (quoteSeen == IMS_TRUE && *(q-1) != '"')
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"'Quotation' not terminated. Command: '%s'",
				command);
			return ((IMS_CMD_TOKENS *) NULL);
		}
		else
		{
			if (seqSeen == IMS_TRUE && *(q-1) != ')')
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"'Sequence' not terminated by a ')'. Command: '%s'",
					command);
				return ((IMS_CMD_TOKENS *) NULL);
			}
		}
		return ((IMS_CMD_TOKENS *) &standard);

	default:
		/* Invalid file type. */
		(void) ims_msg (msgDesc, IMS_ERROR, "Invalid Commandfile type.");
		return ((IMS_CMD_TOKENS *) NULL);
	}
}
