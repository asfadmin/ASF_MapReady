/***********************************************************************
* editheader
* tool to edit a header of a drd file
*
* 08-11-1994 by Hans-Joerg Wagner
*/

#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "drdapi.h"
#include "hjwlib.h"

void Hello();
void Bye();
void Usage();
int CmdLine();
void CopyFile();

struct {
    char *pszInFile;
    char *pszOutFile;
    } g_cmdLine;

/* main program */
main(argc,argv)
int argc;
char **argv;
{
Hello();

if (!CmdLine(argc,argv)) {
	Usage();
	exit(11);
	}

if (!drdInit("editheader",1,0,"Alaska SAR Facility") ) {
	printf("error initializing drd library\n");
	}
else {
	CopyFile();
	drdExit();
	}
Bye();
}

void Hello()
{
printf("EDITHEADER tool to change headers of drd files\n");
}

void Bye()
{
printf("EDITHEADER says thank you. Join us again\n");
}

void Usage()
{
printf("usage:\n");
printf("\teditheader infile, outfileroot\n");
printf("\t      infile is the full name of the input file\n");
printf("\t      outfileroot is the root of the output file. The extention\n");
printf("\t                  is automaticaly set to the same as infile\n");
printf("\t		    if this parameter is not given infile will be\n");
printf("\t                  overwritten\n");
}

int CmdLine(argc,argv)
int argc;
char **argv;
{
char *ext;

/* argv[0] = "editheader" */
/* argv[1] = infile */
/* argv[2] = outfileroot */

if (argc > 3 || argc < 2)
	return 0;

g_cmdLine.pszInFile = argv[1];
g_cmdLine.pszOutFile = ((argc == 3)? argv[2] : NULL);
return 1;
}


void CopyFile()
{
DRDFILE *pFH;
char *pszOutFile;
char *pTmp;
int ret;

/* open input file */
pFH = drdOpenUnknown(g_cmdLine.pszInFile);
if (NULL == pFH) {
	printf("error opening file %s\n",g_cmdLine.pszInFile);
	return;
	}

/* creating outfile name */
if (NULL == g_cmdLine.pszOutFile) {
	/* if no outfile given, create temporary file */
	pTmp = tempnam("./","drdcpy");
	pszOutFile = malloc(strlen(pTmp) + 1);
	if (NULL == pszOutFile) {
		printf("error allocating memory for outfile name\n");
		drdClose(pFH);
		return;
		}
	strcpy(pszOutFile,pTmp);
	}
else {
	/* else create full name */
	pTmp = strrchr(g_cmdLine.pszInFile,'.');
	if (NULL == pTmp) {
		printf("infile has no extention\n");
		drdClose(pFH);
		return;
		}
	pszOutFile = malloc(strlen(g_cmdLine.pszOutFile) + strlen(pTmp) + 1);
	if (NULL == pszOutFile) {
		printf("error allocating memory for outfile name\n");
		drdClose(pFH);
                return;
                }
        strcpy(pszOutFile,g_cmdLine.pszOutFile);
	strcat(pszOutFile,pTmp);
	}

/* edit comments */
pFH->apszComments = edit_lines(pFH->apszComments);

/* open output file and copy */
/* bDrdDebug = 1; */
printf("Rewrite data\n");
ret = drdCopyFile(pFH,pszOutFile,NULL,NULL,0,0,NULL,NULL,pFH->apszComments);
if (1 == ret)
	printf("error rewriting file\n");
else if (2 == ret)
	printf("cannot lock input file, probably used by another process\n");

/* if no outfile given in command line, overwrite file with temp file */
if (NULL == g_cmdLine.pszOutFile)
	execlp("mv","mv",pszOutFile,g_cmdLine.pszInFile);
	
free (pszOutFile);

drdClose(pFH);
return;
}








