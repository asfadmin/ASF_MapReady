/* hjwlib.c
*  a few usefull functions
*	by Hans-Joerg Wagner
*/
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "hjwlib.h"

char *g_hjw_filename = NULL;

/**********************************************************************
* fopendef
* openes a file like fopen. If name is NULL the name of the file will
* be created by appending defext to defroot.
* stores the produced file name in the global variable g_hjw_filename
*
* 07-28-1994	Hans-Joerg Wagner
*/
FILE *fopendef(name,defroot,defext,mode)
char *name,*defroot,*defext,*mode;
{
char *pTmp;
FILE *pRet;
 
if (NULL == name) {
        pTmp = malloc(strlen(defroot) + strlen(defext) + 1);
        if (NULL == pTmp) return NULL;
        strcpy(pTmp,defroot);
        strcat(pTmp,defext);
        }
else
        pTmp = name;
pRet = fopen(pTmp,mode);

/* save file name */
if (NULL == g_hjw_filename) free(g_hjw_filename);
g_hjw_filename = malloc(strlen(pTmp)+1);
strcpy(g_hjw_filename,pTmp);

/* free pTmp if necessary */
if (NULL == name)
        free(pTmp);
return pRet;
}

/**********************************************************************
* fgets_unknown_length
* reads a line with unknown length from a file into a buffer. 
*    arguments:
* file	file to read from
* buf	pointer to an buffer, allocated with malloc. If this argument
*	is NULL the function will allocate a new buffer. In either case
*	the allocated memory should be freed using the free function.
*	Caused by use of the realloc function the pointer passed in buf is 
*	invalid on return. The new pointer is returned.
* pLen	current length of the buffer. Unsigned integer. Will be updated
*	by function
*
*    return value:
* pointer to the valid buffer. USE THIS POINTER instead of the pointer
* passed in argument buf, because this pointer can be invalid after calling
* this function.
* If the function was unsuccessful all memory will be disallocated and
* NULL will be returned
*
* for example of useage see sdevf.c
*/
char *fgets_unknown_length(file,buf,pLen)
FILE *file;
char *buf;
unsigned *pLen;
{
char *pTmp;
char *newBuf;
if (NULL == buf) {
	newBuf = malloc(ALLOCINC+1);
	(*pLen) = ALLOCINC;
	}
else
	newBuf = buf;

if (NULL == newBuf)
	return NULL;

newBuf[0] = '\0';
pTmp = newBuf;
do {
    fgets(pTmp,(*pLen) - strlen(newBuf),file);
    if (newBuf[strlen(newBuf)-1] != '\n' ) {
	buf = realloc(newBuf,(*pLen) + ALLOCINC);
	if (NULL == buf) {
	    free(newBuf);
	    return NULL;
	    }
	newBuf = buf;
	pTmp = newBuf + (*pLen) - 1;
	(*pLen) += ALLOCINC;
	}
    else 
	break;
    } while (!feof(file));
return newBuf;
}

/****************************************************************
* edit_lines
* writes given comment lines to a file, executes vi opening this
* file and rereads it into the comment lines
* the end of the comment lines is marked with an NULL pointer
* the lines are cut after 80 bytes
*/
char **edit_lines(comments)
char **comments;
{
char *pszFile;
char *pszEditor;
FILE *file;
char szTmp[128];
int count, count1;

pszFile = tempnam(NULL,"hjwlib");
printf("temfile = %s\n",pszFile);
if (comments != NULL) {
	/* writing existing command line */
	file = fopen(pszFile,"w");
	if (NULL == file)
		return NULL;
	for (count = 0; comments[count] != NULL; count++)
		fputs(comments[count],file);
	fclose(file);
	}
/* looking for standard editor */
pszEditor = getenv("EDITOR");
if (NULL == pszEditor) pszEditor = "vi";
/* executeing editor */
sprintf(szTmp,"%s %s",pszEditor,pszFile);
system(szTmp);

/* rereading file */
file = fopen(pszFile,"r");
if (NULL == file) {
	comments = (char **)malloc(sizeof(char *));
	(*comments) = NULL;
	}
else {
    /* how many lines are in there */
    for (count = 0; NULL != fgets(szTmp,81,file); count++);

    /* allocating memory */
    comments = (char **)calloc(count+1,sizeof(char *));
    if (NULL == comments)
	return NULL;
    /* reading */
    rewind(file);
    for (count1 = 0; count1 < count; count1++) {
	fgets(szTmp,81,file);
	comments[count1] = malloc(strlen(szTmp) + 1);
	if (NULL == comments[count1] )
		return NULL;
	/* remove newline character */
	if (szTmp[strlen(szTmp)-1] == '\n') szTmp[strlen(szTmp)-1] = '\0';
	strcpy(comments[count1],szTmp);
	}
    /* set end of comments mark */
    comments[count1] = NULL;
    }
/* remove temporary file */
unlink(pszFile);
return comments;
}

/**************************************************************************
* PlotDiagram
* is an universal program to plot a diagram using the tool diagram
*/ 
