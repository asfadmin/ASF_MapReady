static char sccsid_genutils_c[] = "@(#)genUtils.c	1.6 96/10/31 11:31:39";

#include <stdio.h>   
#include <stdlib.h>   
#include <string.h>   
#include <sys/types.h> /* ctime_r */
#include <time.h>      /* ctime_r */
#include <syslog.h>      

#ifdef __sgi
/*----------------------------------------------------------
 * NAME:
 *  mkTime
 *
 * DESCRIPTION:
 *  replacement routine for IRIX 5.x mktime()
 *  which has a leap year bug
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
time_t mkTime(struct tm *t) 
{                          
  time_t t_sec;
  int leapFix=0;

  if (t->tm_mday == 366) {
    t->tm_mday = 365;
    leapFix = 1;
  }

  t_sec = mktime(t);
  if (leapFix)
    t_sec += 86400;
  return(t_sec);

} /* mkTime */
#endif

/*----------------------------------------------------------
 * NAME:
 *  convertEnvFile
 *
 * DESCRIPTION:
 *  expand a file name with an embedded environment variable
 *  to the complete full path name.  returns NULL if this
 *  environment variable is not set.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char *convertEnvFile(char *inFile)
{
  int envLen;
  char *envTranslation, *envPart, *filePart, *outFile ;

  if (inFile[0] == '$') {
    envPart = (char *) malloc(strlen(inFile) + 1);
    if (envPart == NULL) {
      printfLLog(LOG_DEBUG, "convertEnvFile() error allocating memory\n");
      return(NULL);
    }
    strcpy(envPart, inFile);

    filePart = strchr(inFile, '/');      /* save file part of incoming name */
    envLen = strlen(envPart) - strlen(filePart);
    envPart[envLen] = '\0';              /* save env part of incoming name */
    envTranslation = getenv(&envPart[1]); /* name starts after $ character */
    free(envPart);
    if (envTranslation == NULL) {  /* this environment variable not set */
      return(NULL); 
    }

    outFile = (char *) malloc(strlen(envTranslation) + strlen(filePart) + 2);
    if (outFile == NULL) {
      printfLLog(LOG_DEBUG, "convertEnvFile() error allocating memory\n");
      return(NULL);
    }
    sprintf(outFile, "%s%s", envTranslation, filePart);
  }
  else
    outFile = inFile;

  return(outFile);

} /* convertEnvFile */


