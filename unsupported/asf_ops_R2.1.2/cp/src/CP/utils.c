
static char sccsid_utils_c[] = "@(#)utils.c	1.20 97/02/28 12:13:45";

#include <stdio.h>   
#include <stdlib.h>   
#include <string.h>   
#include <errno.h>   
#include <sys/types.h>     /* ctime_r */
#include <time.h>     /* ctime_r */

#include <sys/stat.h>   /* open */
#include <fcntl.h>      /* open */
#include <unistd.h>     /* read */

#include <sys/types.h>  /* open, statfs */
#include <sys/statfs.h> /* statfs */


void printStringArray(char *name, int howMany, char **strings)
{
  int j;
#ifdef DEBUG
    printf("\tstring array %d %s: ", howMany, name);
    for (j=0; j<howMany && strings[j] != NULL; j++)  {
      /* printf("%s at 0x%x \n", strings[j], strings[j]); */
      printf("%s ", strings[j]);
    }
    printf("\n");
#endif
}

void printIntArray(char *name, int howMany, int *ints)
{
  int j;
#ifdef DEBUG
    printf("\tint array %d %s: ", howMany, name);
    for (j=0; j<howMany ; j++)  {
      printf("%d ", ints[j]);
    }
    printf("\n");
#endif
}

void printDoubleArray(char *name, int howMany, double *doubles)
{
  int j;
#ifdef DEBUG
    printf("\tdouble array %d %s: ", howMany, name);
    for (j=0; j<howMany ; j++)  {
      printf("%lf ", doubles[j]);
    }
    printf("\n");
#endif
}



/*----------------------------------------------------------
 * NAME:
 *  string_to_timeval
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int string_to_timeval(char* ts, struct timeval *tv)
{
    float frac;
    struct tm t;
    memset(&t, 0, sizeof(struct tm));

    if (strchr(ts, '.') == NULL) {
        if (sscanf(ts, "%d-%dT%d:%d:%d",
                &t.tm_year, &t.tm_yday, &t.tm_hour, &t.tm_min, &t.tm_sec) != 5)
            return (-1);
        tv->tv_usec = 0;
    }
    else if (sscanf(ts, "%d-%dT%d:%d:%f",
                    &t.tm_year, &t.tm_yday, &t.tm_hour, &t.tm_min, &frac) != 5)
        return (-1);
    else {
        t.tm_sec = frac;
        tv->tv_usec  = ((frac - t.tm_sec) * 1000000);
    }
    if (t.tm_hour > 23 || t.tm_min  > 59  || t.tm_sec > 61 ||
        t.tm_yday < 1  || t.tm_yday > 366 || t.tm_year < 1900)
        return (-1);

    t.tm_mday = t.tm_yday;
    t.tm_yday -= 1;
    t.tm_year -= 1900;
    t.tm_isdst = -1;
    tv->tv_sec = mkTime(&t);;

    return (0);

} /* string_to_timeval */

/*----------------------------------------------------------
 * NAME:
 *  timeval_to_string
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int timeval_to_string(char* buf, struct timeval* tp)
{
    int n;
    struct tm tm;
    (void) localtime_r((time_t*) tp, &tm);

    n = sprintf(buf, "%.4d-%.3dT%.2d:%.2d:%.2d",
                tm.tm_year+1900,
                tm.tm_yday+1,
                tm.tm_hour,
                tm.tm_min,
                tm.tm_sec);
    /* if (tp->tv_usec) */
        n += sprintf(buf+n, ".%.3d", tp->tv_usec/1000);
    return n;
}



void printTm(char *str, struct tm tm) /* debug routine */
{
printf("%s:isdst %2d yday %3d wday %3d yr %3d mo %3d mday %3d hr %3d min %3d sec %3d \n",
        str,
        tm.tm_isdst, tm.tm_yday, tm.tm_wday,
        tm.tm_year, tm.tm_mon, tm.tm_mday,
        tm.tm_hour, tm.tm_min, tm.tm_sec);
}


time_t timeval_to_time(struct timeval tv)
{
struct tm tm;
time_t mkTimeVal;

localtime_r((time_t*) &tv, &tm);

mkTimeVal = mkTime(&tm);

#ifdef DEBUG
printf("%.4d-%.3dT%.2d:%.2d:%.2d.%.3d\n",
        tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour,
        tm.tm_min, tm.tm_sec, tv.tv_usec/1000);

printf("mkTime val %ld\n", mkTimeVal);
#endif

return(mkTimeVal);

}

char *timestamp()
{
  time_t clock;
  char ts[28], *t;

 /* ctime_r((time(&clock), &clock), ts, sizeof(ts)); */
  clock = time ((time_t *) 0);
 (void) ctime_r (&clock, ts );

  ts[strlen(ts)-1] = '\0';  /* remove \n */

  t = malloc(strlen(ts));
  strcpy(t, ts);
  return(t);

}

void bsort(int *array, int arraySize) /* bubble sort.  who cares if it's slow */
{
  int i, j, temp;
#ifdef SORT_DEBUG
printf("sorting unordered array ");
for (i=0; i<arraySize; i++) printf("%d ", array[i]); 
#endif

  for (i = 0; i < arraySize; i++) 
    for (j = arraySize-1; i < j; --j)
      if (array[j-1] > array[j]) {
        temp = array[j-1];
        array[j-1] = array[j];
        array[j] = temp;
      }

#ifdef SORT_DEBUG
printf("to ordered array: ");
for (i=0; i<arraySize; i++) printf("%d ", array[i]); printf("\n");
#endif

      
}

/*----------------------------------------------------------
 * NAME:
 *  getDirFree
 *
 * DESCRIPTION:
 *  returns number of free blocks in directory.  returns 0
 *  if there was a problem getting the file system information
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

long getDirFree(char *dirName)
{
  int ret, fstyp=0;
  struct statfs buf;

  ret = statfs(dirName, &buf, sizeof (struct statfs), fstyp); /*  0 = success */
                                                              /* -1 = error */

#ifdef SIZE_DEBUG
  printf("\tgetDirFree: %s size %ld\n", dirName, buf.f_bfree); 
#endif
  return(ret ? 0 : buf.f_bfree );  

} /* getDirFree */

/*----------------------------------------------------------
 * NAME:
 *  readFile
 *
 * DESCRIPTION:
 *  read the contents of a file into a string and return this string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char *readFile(char *filename)
{
  char *fileStr;
  struct stat fs;
  int retval, fd;
  fd = open(filename, 0);
  if (fd == -1) 
    return(NULL);

  retval = fstat(fd, &fs);

  if ((fs.st_mode & S_IFMT) == S_IFDIR) 
    return(NULL);

  fileStr = (char *)malloc(fs.st_size);

  if (read(fd, fileStr, fs.st_size) != fs.st_size) {
    free(fileStr);
    return(NULL);
  }
  close(fd);

  fileStr[fs.st_size-1] = '\0';
  return(fileStr);
} /* readFile */



/*----------------------------------------------------------
 * NAME:
 *  readIntStrFromFile
 *
 * DESCRIPTION:
 *  get a line from the file specified by the file pointer fp,
 *  and return its value as an int
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int readIntStrFromFile(FILE *fp, char *str, int len)
{
  if (fgets(str, len, fp) == NULL)
    return(0);
  else 
    return(atoi(str));
}

/*----------------------------------------------------------
 * NAME:
 *  readLineStrFromFile
 *
 * DESCRIPTION:
 *  get a line from the file specified by the file pointer fp,
 *  and return its value as a string (minus the \n)
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int zzzreadLineStrFromFile(FILE *fp, char *str, int len)
{
  char *p = malloc(len);

  fgets(p, len, fp);
  if (p == NULL)
    return(0);
printf("readLineStrFromFile - read %s. from file\n", p);
  p[strlen(p)-1] = '\0';  /* remove \n */
printf("readLineStrFromFile - removed \n from %s.\n", p);
  str = malloc(strlen(p));
  if (str == NULL)  
    return(0);
  strcpy(str, p);
printf("readLineStrFromFile - returning %s.\n", str);
  free(p);
  return(strlen(str));
}

char * readLineStrFromFile(FILE *fp, int len, int *bytes_read)
{
  char *str, *p = malloc(len);

  fgets(p, len, fp);
  if (p == NULL)
    return(NULL);

  if (p[strlen(p)-1] == '\n') /* remove trailing \n */
    p[strlen(p)-1] = '\0';  

  str = malloc(strlen(p));
  if (str == NULL)
    return(NULL);
  strcpy(str, p);

  free(p);
  *bytes_read = strlen(str);
  return(str);
}



/*----------------------------------------------------------
 * NAME:
 *  doesFileExist
 *
 * DESCRIPTION:
 *  check if file exists, and is of a nonzero size.
 *  returns 1 if exists and nonzero size; 0 otherwise
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int doesFileExist(char *namePtr)
{
 struct stat fs;
 int    retval;

  retval = stat(namePtr, &fs);
  if (retval == -1) {
   return(0);
  }

  if ((fs.st_mode & S_IFMT) == S_IFREG && fs. st_size > 0)
   return(1);

  return(0);

} /* doesFileExist...........................*/


/*----------------------------------------------------------
 * NAME:
 *  nonzeroFileExists
 *
 * DESCRIPTION:
 *  check if file exists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int nonzeroFileExists(char *namePtr)  /* return 1 for exist, 0 for not exist */
{
 struct stat fs;
 int    retval;

  retval = stat(namePtr, &fs);
  if (retval == -1) {
   return(0);
  }

  if ((fs.st_mode & S_IFMT) == S_IFREG && fs. st_size > 0)
   return(1);

  return(0);

} /* nonzeroFileExists...........................*/

/*----------------------------------------------------------
 * NAME:
 *  fileExists
 *
 * DESCRIPTION:
 *  check if file exists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int fileExists(char *namePtr)  /* return 1 for exist, 0 for not exist */
{
 struct stat fs;
 int    retval;

  retval = stat(namePtr, &fs);
  if (retval == -1) {
   return(0);
  }

  if ((fs.st_mode & S_IFMT) == S_IFREG)
   return(1);

  return(0);

} /* fileExists...........................*/


/*----------------------------------------------------------
 * NAME:
 *  dirExists
 *
 * DESCRIPTION:
 *  check if directory exists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int dirExists(char *namePtr)  /* return 1 for exist, 0 for not exist */
{
 struct stat fs;
 int    retval;

  retval = stat(namePtr, &fs);
  if (retval == -1) {
   return(0);
  }

  if ((fs.st_mode & S_IFMT) == S_IFDIR)
   return(1);

  return(0); /* exists but is not a directory */

} /* dirExists...........................*/



#ifdef CONVERT_ENV
#include <sys/syslog.h> /* convertEnvFile */
char *convertEnvFile(char *inFile)
{
  int envLen;
  char *envTranslation, *envPart, *filePart, *outFile ;

  if (inFile == NULL)
    return(NULL);

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
#endif
