#ifndef _utils_h__
#define _utils_h__

static char sccsid_utils_h[] = "@(#)utils.h	1.6 96/10/25 10:50:20";



time_t mkTime();
time_t timeval_to_time(struct timeval tv);

int string_to_timeval(char* ts, struct timeval *tv);
int timeval_to_string(char* buf, struct timeval* tp);

int doesFileExist(char *namePtr);

char *convertEnvFile(char *inFile);
char *readFile(char *filename);
int readIntStrFromFile(FILE *fp, char *str, int len);
int dirExists(char *namePtr) ;
long getDirFree(char *dirName);


#endif       /* !_utils_h__ */

