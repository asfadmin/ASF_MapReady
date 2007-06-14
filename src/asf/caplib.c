/**************************** Fail-Free Library *******************************
These are convenience replacements for the standard library routines.  They
differ from the standard routines ONLY in that they will not fail-- if they
encounter an error, they will print an informative error message to stderr and
exit.  This way, programs can be written more easily, quickly, and cleanly,
and users get better error messages.

Note that to minimize impact on readability and portability, the return types,
error codes, and ALL functionality of the library routines will be passed
through.  For example, MALLOC'ing zero bytes is OK, because malloc'ing zero
bytes is OK.

The only difference is that malloc might return NULL, and MALLOC will never
return NULL (because it will exit before that).

This way, we can have maximum trust in these routines, and use them as much as
possible.
******************************************************************************/
#include "caplib.h"
#include "log.h"

behavior_on_error_t caplib_behavior_on_error = BEHAVIOR_ON_ERROR_ABORT;

void programmer_error(char *mess)
{
	char error_message[1024];
	sprintf(error_message,
		"********* ERROR!  Internal program error *******\n"
		"* \n"
		"*     The following programmer error occured:\n"
		"* {%s}"
		"*     This isn't supposed to make sense to users, but\n"
		"* you as a user aren't supposed to see this.\n"
		"* Please contact ASF via email at uso@asf.alaska.edu.\n"
		"**     Program terminating... internal program error.\n",
		mess);
	fprintf(stderr,error_message);
	if (fLog!=NULL)
	  fprintf(fLog,error_message);

        /* always abort for programmer error */
	exit(199);
}

void bail(const char *mess, ...)
{
  va_list ap;

  fprintf(stderr, "***************** ERROR!  *******\n"
	          "* \n"
	          "*     The following error occured:\n"
                  "* ");
  va_start(ap, mess);
  vfprintf(stderr, mess, ap);
  va_end(ap);
  fprintf(stderr, "**     Program terminating...\n");

  if (fLog!=NULL) {
    fprintf(fLog, "***************** ERROR!  *******\n"
	          "* \n"
	          "*     The following error occured:\n"
                  "* ");
    va_start(ap, mess);
    vfprintf(fLog, mess, ap);
    va_end(ap);
    fprintf(fLog, "**     Program terminating...\n");
  }

  /* bail ignores behavior_on_error, always aborts */
  exit(198);
}

/* MALLOC ignores behavior_on_error -- if out of memory, we should quit */
void *MALLOC(size_t size)
{
  void *ret = malloc(size);
	char error_message[1024];

	if (ret==NULL)
	{
#ifdef ENOMEM
		if (errno==ENOMEM) /*There will never be enough memory.*/
		{
			sprintf(error_message,
				"*****  ERROR!  Out of Memory *******\n"
				"* \n"
				"*    This program needs more memory than your\n"
				"* system has.  It was asking for %i bytes of memory.\n"
				"*    You might ask your system administrator to\n"
				"* increase the size of your swap partition, or add\n"
				"* more real memory to your system.\n"
				"**    Program terminating... Out of memory.\n",
				(int)size);
			fprintf(stderr,error_message);
			if (fLog!=NULL)
			  fprintf(fLog,error_message);

                        exit(200);
		}
#endif
#ifdef EAGAIN
		if (errno==EAGAIN) /*There's not enough memory now.*/
		{
			sleep(2); /*Wait 2 seconds...*/
			ret=malloc(size);/*... try again.*/
			if (ret==NULL)
			{ /*If the call failed again, we just bail.*/
				sprintf(error_message,
					"*****  ERROR!  Out of Memory *******\n"
					"* \n"
					"*    This program needs more memory than your\n"
					"* system has free at this moment.  It was \n"
					"* asking for %i bytes of memory.\n"
					"*    You might try running the program again\n"
					"* later, when there are fewer people on.\n"
					"**    Program terminating... Out of memory.\n",
					(int)size);
				fprintf(stderr,error_message);
				if (fLog!=NULL)
				  fprintf(fLog,error_message);

                                exit(201);
			}
			else return ret;
		}
#endif
	 /*An unknown memory error might cause us to get here.*/
	 	sprintf(error_message,
			"*****  ERROR!  Out of Memory *******\n"
			"* \n"
			"*    This program needs more memory than your\n"
			"* system has.  It was asking for %i bytes of memory.\n"
			"*    You might ask your system administrator to\n"
			"* increase the size of your swap partition, or add\n"
			"* more real memory to your system.\n"
			"**    Program terminating... Out of memory.\n",
			(int)size);
		fprintf(stderr,error_message);
		if (fLog!=NULL)
		  fprintf(fLog,error_message);

                exit(202);
	}
	return ret;
}

/* CALLOC ignores behavior_on_error -- if out of memory, we should quit */
void *CALLOC(size_t nmemb, size_t size)
{
  void *ret = calloc(nmemb, size);
  char error_message[1024];

  if (ret==NULL)
  {
#ifdef ENOMEM
    if (errno==ENOMEM) /*There will never be enough memory.*/
    {
      sprintf(error_message,
              "*****  ERROR!  Out of Memory *******\n"
              "* \n"
              "*    This program needs more memory than your\n"
              "* system has.  It was asking for %i bytes of memory.\n"
              "*    You might ask your system administrator to\n"
              "* increase the size of your swap partition, or add\n"
              "* more real memory to your system.\n"
              "**    Program terminating... Out of memory.\n",
              (int)size * (int)nmemb);
      fprintf(stderr,error_message);
      if (fLog!=NULL)
        fprintf(fLog,error_message);

      exit(200);
    }
#endif
#ifdef EAGAIN
    if (errno==EAGAIN) /*There's not enough memory now.*/
    {
      sleep(2); /*Wait 2 seconds...*/
      ret=calloc(nmemb, size); /*... try again.*/
      if (ret==NULL)
      { /*If the call failed again, we just bail.*/
        sprintf(error_message,
                "*****  ERROR!  Out of Memory *******\n"
                "* \n"
                "*    This program needs more memory than your\n"
                "* system has free at this moment.  It was \n"
                "* asking for %i bytes of memory.\n"
                "*    You might try running the program again\n"
                "* later, when there are fewer people on.\n"
                "**    Program terminating... Out of memory.\n",
                (int)size * (int)nmemb);
        fprintf(stderr,error_message);
        if (fLog!=NULL)
          fprintf(fLog,error_message);

        exit(201);
      }
      else return ret;
    }
#endif
    /*An unknown memory error might cause us to get here.*/
    sprintf(error_message,
            "*****  ERROR!  Out of Memory *******\n"
            "* \n"
            "*    This program needs more memory than your\n"
            "* system has.  It was asking for %i bytes of memory.\n"
            "*    You might ask your system administrator to\n"
            "* increase the size of your swap partition, or add\n"
            "* more real memory to your system.\n"
            "**    Program terminating... Out of memory.\n",
            (int)size * (int)nmemb);
    fprintf(stderr,error_message);
    if (fLog!=NULL)
      fprintf(fLog,error_message);

    exit(202);
  }
  return ret;
}

void FREE(void *ptr)
{
	if (ptr != NULL) free(ptr);
}

void FREE_BANDS(char **ptr)
{
    if (ptr != NULL) {
        int i;
        for (i=0; i<MAX_BANDS; ++i)
            if (ptr[i]) FREE(ptr[i]);
        FREE (ptr);
    }
}


FILE *FOPEN(const char *file,const char *mode)
{
#if defined(win32)
	/* Although there is a man page for fopen64 on windows Cygwin
	 * it seems that the 64 bit functionality is not yet in place */
	FILE *ret=fopen(file,mode);
#else
	FILE *ret=fopen64(file,mode);
#endif
	char error_message[1024];

	if (ret==NULL)
	{
		sprintf(error_message,
			"*****  ERROR!  Cannot Open File! *******\n"
			"* \n"
			"*    This program tried to open the file named\n"
			"* '%s' ",file);
		fprintf(stderr,error_message);
		if (fLog!=NULL)  fprintf(fLog,error_message);

		if (mode[0]=='r') {
			sprintf(error_message,"for reading.\n");
			fprintf(stderr,error_message);
			if (fLog!=NULL) fprintf(fLog,error_message);
		}

		else if (mode[0]=='w'||mode[0]=='a') {
			sprintf(error_message,"for writing.\n");
			fprintf(stderr,error_message);
			if (fLog!=NULL) fprintf(fLog,error_message);
		}

		else {
			sprintf(error_message,"in the unknown way '%s'.\n",mode);
			fprintf(stderr,error_message);
			if (fLog!=NULL) fprintf(fLog,error_message);
		}

                sprintf(error_message,
                        "* This file does not exist or cannot be opened.\n");

                if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT) {
                    strcat(error_message,
                            "* Please check the file name and try again.\n"
                            "**   Program terminating... Cannot open file.\n");
                }

                fprintf(stderr,error_message);
                if (fLog!=NULL) fprintf(fLog,error_message);

                if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT)
                    exit(203);
	}
	return ret;
}


size_t FREAD(void *ptr,size_t size,size_t nitems,FILE *stream)
{
	size_t ret;
	char error_message[1024];

	if (ptr==NULL)
		programmer_error("NULL data buffer passed to FREAD.\n");
	if (stream==NULL)
		programmer_error("NULL file pointer passed to FREAD.\n");
	ret=fread(ptr,size,nitems,stream);
	if (ret!=nitems)
	{
		if (0!=feof(stream))
		{
			sprintf(error_message,
				"*****  ERROR!  Read past end of file! *******\n"
				"* \n"
				"*    This program tried to read %i bytes past\n"
				"* the end of file stream 0x%x.  You might want to\n"
				"* check any image-size related parameters you passed\n"
				"* to the program.\n",
				(int)size*nitems,(int)stream);

                        if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT)
                            strcat(error_message,
                                   "**    Program terminating... Attempted read past end of file.\n");

			fprintf(stderr,error_message);
			if (fLog!=NULL)  fprintf(fLog,error_message);

                        if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT)
                            exit(204);
                        else
                            return ret;
		}

		sprintf(error_message,
			"*****  ERROR!  Error reading file! *******\n"
			"* \n"
			"*    When reading the file stream 0x%x, this program\n"
			"*  encountered the following error:\n",
			(int)stream);
		fprintf(stderr,error_message);
		if (fLog!=NULL) fprintf(fLog,error_message);

		perror(NULL);

		sprintf(error_message,
			"* Note that this was NOT a read-past end of file error.\n");

                if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT)
                    strcat(error_message,
                           "**   Program terminating... Error encounter while reading.\n");

		fprintf(stderr,error_message);
		if (fLog!=NULL) fprintf(fLog,error_message);

                if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT)
                    exit(205);
	}
	return ret;
}
size_t FWRITE(const void *ptr,size_t size,size_t nitems,FILE *stream)
{
	size_t ret;
	char error_message[1024];

	if (ptr==NULL)
		programmer_error("NULL data buffer passed to FWRITE.\n");
	if (stream==NULL)
		programmer_error("NULL file pointer passed to FWRITE.\n");
	ret=fwrite(ptr,size,nitems,stream);
	if (ret!=nitems)
	{
		sprintf(error_message,
			"*******  ERROR writing file! *******\n"
			"*\n"
			"*   This program tried to write %i bytes to\n"
			"* the stream 0x%x.  This writing failed, because:\n",
			(int) size*nitems,(int)stream);
		fprintf(stderr,error_message);
		if (fLog!=NULL) fprintf(fLog,error_message);

		perror(NULL);
		sprintf(error_message,
			"* The most common cause of file write errors\n"
			"* is running out of disk space.  Try using the\n"
			"*'df -k' command to check the amount of free space\n"
			"* on your current volume.  Many of ASF's utilities\n"
			"* require very large amounts of disk space.\n"
			"**   Program terminating... error during file write.\n");
		fprintf(stderr,error_message);
		if (fLog!=NULL)
		  fprintf(fLog,error_message);

                /* write errors we will still make fatal */
                exit(206);
	}
	return ret;
}

int FSEEK(FILE *stream,int offset,int ptrname)
{
	int ret;
	if (stream==NULL)
		programmer_error("NULL file pointer passed to FSEEK.\n");
	ret=fseek(stream,offset,ptrname);
	if (ret==-1)
		programmer_error("Stream passed to FSEEK is not seekable.\n");
	return ret;
}

int FSEEK64(FILE *stream,long long offset,int ptrname)
{
	int ret;
	if (stream==NULL)
		programmer_error("NULL file pointer passed to FSEEK64.\n");

#if defined(irix)
	ret=fseek64(stream,offset,ptrname);
#elif defined(win32)
	/* Although there is a man page for fseeko64 on Windows cygwin
	 * it appears that 64 bit ops are not yet supported there */
	ret=fseek(stream,(long)offset,ptrname);
#else
	ret=fseeko64(stream,offset,ptrname);
#endif
	if (ret==-1)
	{
		fprintf(stderr, "The file offset passed is: %lli\n", offset);
		if (fLog!=NULL)
		   fprintf(fLog, "The file offset passed is: %lli\n", offset);

                if (caplib_behavior_on_error == BEHAVIOR_ON_ERROR_ABORT)
		    programmer_error("Stream passed to FSEEK64 is not seekable.\n");
                else {
                    fprintf(stderr, "Stream passed to FSEEK64 is not seekable.\n");
		    if (fLog!=NULL)
		        fprintf(fLog, "Stream passed to FSEEK64 is not seekable.\n");
                }
	}
	return ret;
}

long long FTELL64(FILE *stream)
{
	long long ret;
	if (stream==NULL)
		programmer_error("NULL file pointer passed to FTELL64.\n");
#if defined(irix)
	ret=(long long)ftell64(stream);
#elif defined(win32)
	 /* Although there is a man page for ftello64 on Windows cygwin,
	  * it appears that 64 bit ops are nont yet supported there */
	ret=(long long)ftell(stream);
#else
	ret=ftello64(stream);
#endif
	if (ret==-1)
		programmer_error("Stream passed to FTELL64 is not seekable.\n");
	return ret;
}

int FCLOSE(FILE *stream)
{
	return fclose(stream);
}
int FFLUSH(FILE *stream)
{
	return fflush(stream);
}

// Some systems don't seem to have "strdup" ... here we define
// one for ourselves -- also, it goes through our checked-malloc,
// MALLOC, so now all memory is really checked, plus if we ever
// add mem-leak detection in MALLOC we can trace strdup's usage
// as well.
char *STRDUP (const char *s)
{
  char *result;

  if (s != NULL) {
    result = MALLOC (sizeof (char) * (strlen (s) + 1));

    int idx = 0;
    while ( s[idx] != '\0') {
      result[idx] = s[idx];
      idx++;
    }

    result[idx] = '\0';
  }
  else {
    result = NULL;
  }

  return result;
}


