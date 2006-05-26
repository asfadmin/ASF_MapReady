/* A collection of file I/O utilities.  Primarily deals with file
   naming.  */

#include <assert.h>

#include "asf.h"

static char *
static_strdup (const char *s)
{
  char *result = malloc (sizeof (char) * (strlen (s) + 1));

  int idx = 0;
  while ( s[idx] != '\0') {
    result[idx] = s[idx];
    idx++;
  }

  result[idx] = '\0';

  return result;
}

int extExists(const char *name, const char *newExt)
{
  char *fName = appendExt (name,newExt);
  int exists = fileExists (fName);
  free (fName);
  return exists;
}

int fileExists(const char *name)
{
  FILE *f = fopen (name,"r");
  if (f == NULL)
    return 0;
  fclose (f);
  return 1;
}

char *findExt(const char *name)
{
  int i;
  i = strlen(name) -1;		/* Start at end of name.  */
  while ( (i>0) && (name[i] != '.') && (name[i] != DIR_SEPARATOR) ) {
    /* Work backwards until we hit a directory separator or extension
       separator.*/
    i--;
  }
  if ( (i>0) && (name[i]=='.') )
    /* We found an extension!  */
    return &name[i];
  else
    /* We couldn't find an extension.  */
    return NULL;
}

/* Returns newly allocated string that is the input string with
   any extension removed */
char *stripExt(const char *in)
{
  char *out = static_strdup(in);
  char *ext = findExt(out);
  if (ext) *ext = '\0';
  return out;
}

char *appendExt(const char *name, const char *newExt)
{
  char *ret, *ext;
  int l_name, l_newExt;

  l_name = strlen(name);
  l_newExt = newExt ? strlen(newExt) : 0;

  ret = (char *) MALLOC (sizeof(char) * (l_name + l_newExt + 2));
  strcpy (ret, name);

  ext = findExt (ret);

  /* Weird semantics: if newExt is NULL, we just return a copy of name
     without any clipping.  If nothing uses this it should be removed.  */
  if ( newExt == NULL )
    return ret;

  if ( ext != NULL )
    /* We found an existing dot extension, clip it off (the dot itself
       it clipped).  */
    *ext = '\0';

  /* Put new extension on the end.  */
  strcat (ret, newExt);

  return ret;
}

void create_name(char *out,const char *in,const char *newExt)
{
  char *ext;
  strcpy (out,in);
  /* If we find an existing extension, clip it off.  */
  ext = findExt (out);
  if ( ext != NULL )
    *ext = '\0';
  /* Append the new extension.  */
  strcat (out, newExt);
}

void append_ext_if_needed(char *file_name, const char *newExt, 
			  const char *alsoAllowedExt)
{
  /* modifies the given file_name string in-place */

  char *ext;
  ext = findExt(file_name);

  if (!ext || 
      (strcmp(ext, newExt) != 0 && 
       (!alsoAllowedExt || strcmp(ext, alsoAllowedExt) != 0)))
  {
    /* no current extension, or the extension is not what we would add 
       -- so add ours */
    strcat(file_name, newExt);
  }
  else
  {
    /* already has correct extension (or the alternate) 
       -- nothing to be done */    
  }

  return;
}

/****************************************************************************
 * split_dir_and_file:
 * Takes a string and fills one pre-allocated array with any path prior to
 * the file name, and fills another pre-allocated array with the file name */
void split_dir_and_file(const char *inString, char *dirName, char *fileName)
{
  int ii;

   /* Start at end of inString. */
  ii = strlen(inString) - 1;

  /* Work backwards until we hit a directory separator. */
  while ((ii>0) && (inString[ii]!=DIR_SEPARATOR)) {
    ii--;
  }

  if (inString[ii]==DIR_SEPARATOR) {
    ii++;
  }

  strcpy(dirName, inString);
  dirName[ii] = '\0';

  strcpy(fileName, &inString[ii]);
}


/*****************************************************************************
 * split_base_and_ext:
 * Fills 'extension' with the file's extension and returns TRUE if there is
 * indeed an extension. Otherwise it returns FALSE and fills 'extension' with
 * an empty string. If 'side' is PREPENDED_EXTENSION, then it looks for the
 * extension at the front of the file name. If 'side' is APPENDED_EXTENSION, it
 * looks for the extension at the end of the file name. Otherwise it returns
 * FALSE and fills 'extension' with an empty string. The extension separator
 * is a '.' It assumes 'fileName' is only the file name (no path included) */
int split_base_and_ext(char *fileName, int side, char *baseName,
                       char *extension)
{
   int ii;

   if (side == APPENDED_EXTENSION) {
      /* Work backwards until we hit an extension separator. ('.') */
      for (ii=strlen(fileName)-1; (ii>0) && (fileName[ii]!='.'); ii--) ;
   }
   else if (side == PREPENDED_EXTENSION) {
      /* Work forwards until we hit an extension separator. ('.') */
      for (ii=0; (ii<strlen(fileName)) && (fileName[ii]!='.'); ii++) ;
   }
   else {
      strcpy(baseName,fileName);
      strcpy(extension, "");
      return FALSE;
   }

   if ((fileName[ii]=='.') && (side==APPENDED_EXTENSION)) {
      strcpy(baseName, fileName);
      baseName[ii] = '\0';
      strcpy(extension, &fileName[ii]);
      return TRUE;
   }
   if ((fileName[ii]=='.') && (side==PREPENDED_EXTENSION)) {
      strcpy(baseName, &fileName[ii+1]);
      strcpy(extension, fileName);
      extension[ii+1] = '\0';
      return TRUE;
   }
   else {
      /* We couldn't find an extension. */
      strcpy(baseName,fileName);
      strcpy(extension, "");
      return FALSE;
   }
}


/*******************************************************************************
 * fopenImage:
 * first tries to open the given image name, then appends ".img" and tries again
 * It returns a pointer to the opened file.*/
FILE *fopenImage(const char *fName,const char *access)
{
	int forWriting=0;
	FILE *fRet=NULL;
	char *openName=NULL;
        /* Check to make sure that the (bone-headed) user didn't ask
           us to open a .ddr image or .meta file.  findExt may return
           a pointer into its argument string, so fName isn't const
           anymore.  */
	char *ext=findExt( (char *) fName);
	if (NULL!=ext)
	{
	  if ( (0==strcmp(ext,".ddr")) || (0==strcmp(ext,".meta")))
	    ext[0]=0;/*Clip off stupid extention-- will append .img later*/
	  else if ( ((0==strcmp(ext,".D"))||(0==strcmp(ext,".L"))) && access[0]=='w')
	    ext[0]=0;/*Clip off stupid extention-- will append .img later*/
	}

/*Find the file's actual name, with the correct extension.*/
	if (access[0]=='w')
	{/*We're opening for writing-- we have to be conservative.*/
		forWriting=1;
		if (NULL==findExt((char *)fName))/*If there is no extension,*/
			openName=appendExt(fName,".img");/*Append .img*/
		else /*There was an extension, so */
			openName=appendExt(fName,NULL);/*Do nothing to the name.*/
	}
	else
	{/*We're opening for reading-- search many extensions for the image.*/
		char *extTable[]={".img",".dem",".ht",".coh",NULL};
		int extNo=-1;
		do
		{
			char *ext=(extNo==-1)?NULL:extTable[extNo];
			if (extExists(fName,ext))
			{/*We found a file with the given basename and extension!*/
				openName=appendExt(fName,ext);
				break;/*Once we find one, we're done.*/
			}
			extNo++;
		}
		while (extTable[extNo]!=NULL);
	}
/*Try to open this name.*/
	if (NULL!=openName)
	{
		fRet=fopen(openName,access);
		if (fRet!=NULL)
		{/*We've sucessfully opened the file.*/
			free(openName);/*Free the name.*/
			return fRet;/*Return the file pointer.*/
		}
	}
/*An error occured-- tell the user and quit.*/
	fprintf(stderr,
			"********************** ERROR! ***********************\n"
			"An error occured trying to open for %s the\n"
			"file named '%s'.\n\n",forWriting?"write or create":"reading",
			fName);
	if (forWriting)
		fprintf(stderr,
			"This could be because you don't have write permissions\n"
			"in this directory, because your disk quota is full, or \n"
			"because your disk is full.\n");
	else /*for reading*/
		fprintf(stderr,
				"This could be because the file doesn't exist in this\n"
				"directory, or you don't have read permissions for the\n"
				"file.  I even searched for common image extensions.\n");
	exit(102);
	return NULL;
}


//#define BUFFER_SIZE 16777216 /* 16 megabyte buffer */
#define BUFFER_SIZE 1048576 /* 1 megabyte buffer */
/******************************************************************************
 * fileCopy:
 * Copy the file specified by "src" to the file specified by "dst". Error
 * checking is taken care of by the caplib functions (ie FOPEN, FWRITE, etc) */
void fileCopy(const char *src, const char *dst)
{
   char buffer[BUFFER_SIZE];
   FILE *srcFp;
   FILE *dstFp;
   unsigned int amount;

   srcFp = FOPEN( src, "rb" );
   dstFp = FOPEN( dst, "wb" );

   do {
      amount = fread( buffer, sizeof(char), BUFFER_SIZE, srcFp );
      if (amount) {
         fwrite( buffer, sizeof(char), amount, dstFp );
      }
   /* when amount read is < BUFSZ, copy is done */
   } while (amount == BUFFER_SIZE);

   FCLOSE(srcFp);
   FCLOSE(dstFp);
}
