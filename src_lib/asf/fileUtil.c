/* A collection of file I/O utilities.  Primarily deals with file
   naming.  */

#include <assert.h>

#include "asf.h"

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
  while ( (i>0) && (name[i] != '.') && (name[i] != '/') ) {
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

char *appendExt(const char *name, const char *newExt)
{
  char *ret = (char *) MALLOC (sizeof(char) 
			       * (MAX_APPENDEXT_RESULT_STRING_LENGTH + 1));
  assert (strlen (name) <= MAX_APPENDEXT_RESULT_STRING_LENGTH);
  strcpy (ret, name);
	
  char *ext = findExt (ret);

  /* Weird semantics: if newExt is NULL, we just return a copy of name
     without any clipping.  If nothing uses this it should be removed.  */
  if ( newExt == NULL )
    return ret;

  if ( ext != NULL )
    /* We found an existing dot extension, clip it off (the dot itself
       it clipped).  */
    *ext = '\0';	

  /* Put new extension on the end.  */
  assert (strlen (ret) + strlen (newExt) 
	  <= MAX_APPENDEXT_RESULT_STRING_LENGTH);
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





