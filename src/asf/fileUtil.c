/* A collection of file I/O utilities.  Primarily deals with file
   naming.  */

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

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
  char *ext;
  int ii;
  ii = strlen(name) - 1;		/* Start at end of name.  */
  while ( (ii>0) && (name[ii] != '.') && !IS_DIR_SEPARATOR(name[ii]) ) {
    /* Work backwards until we hit a directory separator or extension
       separator.*/
    ii--;
  }
  if ( (ii>0) && (name[ii]=='.') ) {
    /* We found an extension! (maybe) */
    ext = (char *) &name[ii];
    if (strcmp_case(ext, ".META") == 0 ||
	strcmp_case(ext, ".DDR") == 0 ||
	strcmp_case(ext, ".IMG") == 0 ||
	strcmp_case(ext, ".DEM") == 0 ||
	strcmp_case(ext, ".TIF") == 0 ||
	strcmp_case(ext, ".TIFF") == 0 ||
	strcmp_case(ext, ".JPG") == 0 ||
	strcmp_case(ext, ".JPEG") == 0 ||
	strcmp_case(ext, ".PGM") == 0 ||
	strcmp_case(ext, ".CFG") == 0 ||
	strcmp_case(ext, ".D") == 0 ||
	strcmp_case(ext, ".L") == 0)
      return (char *) &name[ii];
    else
      return NULL;
  }
  else
    /* We couldn't find an extension.  */
    return NULL;
}

/* Returns newly allocated string that is the input string with
   any extension removed */
char *stripExt(const char *in)
{
  char *out = STRDUP(in);
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

  /* Weird semantics: if newExt is NULL, we just return a copy of name
     without any clipping.  If nothing uses this it should be removed.  */
  if ( newExt == NULL )
    return ret;

  ext = findExt (ret);
  if ( ext != NULL )
    /* We found an existing dot extension, clip it off (the dot itself
       is clipped).  */
    *ext = '\0';

  /* Put new extension on the end.  */
  strcat (ret, newExt);

  return ret;
}

char * appendToBasename(const char *inFile, const char *suffix)
{
  char *ret = MALLOC(sizeof(char)*(strlen(inFile)+strlen(suffix)+5));
  strcpy(ret, inFile);

  /* find the extension */
  char *p = findExt(ret);

  if (p && p != ret) {
    /* we have an extension - make a copy of it, then add the
       suffix, then add back on the extension again */
    char *ext;
    *p++ = '\0';
    ext = STRDUP(p);
    strcat(ret, suffix);
    strcat(ret, ".");
    strcat(ret, ext);
    free(ext);
  } else {
    /* no extension found - just add the suffix */
    strcat(ret, suffix);
  }

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

/* Append an extension if it's not already there. Case insensitive. */
void append_ext_if_needed(char *file_name, const char *newExt,
                          const char *alsoAllowedExt)
{
  char *ext, extuc[1024];
  char new1uc[1024], new2uc[1024];
  int ext_exists;

  ext = findExt(file_name);
  ext_exists = (ext) ? TRUE : FALSE;
  if (ext_exists)
    strcpy(extuc, uc(ext));

  if (ext_exists) {
    if (newExt != NULL) {
      strcpy(new1uc,uc(newExt));
      if (strcmp(extuc, new1uc)==0) {
        return;
      }
    }
    if (alsoAllowedExt != NULL) {
      strcpy(new2uc,uc(alsoAllowedExt));
      if (strcmp(extuc, new2uc)==0) {
        return;
      }
    }
  }

  // If we haven't returned yet, we need to apply the extension
  strcat(file_name, newExt);
}

// Strip of known extension and return file name with band extension added
void append_band_ext(char *inFile, char *outFile, char *bandExt)
{
  char *ext, *base_name;

  base_name = (char *) MALLOC(sizeof(char)*255);

  ext = findExt(inFile);

  if (ext && 
      (strcmp_case(ext, ".IMG") == 0 ||
       strcmp_case(ext, ".TIF") == 0 ||
       strcmp_case(ext, ".TIFF") == 0 ||
       strcmp_case(ext, ".JPG") == 0 ||
       strcmp_case(ext, ".JPEG") == 0 ||
       strcmp_case(ext, ".PGM") == 0))
    base_name = stripExt(inFile);
  else
    strcpy(base_name, inFile);
  if (bandExt)
    sprintf(outFile, "%s_%s", base_name, bandExt);
  else
    strcpy(outFile, base_name);
  if (ext)
    strcat(outFile, ext);
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
  while ((ii>0) && !IS_DIR_SEPARATOR(inString[ii])) {
    ii--;
  }

  /* ii could be -1, if the input string was empty */
  if (ii < 0) ii = 0;

  if (IS_DIR_SEPARATOR(inString[ii])) {
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
int split_base_and_ext(char *fileName, int side, char separator,
		       char *baseName, char *extension)
{
   int ii;

   if (side == APPENDED_EXTENSION) {
      /* Work backwards until we hit an extension separator. */
      for (ii=strlen(fileName)-1; (ii>0) && (fileName[ii]!=separator); ii--) ;
   }
   else if (side == PREPENDED_EXTENSION) {
      /* Work forwards until we hit an extension separator. */
      for (ii=0; (ii<strlen(fileName)) && (fileName[ii]!=separator); ii++) ;
   }
   else {
      strcpy(baseName,fileName);
      strcpy(extension, "");
      return FALSE;
   }

   if ((fileName[ii]==separator) && (side==APPENDED_EXTENSION)) {
      strcpy(baseName, fileName);
      baseName[ii] = '\0';
      strcpy(extension, &fileName[ii]);
      return TRUE;
   }
   if ((fileName[ii]==separator) && (side==PREPENDED_EXTENSION)) {
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
        if (errno) asfPrintStatus("Error %d: %s\n", strerror(errno));
        asfPrintStatus("Tried to open: %s\n", openName);


	fprintf(stderr,
			"********************** Error! ***********************\n"
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

void fileRename(const char *src, const char *dst)
{
    // FIXME
    // asfSystem("mv %s %s", src, dst);
    rename(src, dst);
}

void renameImgAndMeta(const char *src, const char *dst)
{
    char * src_meta_file = appendExt(src, ".meta");
    char * src_img_file = appendExt(src, ".img");

    char * dst_meta_file = appendExt(dst, ".meta");
    char * dst_img_file = appendExt(dst, ".img");

    fileRename(src_meta_file, dst_meta_file);
    fileRename(src_img_file, dst_img_file);

    free(src_meta_file);
    free(src_img_file);

    free(dst_meta_file);
    free(dst_img_file);
}

/* returns a newly allocated string, strips off any extension
   and directory info */
char *
get_basename(const char *in)
{
   char *dir = MALLOC(sizeof(char)*(strlen(in)+2));
   char *file = MALLOC(sizeof(char)*(strlen(in)+2));
   split_dir_and_file(in,dir,file);
   free(dir);
   char *ext=findExt(file);
   if (ext) *ext = '\0';
   return file;
}

/* returns a newly allocated string, returns just the directory portion.
   returns an empty string if no directory info is present.
   the returned string will be terminated with the directory separator */
char *
get_dirname(const char *in)
{
   char *dir = MALLOC(sizeof(char)*(strlen(in)+2));
   char *file = MALLOC(sizeof(char)*(strlen(in)+2));
   split_dir_and_file(in,dir,file);
   free(file);
   return dir;
}

/* returns a newly allocated string, returns just the filename portion. */
char *
get_filename(const char *in)
{
   char *dir = MALLOC(sizeof(char)*(strlen(in)+2));
   char *file = MALLOC(sizeof(char)*(strlen(in)+2));
   split_dir_and_file(in,dir,file);
   free(dir);
   return file;
}


// create a directory. acts like 'mkdir -p'. return 0 on success, -1 on fail.
int
create_dir(const char *dir)
{
  int keep_going=TRUE;
  int ret=-1;
  char *ptr1, *ptr2;
  char *dir_tmp = (char*)MALLOC((strlen(dir)+1)*sizeof(char));

  // big list of S_*'s equivalent to mode = 0777
  int mode =   S_IRUSR | S_IWUSR | S_IXUSR
             | S_IRGRP | S_IWGRP | S_IXGRP
             | S_IROTH | S_IWOTH | S_IXOTH;

  ptr1 = strcpy(dir_tmp, dir);

  do {
    if ((ptr2=strpbrk(ptr1,DIR_SEPARATOR_STR)) != NULL) {
      *ptr2 = '\0';
      ptr1 = ptr2+1;
    }
    else {
      keep_going = FALSE;
    }
    ret = mkdir(dir_tmp, mode);
    if (keep_going) {
      *ptr2 = DIR_SEPARATOR;
    }
  } while (keep_going);

  return ret;
}

/* create a directory - deleting any existing one first */
int
create_clean_dir(const char *dir)
{
    remove_dir(dir);
    return create_dir(dir);
}

/* dirwalk:
 * from "The C Programming Language" 2nd edition by Kernigan & Ritchie. pg. 182
 * Apply fcn to all files in dir */
static void dirwalk(const char *dir, int (*fcn)(const char*))
{
  char name[1024];
  struct dirent *dp;
  DIR *dfd;

  if ((dfd = opendir(dir)) == NULL) {
    asfPrintWarning("dirwalk: cannot open %s\n",dir);
    return; // error
  }
  while ((dp = readdir(dfd)) != NULL) {
    if (strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0) {
      continue;
    }
    if (strlen(dir)+strlen(dp->d_name)+2 > sizeof(name)) {
      asfPrintWarning("dirwalk: name %s/%s exceeds buffersize.\n",
                      dir, dp->d_name);
      return; // error
    }
    else {
      sprintf(name, "%s/%s", dir, dp->d_name);
      (*fcn)(name);
    }
  }
  closedir(dfd);
}

// remove a directory, and everything in it (return 0 on success, -1 on fail)
int
remove_dir(const char *name)
{
  struct stat stbuf;

  if (stat(name, &stbuf) == -1) {
//    asfPrintWarning("remove_dir: cannot access %s\n", name);
    return -1; // error
  }
  if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
    dirwalk(name, remove_dir);
  }
  return remove(name);
}

char *
getPath(const char *in)
{
  char *dir = malloc(sizeof(char)*(strlen(in) + 2));
  char *file = malloc(sizeof(char)*(strlen(in) + 2));

  split_dir_and_file(in, dir, file);
  free(file);

  if (dir[strlen(dir) - 1] == DIR_SEPARATOR)
      dir[strlen(dir) - 1] = '\0';

  return dir;
}
