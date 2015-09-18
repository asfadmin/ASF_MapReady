/* A collection of file I/O utilities.  Primarily deals with file
   naming.  */

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#include "asf.h"

void removeImgAndMeta(const char *base)
{
    char * meta_file = appendExt(base, ".meta");
    char * img_file = appendExt(base, ".img");
    char * hdr_file = appendExt(base, ".hdr");

    if (fileExists(meta_file))
      remove_file(meta_file);
    if (fileExists(img_file))
      remove_file(img_file);
    if (fileExists(hdr_file))
      remove_file(hdr_file);

    FREE(meta_file);
    FREE(img_file);
    FREE(hdr_file);
}

void copyImgAndMeta(const char *src, const char *dst)
{
    char * src_meta_file = appendExt(src, ".meta");
    char * src_img_file = appendExt(src, ".img");

    char * dst_meta_file = appendExt(dst, ".meta");
    char * dst_img_file = appendExt(dst, ".img");

    fileCopy(src_meta_file, dst_meta_file);
    fileCopy(src_img_file, dst_img_file);

    free(src_meta_file);
    free(src_img_file);

    free(dst_meta_file);
    free(dst_img_file);
}

int extExists(const char *name, const char *newExt)
{
  char *fName = appendExt (name,newExt);
  int exists = fileExists (fName);
  free (fName);
  return exists;
}

long long fileSize(const char *name)
{
  FILE *fp = FOPEN(name,"rb");
  FSEEK64(fp,0,SEEK_END);
  long long file_size = FTELL64(fp);
  fclose(fp);
  return file_size;
}

int numFiles(const char *path)
{
  struct stat statbuf;
  struct dirent *dp;
  DIR *dir = NULL;
  int fileCount = 0;

  if (path && strlen(path) > 0) {
    dir = opendir(path);
  }
  else {
    dir = opendir(".");
  }

  while ((dp = readdir(dir)) != NULL) {
    char file[1024];
    if (dp->d_name && strlen(dp->d_name)) {
      sprintf(file, "%s%c%s", path, DIR_SEPARATOR, dp->d_name);
      if (stat(file, &statbuf) < 0) {
        continue;
      }
      else if (S_ISREG(statbuf.st_mode)) {
        fileCount++;
      }
    }
    else {
      continue;
    }
  }

  closedir(dir);
  return fileCount;
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
  ii = strlen(name) - 1;        /* Start at end of name.  */
  while ( (ii>0) && (name[ii] != '.') && !IS_DIR_SEPARATOR(name[ii]) ) {
    /* Work backwards until we hit a directory separator or extension
       separator.*/
    ii--;
  }
  if ( (ii>0) && (name[ii]=='.') ) {
    /* We found an extension! (maybe) */
    ext = (char *) &name[ii];
    if (strcmp_case(ext, ".META") == 0      ||
        strcmp_case(ext, ".ENVI") == 0      ||
	    strcmp_case(ext, ".BIN") == 0       || // PolSARPro extension (and .bin.hdr)
	    strcmp_case(ext, ".HDR") == 0       ||
        strcmp_case(ext, ".ESRI") == 0      ||
        strcmp_case(ext, ".DDR") == 0       ||
        strcmp_case(ext, ".IMG") == 0       ||
        strcmp_case(ext, ".DEM") == 0       ||
        strcmp_case(ext, ".TIF") == 0       ||
        strcmp_case(ext, ".TIFF") == 0      ||
        strcmp_case(ext, ".JPG") == 0       ||
        strcmp_case(ext, ".JPEG") == 0      ||
        strcmp_case(ext, ".PNG") == 0       ||
        strcmp_case(ext, ".PGM") == 0       ||
        strcmp_case(ext, ".PPM") == 0       ||
        strcmp_case(ext, ".CFG") == 0       ||
        strcmp_case(ext, ".CSV") == 0       ||
        strcmp_case(ext, ".CPX") == 0       ||
        strcmp_case(ext, ".KML") == 0       ||
        strcmp_case(ext, ".LUT") == 0       ||
        strcmp_case(ext, ".IN") == 0        ||
        strcmp_case(ext, ".LDR") == 0       ||
        strcmp_case(ext, ".RAW") == 0       ||
        strcmp_case(ext, ".000") == 0       ||
        strcmp_case(ext, ".001") == 0       ||
        strcmp_case(ext, ".AIRSAR") == 0    ||
        strcmp_case(ext, ".VVI2") == 0      || // an airsar extension
        strcmp_case(ext, ".DEMI2") == 0     || // an airsar extension
        strcmp_case(ext, ".DATGR") == 0     || // an airsar extension
        strcmp_case(ext, ".CORGR") == 0     || // an airsar extension
        //strcmp_case(ext, ".INCGR") == 0     || // an airsar extension
        strcmp_case(ext, ".D") == 0         ||
        strcmp_case(ext, ".L") == 0         ||
        strcmp_case(ext, ".BIL") == 0       ||
        strcmp_case(ext, ".BSQ") == 0       ||
        strcmp_case(ext, ".PAR") == 0       ||
        strcmp_case(ext, ".PAL") == 0       ||
        strcmp_case(ext, ".SHP") == 0       || // ArcGIS
        strcmp_case(ext, ".SHX") == 0       || // ArcGIS
        strcmp_case(ext, ".DBF") == 0       || // ArcGIS
        strcmp_case(ext, ".PRJ") == 0       || // ArcGIS
        strcmp_case(ext, ".KML") == 0       ||
        strcmp_case(ext, ".PI") == 0        ||
        strcmp_case(ext, ".TAR") == 0       ||
        strcmp_case(ext, ".NUL") == 0       ||
        strcmp_case(ext, ".VOL") == 0       ||
        strcmp_case(ext, ".CSV") == 0       ||
        strcmp_case(ext, ".LOG") == 0				||
        strcmp_case(ext, ".BRS") == 0       ||
        strcmp_case(ext, ".AMP") == 0       || // roipac
        strcmp_case(ext, ".COR") == 0       || // roipac
        strcmp_case(ext, ".INT") == 0       || // roipac
        strcmp_case(ext, ".RSC") == 0       || // roipac
	strcmp_case(ext, ".NC") == 0        || // netCDF
	strcmp_case(ext, ".H5") == 0        || // HDF5
	strcmp_case(ext, ".CC") == 0        || // gamma
	strcmp_case(ext, ".SM") == 0        || // gamma
	strcmp_case(ext, ".DIFF") == 0      || // gamma
	strcmp_case(ext, ".XML") == 0       ||
	strcmp_case(ext, ".ANN") == 0       || // UAVSAR annotation file
	strcmp_case(ext, ".GRD") == 0       || 
	strcmp_case(ext, ".MLC") == 0       || 
	strcmp_case(ext, ".DAT") == 0       || 
	strcmp_case(ext, ".SPECS") == 0     ||
	strcmp_case(ext, ".FLT") == 0       || // Gridfloat
	strcmp_case(ext, ".HGT") == 0       || // SRTM JPL height
	strcmp_case(ext, ".DIS") == 0       || // Seasat processing
	strcmp_case(ext, ".HDF") == 0       || // RGPS Eularian products
	strcmp_case(ext, ".ZIP") == 0)

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

  /* if the final characters is a dot, trim it */
  if (ret[strlen(ret)-1] == '.')
      ret[strlen(ret)-1] = '\0';

  /* Allow passing in an empty string as "newExt" in order to strip the ext */
  if (strlen(newExt)>0) {
    /* if the new extension doesn't begin with ".", add one */
    if (newExt[0] != '.')
      strcat(ret, ".");

    /* Put new extension on the end.  */
    strcat (ret, newExt);
  }

  return ret;
}

char * appendToBasename(const char *inFile, const char *suffix)
{
  /* adding a lot of padding here... sometimes returned values get
     passed to append_ext_if_needed()... */
  char *ret = MALLOC(sizeof(char)*(strlen(inFile)+strlen(suffix)+20));
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
  } else if (ret[strlen(ret)-1]=='.') {
    /* handle the "." case specially... */
    ret[strlen(ret)-1]='\0';
    strcat(ret, suffix);
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

  ext = findExt(file_name);
  if (ext) {
    strcpy(extuc, uc(ext));
    if (newExt) {
      strcpy(new1uc,uc(newExt));
      if (strcmp(extuc, new1uc)==0) {
        return;
      }
    }
    if (alsoAllowedExt) {
      strcpy(new2uc,uc(alsoAllowedExt));
      if (strcmp(extuc, new2uc)==0) {
        return;
      }
    }
  }

  // If we haven't returned yet, we need to apply the extension

  // don't append any "." if one is already there
  const char *p = newExt;
  if (file_name[strlen(file_name)-1] == '.')
      ++p;

  // if the basename doesn't end with a "." and the given extension
  // doesn't have a leader ".", we will add one ourselves
  if (file_name[strlen(file_name)-1] != '.' && p[0] != '.')
      strcat(file_name, ".");

  // now can append given extension
  strcat(file_name, p);
}

// Strip of known extension and return file name with band extension added
void append_band_ext(char *inFile, char *outFile, char *bandExt)
{
  char *ext, *base_name;

  ext = findExt(inFile);

  if (ext &&
      (strcmp_case(ext, ".IMG") == 0 ||
       strcmp_case(ext, ".TIF") == 0 ||
       strcmp_case(ext, ".TIFF") == 0 ||
       strcmp_case(ext, ".JPG") == 0 ||
       strcmp_case(ext, ".JPEG") == 0 ||
       strcmp_case(ext, ".PNG") == 0 ||
       strcmp_case(ext, ".PGM") == 0 ||
       strcmp_case(ext, ".BIN") == 0 ||
       strcmp_case(ext, ".NC") == 0 ||
       strcmp_case(ext, ".H5") == 0))
    base_name = stripExt(inFile);
  else {
    base_name = (char *) MALLOC(sizeof(char)*255);
    strcpy(base_name, inFile);
  }
  if (bandExt)
    sprintf(outFile, "%s_%s", base_name, bandExt);
  else
    sprintf(outFile, "%s", base_name);
  if (ext)
    strcat(outFile, ext);
  free(base_name);
}

/****************************************************************************
 * split_dir_and_file:
 * Takes a string and fills one pre-allocated array with any path prior to
 * the file name, and fills another pre-allocated array with the file name
 * The path name will include a trailing directory separator
 * The path name will be the empty string if there was no path in the input
 */
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
      else if ( ((0==strcmp(ext,".D"))||(0==strcmp(ext,".L"))) &&
          (access[0]=='w' || access[0]=='a'))
        ext[0]=0;/*Clip off stupid extention-- will append .img later*/
    }

/*Find the file's actual name, with the correct extension.*/
    if (access[0]=='w' || access[0]=='a')
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
        if (errno) {
            asfPrintStatus("Errno %d: %s\n",
                           errno, strerror(errno));
        }
        asfPrintStatus("Tried to open: %s\n",
             (openName != NULL && strlen(openName)) ? openName : "invalid filename");


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


// had to reduce the buffer size even further, getting crashes with
// the 1mb buffer on Windows Vista
//#define BUFFER_SIZE 16777216 /* 16 megabyte buffer */
//#define BUFFER_SIZE 1048576 /* 1 megabyte buffer */
#define BUFFER_SIZE 262144 /* 250k buffer */
/******************************************************************************
 * fileCopy:
 * Copy the file specified by "src" to the file specified by "dst". Error
 * checking is taken care of by the caplib functions (ie FOPEN, ASF_FWRITE, etc) */
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
         int a = fwrite( buffer, sizeof(char), amount, dstFp );
	 if (a<amount) {
	   asfPrintError("Error copying file %s -> %s\n"
			 "Only %d of %d bytes written.\n%s\n",
			 src, dst, a, amount, strerror(errno));
	 }
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
    if (fileExists(src) && fileExists(dst))
      unlink(dst);
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

// check to see if a string is an existing directory
int
is_dir(const char *dir)
{
  DIR *tmp = opendir(dir);
  if(tmp) {
    closedir(tmp);
    return TRUE;
  }
  else
    return FALSE;
}

// create a directory. acts like 'mkdir -p'. return 0 on success, -1 on fail.
int
create_dir(const char *dir)
{
  int keep_going=TRUE;
  int ret=-1;
  char *ptr1, *ptr2;
  char *dir_tmp = (char*)MALLOC((strlen(dir)+1)*sizeof(char));

#ifndef mingw
  // big list of S_*'s equivalent to mode = 0777
  int mode =   S_IRUSR | S_IWUSR | S_IXUSR
             | S_IRGRP | S_IWGRP | S_IXGRP
             | S_IROTH | S_IWOTH | S_IXOTH;
#else
   // Windows path length limit: 260 chars
   // This is supposed to be defined as MAX_PATH (which is 260)
   // However it looks like the actual limit is 247... not sure why
   if (strlen(dir) >= 247) {
      asfPrintWarning("Path too long:\n%s\n", dir);
      return -1;
   }
#endif

  ptr1 = strcpy(dir_tmp, dir);

  do {
    if ((ptr2=strpbrk(ptr1,DIR_SEPARATOR_STR)) != NULL) {
      *ptr2 = '\0';
      ptr1 = ptr2+1;
    }
    else {
      keep_going = FALSE;
    }
    if (strlen(dir_tmp) > 0) {
#ifdef mingw
      ret = mkdir(dir_tmp);
#else
      ret = mkdir(dir_tmp, mode);
#endif
      if (ret != 0 && errno != EEXIST) {
        asfPrintWarning("create_dir failed to create: %s\nErr: %d %s\n",
                        dir_tmp, errno, strerror(errno));
      }
    }
    if (keep_going) {
      *ptr2 = DIR_SEPARATOR;
    }
  } while (keep_going);

  FREE(dir_tmp);
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
      closedir(dfd);
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
  if (is_dir(name))
    dirwalk(name, remove_dir);
  return remove_file(name);
}

char *
getPath(const char *in)
{
  char *dir = malloc(sizeof(char)*(strlen(in) + 2));
  char *file = malloc(sizeof(char)*(strlen(in) + 2));

  split_dir_and_file(in, dir, file);
  free(file);

  // On Windows, allow / as a separator, too, since cygwin does
  if (strlen(dir) > 0) {
    if (dir[strlen(dir) - 1] == DIR_SEPARATOR || dir[strlen(dir) - 1] == '/')
      dir[strlen(dir) - 1] = '\0';
  }

  return dir;
}

const char *bin_postfix()
{
#ifdef win32
    return ".exe";
#else
    return "";
#endif
}

int remove_file(const char *file)
{
  if (is_dir(file)) {
    int ret = rmdir(file);
    if (ret < 0) {
      asfPrintWarning("Could not remove directory '%s': %s\n",
                      file, strerror(errno));
    }
    if (is_dir(file)) {
      asfPrintWarning("Failed to remove directory '%s'\n");
    }
    return ret;
  }
  else if (fileExists(file)) {
    int ret = unlink(file);
    if (ret < 0) {
      asfPrintWarning("Could not remove file '%s': %s\n",
                      file, strerror(errno));
    }
    return ret;
  }
  return 0; // success, I guess
}

void catFile(char *file)
{
  FILE *fp = FOPEN(file, "r");
  char line[1024];
  while (fgets(line, 1024, fp))
    asfPrintStatus(line);
  FCLOSE(fp);
}
