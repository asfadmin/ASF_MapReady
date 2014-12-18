#include "asf_vector.h"
#include "asf.h"
#include "asf_meta.h"
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   %s <directory> <outFile>\n", name);
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   inFile    Directory\n"
   "   outFile   Basename of the output shapefile\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program aggregates the shape files in a directory into one shape "
   "file.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

static void files_from_dir(const char *path, char ***files, int *fileCount)
{
  struct stat statbuf;
  struct dirent *dp;
  char file[1024];
  int ii, count = 0;

  DIR *dir = opendir(path);
  while ((dp = readdir(dir)) != NULL) {
    if (dp->d_name && strlen(dp->d_name)) {
      sprintf(file, "%s%c%s", path, DIR_SEPARATOR, dp->d_name);
      if (stat(file, &statbuf) < 0)
        continue;
      else if (S_ISREG(statbuf.st_mode) && 
        strcmp_case(findExt(dp->d_name), ".shp") == 0)
        count++;
    }
    else
      continue;
  }
  closedir(dir);

  char **inFiles = (char **) MALLOC(sizeof(char *)*count);
  for (ii=0; ii<count; ii++)
    inFiles[ii] = (char *) MALLOC(sizeof(char)*1024);

  ii = 0;
  dir = opendir(path);
  while ((dp = readdir(dir)) != NULL) {
    if (dp->d_name && strlen(dp->d_name)) {
      sprintf(file, "%s%c%s", path, DIR_SEPARATOR, dp->d_name);
      if (stat(file, &statbuf) < 0)
        continue;
      else if (S_ISREG(statbuf.st_mode) && 
        strcmp_case(findExt(dp->d_name), ".shp") == 0) {
        strcpy(inFiles[ii], file);
        ii++;
      }
    }
    else
      continue;
  }
  closedir(dir);

  *files = inFiles;
  *fileCount = count;
}

int main(int argc, char **argv)
{
  int ii, jj, kk, fileCount, width, decimals, nEntities, pointType, nVertices;
  double *x=NULL, *y=NULL;  
  char **files, directory[512], outFile[512], fieldName[255];
  extern int currArg; // Pre-initialized to 1
  DBFHandle dbaseIn, dbaseOut;
  SHPHandle shapeIn, shapeOut;
  DBFFieldType fieldType;
  SHPObject *shapeObject = NULL;

  // Parse command line
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }
  strcpy(directory, argv[currArg]);
  strcpy(outFile, argv[currArg+1]);

  asfSplashScreen(argc, argv);

  // Read files in directory
  files_from_dir(directory, &files, &fileCount);
    
  // Setup output shapefile
  char *dbaseFile = stripExt(outFile);
  dbaseOut = DBFCreate(dbaseFile);
  if (!dbaseOut)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);
  FREE(dbaseFile);
  dbaseFile = appendExt(files[0], ".dbf");
  dbaseIn = DBFOpen(dbaseFile, "rb");
  if (dbaseIn == NULL)
    asfPrintError("Could not open database file '%s'\n", dbaseFile);
  FREE(dbaseFile);
  int field_count = DBFGetFieldCount(dbaseIn);
  for (ii=0; ii<field_count; ii++) {
    fieldType = DBFGetFieldInfo(dbaseIn, ii, fieldName, &width, &decimals);
    if (DBFAddField(dbaseOut, fieldName, fieldType, width, decimals) == -1)
      asfPrintError("Could not add '%s' field to database file\n", fieldName);
  }
  DBFClose(dbaseIn);
  DBFClose(dbaseOut);
  shapeOut = SHPCreate(outFile, SHPT_POLYGON);
  SHPClose(shapeOut);
  
  // Copy projection files
  char *inProj = appendExt(files[0], ".prj");
  char *outProj = appendExt(outFile, ".prj");
  fileCopy(inProj, outProj);
  FREE(inProj);
  FREE(outProj);

  // Add input shapes to output file
  int nValue;
  double fValue;
  char *sValue;
  open_shape(outFile, &dbaseOut, &shapeOut);
  for (ii=0; ii<fileCount; ii++) {
    asfPrintStatus("Aggregate information from %s ...\n", files[ii]);
    open_shape(files[ii], &dbaseIn, &shapeIn);
    SHPGetInfo(shapeIn, &nEntities, &pointType, NULL, NULL);
    for (jj=0; jj<nEntities; jj++) {
      shapeObject = SHPReadObject(shapeIn, jj);
      nVertices = shapeObject->nVertices;
      x = (double *) MALLOC(sizeof(double)*nVertices);
      y = (double *) MALLOC(sizeof(double)*nVertices);
      for (kk=0; kk<nVertices; kk++) {
        x[kk] = shapeObject->padfX[kk];
        y[kk] = shapeObject->padfY[kk];
      }
      SHPDestroyObject(shapeObject);
  
      // Writing attributes into output file
      field_count = DBFGetFieldCount(dbaseIn);
      for (kk=0; kk<field_count; kk++) {
        fieldType = DBFGetFieldInfo(dbaseIn, kk, fieldName, &width, &decimals);
        if (fieldType == FTString) {
          sValue = (char *) MALLOC(sizeof(char)*(width+1));
          strcpy(sValue, DBFReadStringAttribute(dbaseIn, jj, kk));
          DBFWriteStringAttribute(dbaseOut, ii, kk, sValue);
          FREE(sValue);
        }
        else if (fieldType == FTInteger) {
          nValue = DBFReadIntegerAttribute(dbaseIn, jj, kk);
          DBFWriteIntegerAttribute(dbaseOut, ii, kk, nValue);
        }
        else if (fieldType == FTDouble) {
          fValue = DBFReadDoubleAttribute(dbaseIn, jj, kk);
          DBFWriteDoubleAttribute(dbaseOut, ii, kk, fValue);
        }
      }
      shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, nVertices, x, y, NULL);
      SHPWriteObject(shapeOut, -1, shapeObject);
      SHPDestroyObject(shapeObject);
      FREE(x);
      FREE(y);    
    }
    close_shape(dbaseIn, shapeIn);
    FREE(files[ii]);      
  }
  FREE(files);
  close_shape(dbaseOut, shapeOut);

  return(0);
}
