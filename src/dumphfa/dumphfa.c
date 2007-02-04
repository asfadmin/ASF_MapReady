// Import an ERDAS ArcGIS GeoTIFF (a projected GeoTIFF flavor), including
// projection data from its metadata file (ERDAS MIF HFA .aux file) into
// our own ASF Tools format (.img, .meta)
//
// NOTE:
// 1. At this time, only supports Albers Equal Area Conic, Lambert Azimuthal
//    Equal Area, Lambert Conformal Conic, Polar Stereographic, and UTM
// 2. There may be some data duplication between the GeoTIFF tag contents
//    in the TIFF file and the data contents of the metadata (.aux) file.
// 3. Data and parameters found in the metadata (.aux) file supercede the
//    data and parameter values found in the TIFF file
//

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <float_image.h>
#include <spheroids.h>
#include <proj.h>
#include <libasf_proj.h>

#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"

#include "arcgis_spheroids.h"
#include "find_arcgis_geotiff_aux_name.h"
#include "dumphfa.h"

#define ARCGIS_DEFAULT_UTM_SCALE_FACTOR     0.9996
#define ARCGIS_DEFAULT_SCALE_FACTOR         1.0

#define ARCGIS_NUM_PROJDPARAMS              15
#define ARCGIS_PROJPARAMS_STATE_PLANE       0
#define ARCGIS_PROJPARAMS_USER_INDEX1       1
#define ARCGIS_PROJPARAMS_STD_PARALLEL1     2
#define ARCGIS_PROJPARAMS_STD_PARALLEL2     3
#define ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN  4
#define ARCGIS_PROJPARAMS_LAT_ORIGIN        5
#define ARCGIS_PROJPARAMS_FALSE_EASTING     6
#define ARCGIS_PROJPARAMS_FALSE_NORTHING    7
#define ARCGIS_PROJPARAMS_USER_INDEX8       8
#define ARCGIS_PROJPARAMS_USER_INDEX9       9
#define ARCGIS_PROJPARAMS_USER_INDEX10      10
#define ARCGIS_PROJPARAMS_USER_INDEX11      11
#define ARCGIS_PROJPARAMS_USER_INDEX12      12
#define ARCGIS_PROJPARAMS_USER_INDEX13      13
#define ARCGIS_PROJPARAMS_USER_INDEX14      14

#define ARCGIS_NUM_DATUMDPARAMS             7

#define ARCGIS_NAD27_DATUM                  "NAD27"
#define ARCGIS_NAD83_DATUM                  "NAD83"
#define ARCGIS_HARN_DATUM                   "HARN"
#define ARCGIS_WGS84_DATUM                  "WGS 84"

#define ARCGIS_USER_DEFINED_PCS             32767

#define UNKNOWN_PROJECTION_TYPE             -1

typedef struct {
  char sphereName[MAX_EHFA_ENTRY_NAMESTRING_LEN]; // Spheroid name
  double a; // Major axis
  double b; // Minor axis
  double eSquared; // e^2
  double radius; // Radius at equator
} arcgisSpheroidParms_t;

typedef struct {
  unsigned long proNumber; // Projection number (equates to a type, e.g. "Albers..."
  unsigned long proZone; // Only applies to UTM
  double proParams[ARCGIS_NUM_PROJDPARAMS]; // Array of 15 projection parameters
  unsigned short proType; // Index for projection type enum
  char proExeName[MAX_EHFA_ENTRY_NAMESTRING_LEN]; // Name of exe for converting between EPRJ_INTERNAL and EPRJ_EXTERNAL
  char proName[MAX_EHFA_ENTRY_NAMESTRING_LEN]; // Name of type of projection
  arcgisSpheroidParms_t proSpheroid;
} arcgisProjParms_t;

typedef struct {
  char datumname[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char gridname[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  unsigned short type;
  double params[ARCGIS_NUM_DATUMDPARAMS];
} arcgisDatumParms_t;

typedef struct {
  double x;
  double y;
} arcgisCoordinate_t;

typedef struct {
  double width;
  double height;
} arcgisSize_t;

typedef struct {
  char proName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  arcgisCoordinate_t upperLeftCenter;
  arcgisCoordinate_t lowerRightCenter;
  arcgisSize_t pixelSize;
  char units[MAX_EHFA_ENTRY_NAMESTRING_LEN];
} arcgisMapInfo_t;

typedef struct {
  char projection[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char units[MAX_EHFA_ENTRY_NAMESTRING_LEN];
} arcgisEimg_MapInformation_t;

void getArcgisProjParameters(char *infile, arcgisProjParms_t *proParms);
void getArcgisDatumParameters(char *infile, arcgisDatumParms_t *datumParms);
void getArcgisMapInfo(char *infile, arcgisMapInfo_t *arcgisMapInfo);
void getArcgisEimg_MapInformation (char *infile,
                   arcgisEimg_MapInformation_t *arcgisEimg_MapInformation);
void readArcgisEimg_MapInformation (FILE *fp, unsigned long offset,
                   arcgisEimg_MapInformation_t *arcgisEimg_MapInformation);
void DHFAGetDoubleValFromOffset(FILE *fp, unsigned long offset, double *val);
void DHFAGetDoubleVal(FILE *fp, double *val);
void DHFAGetFloatValFromOffset(FILE *fp, unsigned long offset, float *val);
void DHFAGetFloatVal(FILE *fp, float *val);
void DHFAGetStringValFromOffset(FILE *fp, unsigned long offset,
                                unsigned long strLen, char *str);
void DHFAGetStringVal(FILE *fp, unsigned long strLen, char *str);
spheroid_type_t arcgisSpheroidName2spheroid(char *sphereName);
int PCS_2_UTM (short pcs, datum_type_t *datum, unsigned long *zone);
void copy_proj_parms(meta_projection *dest, meta_projection *src);
void print_projection_parameters (arcgisProjParms_t *proParms);
void print_datum_parameters (arcgisDatumParms_t *datumParms);

int main (int argc, char *argv[]) {
  int print_dictionary = 0;
  GString *inGeotiffAuxName;
  short projection_type;
  arcgisProjParms_t arcgisProjParms;
  arcgisDatumParms_t arcgisDatumParms;
  arcgisMapInfo_t arcgisMapInfo;
  datum_type_t datum;
  int i;
  char inBaseName[256];
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
        //arcgisEimg_MapInformation_t arcgisEimg_MapInformation;
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  FILE *fp;


  /***** GET ALL VALUES THAT ARE IN THE ARCGIS METADATA FILE (.aux)  *****/
  /*     IF THE FILE EXISTS.                                             */
  arcgisProjParms.proType = 0;
  arcgisProjParms.proNumber = 0L;
  strcpy(arcgisProjParms.proExeName, MAGIC_UNSET_STRING);
  strcpy(arcgisProjParms.proName, MAGIC_UNSET_STRING);
  arcgisProjParms.proZone = 0L;
  for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++) {
    arcgisProjParms.proParams[i] = MAGIC_UNSET_DOUBLE;
  }
  strcpy(arcgisProjParms.proSpheroid.sphereName, MAGIC_UNSET_STRING);
  arcgisProjParms.proSpheroid.a = MAGIC_UNSET_DOUBLE;
  arcgisProjParms.proSpheroid.b = MAGIC_UNSET_DOUBLE;
  arcgisProjParms.proSpheroid.eSquared = MAGIC_UNSET_DOUBLE;
  arcgisProjParms.proSpheroid.radius = MAGIC_UNSET_DOUBLE;

  strcpy(arcgisDatumParms.datumname, MAGIC_UNSET_STRING);
  strcpy(arcgisDatumParms.gridname, MAGIC_UNSET_STRING);
  arcgisDatumParms.type = -1;
  for (i=0; i<ARCGIS_NUM_DATUMDPARAMS; i++) {
    arcgisDatumParms.params[i] = MAGIC_UNSET_DOUBLE;
  }

  // Get base name
  if (argc ==3) {
    if (strncmp(argv[2], "-d", 2) == 0) {
      if (argc != 3) {
        usage(NULL);
        exit (0);
      }
      strcpy(inBaseName, argv[1]);//inFileName);
      print_dictionary = 1;
    }
    else if (strncmp(argv[1], "-d", 2) == 0) {
      if (argc != 3) {
        usage(NULL);
        exit (0);
      }
      strcpy(inBaseName, argv[2]);//inFileName);
      print_dictionary = 1;
    }
  }
  else if (argc == 2) {
    strcpy(inBaseName, argv[1]);
    print_dictionary = 0;
  }
  else {
    usage(NULL);
    exit (0);
  }

  if (findExt(inBaseName)) {
    *(findExt(inBaseName)) = '\0';
  }

  inGeotiffAuxName = find_arcgis_geotiff_aux_name(inBaseName);
  if ( inGeotiffAuxName == NULL) {
    fprintf(stderr, "No ArcGIS metadata (.aux) file was found using basename %s\n", inBaseName);
    usage(NULL);
    exit (0);
  }
  if ( inGeotiffAuxName != NULL ) { // ArcGIS metadata file (.aux) exists
    fp = fopen(inGeotiffAuxName->str, "r");
    if (fp == NULL) {
      printf("\nError opening input ArcGIS metadata (.aux) file.\n");
      exit (0);
    }

    /***** Parse headers *****/
    /*                       */
    GetAuxHeader(fp, &hdr);
    asfRequire(strncmp(hdr.label, "EHFA_HEADER_TAG", 15) == 0,
               "\nArcGIS metadata (.aux) file invalid\n");
    GetDataHeader(fp, &dhdr, &hdr);
    asfRequire(dhdr.version == 1,
               "\nArcGIS metadata (.aux) file invalid\n");
    GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
    fclose(fp);

    short proj_type;
    proj_type = getArcgisProjType (inGeotiffAuxName->str);
    if (  proj_type != UTM    &&
          proj_type != ALBERS &&
          proj_type != LAMCC  &&
          proj_type != PS     &&
          proj_type != LAMAZ)
    {
      fprintf(stderr, "\nMissing or unsupported projection type found in\n"
          "ArcGIS metadata (.aux) file '%s'\n\n", inGeotiffAuxName->str);
    }
    else {
      if ( proj_type == UTM     ||
           proj_type == ALBERS  ||
           proj_type == LAMCC   ||
           proj_type == PS      ||
           proj_type == LAMAZ   )
      {
        projection_type = proj_type;
      }

      printf("\nIMAGINE type AUX file name and Header:\n");
      printf("======================================\n");
      printf("Input filename: %s\n", inGeotiffAuxName->str);
      printf("AUX file header label: %s\n", hdr.label);
      printf("AUX file data version: %ld\n\n", dhdr.version);
      // Read projection parameters from .aux file
      getArcgisProjParameters(inGeotiffAuxName->str,
                              &arcgisProjParms);
      // Try to get datum record from .aux file
      getArcgisDatumParameters(inGeotiffAuxName->str, &arcgisDatumParms);
      if (strncmp(arcgisDatumParms.datumname, ARCGIS_NAD27_DATUM, strlen(ARCGIS_NAD27_DATUM)) == 0) {
        datum = NAD27_DATUM;
      }
      else if (strncmp(arcgisDatumParms.datumname, ARCGIS_NAD83_DATUM, strlen(ARCGIS_NAD83_DATUM)) == 0) {
        datum = NAD83_DATUM;
      }
      else if (strncmp(arcgisDatumParms.datumname, ARCGIS_WGS84_DATUM, strlen(ARCGIS_WGS84_DATUM)) == 0) {
        datum = WGS84_DATUM;
      }
      else {
        fprintf(stderr,"\nMissing or unsupported datum in ArcGIS metadata (.aux) file...\n");
        // NOTE: The ArcGIS .aux file may have contained "HARN" for High Accuracy Reference Network
        // (a GPS-enhanced NAD83), but we don't separately support it at this time and I'm not sure
        // if it's OK to just call it NAD83 ...
      }
      // Read map info data from .aux file
      getArcgisMapInfo(inGeotiffAuxName->str, &arcgisMapInfo);

      printf("\nAUX file Projection Parameters:\n");
      printf("===============================\n");
      print_projection_parameters (&arcgisProjParms);

      printf("\nAUX file Datum Parameters:\n");
      printf("===============================\n");
      print_datum_parameters (&arcgisDatumParms);
      printf("\n");

      if (print_dictionary) {
        /***** Print dictionary *****/
        /*                          */
        ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);
        PrintDictionary(ddObjects, dictionary);
      }

      // And in closing...
      printf("\n");
    }
  }
  g_string_free (inGeotiffAuxName, TRUE);

  return 0;
}

/***** Returns a numeric value that represents the type of projection *****/
/*     coordinate system exists in the ArcGIS geotiff file, according     */
/*     to the ArcGIS geotiff metadata (.aux) file contents                */
/*                                                                        */
/*     Albers Equal Area Conic == 3  (ALBERS in proj.h)                   */
/*     Lambert Azimuthal Equal Area == 11  (LAMAZ in proj.h)              */
/*     Lambert Conformal Conic == 4  (LAMCC in proj.h)                    */
/*     Polar Stereographic == 6  (PS in proj.h)                           */
/*     Universal Transverse Mercator == 1  (UTM in proj.h)                */
/*                                                                        */
short getArcgisProjType(const char *auxFile) {
  short nodeFound;
  short projType;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  //arcgisEimg_MapInformation_t arcgisEimg_MapInformation;
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  unsigned long proType;
  unsigned long proNumber;
  int i;
  FILE *fp;

  fp = fopen(auxFile, "r");
  asfRequire(fp != NULL,
             "\nError opening input ArcGIS metadata (.aux) file.\n");

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp(hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "\nArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "\nArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  projType = DHFA_UNKNOWN_PROJECTION;
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_PROPARAMETERS, &foundNode); // do a get, but via a search
  if (nodeFound) {
    /* NOTE: proType is an index meaning either "EPRJ_INTERNAL" or     */
    /* "EPRJ_EXTERNAL" in an enumerated type.  The proNumber name      */
    /* however is the projection type, by numerical ID, that we want.  */
    /* NOTE: I used the Eprj_ProParameters data element naming here... */
    DHFAGetIntegerValFromOffset(fp, foundNode.data, &proType, _EMIF_T_ENUM);
    DHFAGetIntegerVal(fp, &proNumber, _EMIF_T_LONG);
    projType = (short)proNumber;
  }

  // If the Eprj_ProParameters data node was nonexistent, then the
  // Eimg_MapInformation node usually is ...try to find the projection
  // type from Eimg_MapInformation instead
  if (!nodeFound) {
    nodeFound = FindNode (fp, &rootNode, EIMG_MAPINFORMATION, &foundNode); // do a get, but via a search
    if (nodeFound) {
      // TODO: readArcgisEimg_MapInformation remains UNTESTED.
      // So far, I have not been able to locate an aux file
      // that has the Eimg_MapInformation data node in it.
      //
      // ...Just allow the projType to remain 'unknown'
      /*readArcgisEimg_MapInformation (fp, foundNode.data,
      &arcgisEimg_MapInformation);*/
    }
  }

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
  }
  fclose(fp);


  return projType;
}

/******************** local_machine_is_little_endian() ********************/
/*  COMMENTS:                                                             */
/*    - Returns non-zero if the local machine architecture/OS writes      */
/*      data to memory/disk in little-endian order, e.g. Intel format     */
/*      else returns zero                                                 */
/*    - 'testlong' is a multi-byte unsigned integer with a non-zero value */
/*      stored in its least-significant byte (LSB).  If the local machine */
/*      writes in big-endian format, then '*(unsigned char*)&testlong' (the byte  */
/*      at the lowest address in memory where 'testlong' is written) will */
/*      be zero.  But if the local machine is writing in little-endian    */
/*      format, then the bytes of 'testlong' will be in reverse order     */
/*      and the LSB will be located at the lowest byte address, e.g.      */
/*      '*(unsigned char*)&testlong' will return the LSB ...non-zero in this case */
unsigned char local_machine_is_little_endian()
{
  unsigned long testlong=1L;
  unsigned char rtn = *(unsigned char*)&testlong;

  return rtn;
}


/***** GetAuxHeader()                                          *****/
/*                                                                 */
/* GetAuxHeader() reads the HFA file header at offset 0x00 in the  */
/* file.  This header contains a pointer to the 'data header'      */
/* which in turn contains pointers to the data dictionary and the  */
/* root of the data tree stored in the file.                       */
/*                                                                 */
void GetAuxHeader(FILE *fp, _Ehfa_HeaderTag* hdr)
{
  char* pTmpUlong;
  char* pTmpUchar;

  /* Allocate temporary variables */
  pTmpUlong = MALLOC(EMIF_T_ULONG_LEN);
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Go to beginning of file */
  fseek(fp, 0L, SEEK_SET);

  /* Read file header label tag */
  DHFAGetString(fp, strlen(EHFA_HEADER_TAG)+1, hdr->label);

  /* Read offset to data header */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  hdr->headerPtr = *((unsigned long*) pTmpUlong);

  /* Clean up */
  FREE(pTmpUlong);
  FREE(pTmpUchar);
}

/***** GetDataHeader()                                         *****/
/*                                                                 */
/* GetDataHeader() reads data access and data dictionary info from */
/* the 'data header', including pointers (offsets) to free data    */
/* storage area within the file and the root entry of the tree of  */
/* data elements                                                   */
/*                                                                 */
void GetDataHeader(FILE *fp, _Ehfa_File* dhdr, _Ehfa_HeaderTag* hdr)
{
  char* pTmpUlong;
  char* pTmpLong;
  char* pTmpSint;

  /* Allocate temporary variables */
  pTmpUlong = MALLOC(EMIF_T_ULONG_LEN);
  pTmpLong = MALLOC(EMIF_T_LONG_LEN);
  pTmpSint = MALLOC(EMIF_T_SHORT_LEN);

  /* Go to data header offset */
  fseek(fp, hdr->headerPtr, SEEK_SET);

  /* Read version number of ERDAS MIF file, should be '1' */
  DHFAfread(pTmpLong, EMIF_T_LONG_LEN, fp);
  dhdr->version = *((long*)pTmpLong);

  /* Read offset to free data locations list */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  dhdr->freeList = *((unsigned long*) pTmpUlong);

  /* Read offset to root entry in data tree */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  dhdr->rootEntryPtr = *((unsigned long*) pTmpUlong);

  /* Read length of header portion of data nodes */
  DHFAfread(pTmpSint, EMIF_T_SHORT_LEN, fp);
  dhdr->entryHeaderLength = *((short int*) pTmpSint);

  /* Read offset to data dictionary at end of file */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  dhdr->dictionaryPtr = *((unsigned long*) pTmpUlong);

  /* Clean up */
  FREE(pTmpUlong);
  FREE(pTmpLong);
  FREE(pTmpSint);
}

/* DHFAswab() mimics swab() by copying 'numBytes' from the array 'from'   */
/* to the array pointed to by 'to', but rather than just exchanging even  */
/* and odd adjacent bytes it reverses the entire stream of bytes during   */
/* the copy from 'from' to 'to'.  This makes the function work for any    */
/* length of binary value read from a file, not just 2-byte values.       */
/* NOTE: This function does nothing when 'numBytes' is negative, but      */
/* unlike swab(), this function DOES work on odd-length ('numBytes' is an */
/* odd number) values.                                                    */
/*                                                                        */
void DHFAswab(char *from, char *to, unsigned int numBytes)
{
  int i, j;

  for (i=0, j=numBytes-1;
       numBytes>0 && i<numBytes;
       i++, j--)
  {
    to[i] = from[j];
  }
}

/* DHFAfread() mimics fread() but reads one element 'size' bytes          */
/* long from the stream pointed to by 'stream', storing them at the       */
/* location given by 'ptr'.  In addition, DHFAfread() determines if the   */
/* local machine is little-endian or big-endian and swabs the bytes if    */
/* necessary (see notes.)                                                 */
/*                                                                        */
/* NOTE: The ERDAS MIF HFA file format is always in little-endian byte    */
/* order and the bytes should therefore be swabbed upon read.  The        */
/* standard fread() however, will swab() bytes already if the local       */
/* machine is little-endian.  Therefore we need to be careful to only     */
/* swab() the bytes read from the file in the case that the local machine */
/* is a big-endian machine (no automatic swabbing by fread())             */
/*                                                                        */
void DHFAfread(char *ptr, size_t size, FILE *fp)
{
  char *pTmp[2];
  char failed = 0;
  int bytesRead;
  int i;

  /* Allocate temporary storage */
  for (i=0; i<2; i++) {
    pTmp[i] = MALLOC(size);
  }

  /* Read the bytes from the file */
  bytesRead = fread(pTmp[0], 1, size, fp);
  if (bytesRead < size) {
    failed = 1;
  }

  /* swab() bytes on big-endian machines since fread() didn't */
  if (!failed) {
    if (local_machine_is_little_endian() == 0) {
      DHFAswab(pTmp[0], pTmp[1], size);
      for (i=0; i<size; i++) ptr[i] = (pTmp[1])[i];
    }
    else {
      for (i=0; i<size; i++) ptr[i] = (pTmp[0])[i];
    }
  }
  else {
    *ptr = 0; /* Default to a zero value if the read failed */
  }

  /* Clean up */
  for (i=0; i<2; i++) {
    FREE(pTmp[i]);
  }
}

/***** GetNode()                                               *****/
/*                                                                 */
/* GetNode(), given an offset to the node, reads one data node     */
/* in the data tree stored in the HFA file.  The data node         */
/* includes type info and pointers (file offsets) to related nodes */
/* in the file.  The actual data is separately accessed via the    */
/* the data pointer (offset) and type information derived from the */
/* node's type info and a look-up in the data dictionary.          */
/*                                                                 */
void GetNode(FILE *fp, unsigned long nodeOffset, _Ehfa_Entry *nodeEntry)
{
  char* pTmpUlong;
  char* pTmpLong;
  unsigned char* pTmpUchar;

  /* Allocate temporary variables */
  pTmpUlong = MALLOC(EMIF_T_ULONG_LEN);
  pTmpLong = MALLOC(EMIF_T_LONG_LEN);
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Go to node offset */
  fseek(fp, nodeOffset, SEEK_SET);

  /* Read 'next child' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->next = *((unsigned long*)pTmpUlong);

  /* Read 'previous child' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->prev = *((unsigned long*)pTmpUlong);

  /* Read 'parent node' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->parent = *((unsigned long*)pTmpUlong);

  /* Read 'child' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->child = *((unsigned long*)pTmpUlong);

  /* Read 'data record' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->data = *((unsigned long*)pTmpUlong);

  /* Read 'data record size' (number of bytes in data record) */
  DHFAfread(pTmpLong, EMIF_T_LONG_LEN, fp);
  nodeEntry->dataSize = *((long*)pTmpLong);

  /* Read 'node name' string */
  DHFAGetString(fp, MAX_EHFA_ENTRY_NAMESTRING_LEN, nodeEntry->name);

  /* Read 'data type' string */
  DHFAGetString(fp, MAX_EHFA_ENTRY_TYPESTRING_LEN, nodeEntry->type);

  /* Read 'time node last modified' */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->modTime = *((long*)pTmpUlong);

  /* Clean up */
  FREE(pTmpUlong);
  FREE(pTmpLong);
  FREE(pTmpUchar);
}

/***** DHFAGetStringFromOffset()                               *****/
/*                                                                 */
/* DHFAGetStringFromOffset() - a utility function.  This function  */
/* reads a fixed-length ASCII string (not stopping due to the      */
/* presence or lack of a '\0\ character) from any file given a     */
/* file offset to where the string resides.                        */
/*                                                                 */
void DHFAGetStringFromOffset(FILE *fp, unsigned char strOffset,
                             unsigned int maxSize, unsigned char *str)
{
  /* Go to node offset */
  fseek(fp, strOffset, SEEK_SET);

  /* Read the string from the file */
  DHFAGetString(fp, maxSize, str);
}

/***** DHFAGetString()                                         *****/
/*                                                                 */
/* DHFAGetString() - a utility function.  This function reads a    */
/* fixed-length ASCII string (not stopping due to the presence or  */
/* lack of a '\0\ character) from any file starting at the current */
/* file position associated with a FILE *fp.                       */
/*                                                                 */
void DHFAGetString(FILE *fp, unsigned int maxSize, unsigned char *str)
{
  unsigned char* pTmpUchar; /* Initialize to anything but '\0' */
  int i;

  /* Allocate temporary variables */
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Read the string from the file (up to and including any '\0's)   */
  /* NOTE: ASCII strings in ERDAS MIF HFA files are fixed length,    */
  /* so do not terminate the loop upon finding a '\0'.  It is up     */
  /* to the writer of the file to make sure that nulls are           */
  /* inserted after the last valid character if they choose to,      */
  /* otherwise it is up to the reader to properly terminate a string */
  /* read from the file.                                             */
  for (i = 0; i < maxSize; i++) {
    fread(pTmpUchar, 1, EMIF_T_UCHAR_LEN, fp);
    if (feof(fp))
      break;
    str[i] = (unsigned char) *pTmpUchar;
  }
  str[maxSize-1] = '\0'; /* Terminate the string (to be safe) */

  /* Clean up */
  FREE(pTmpUchar);
}

/***** DHFAGetIntegerValFromOffset()                           *****/
/*                                                                 */
void DHFAGetIntegerValFromOffset(FILE *fp, unsigned long offset,
                              long *val, char type)
{
  /* Go to file offset and read the WORD value */
  fseek(fp, offset, SEEK_SET);
  DHFAGetIntegerVal(fp, val, type);
}

/***** DHFAGetIntegerVal()                                     *****/
/*                                                                 */
void DHFAGetIntegerVal(FILE *fp, long *val, char type)
{
  char* pTmpWord;

  /* Just in case the definitions change, use the type lengths */
  /* for the fread() etc.                                      */
  switch (type) {
    case _EMIF_T_UCHAR:
      pTmpWord = MALLOC(EMIF_T_UCHAR_LEN);
      DHFAfread(pTmpWord, EMIF_T_UCHAR_LEN, fp);
      *val = *((unsigned char*) pTmpWord);
      break;
    case _EMIF_T_CHAR:
      pTmpWord = MALLOC(EMIF_T_CHAR_LEN);
      DHFAfread(pTmpWord, EMIF_T_CHAR_LEN, fp);
      *val = *((char*) pTmpWord);
      break;
    case _EMIF_T_ENUM:
      pTmpWord = MALLOC(EMIF_T_ENUM_LEN);
      DHFAfread(pTmpWord, EMIF_T_ENUM_LEN, fp);
      *val = *((unsigned short*) pTmpWord);
      break;
    case _EMIF_T_USHORT:
      pTmpWord = MALLOC(EMIF_T_USHORT_LEN);
      DHFAfread(pTmpWord, EMIF_T_USHORT_LEN, fp);
      *val = *((unsigned short*) pTmpWord);
      break;
    case _EMIF_T_SHORT:
      pTmpWord = MALLOC(EMIF_T_SHORT_LEN);
      DHFAfread(pTmpWord, EMIF_T_SHORT_LEN, fp);
      *val = *((short*) pTmpWord);
      break;
    case _EMIF_T_TIME:
      pTmpWord = MALLOC(EMIF_T_ULONG_LEN);
      DHFAfread(pTmpWord, EMIF_T_ULONG_LEN, fp);
      *val = *((unsigned long*) pTmpWord);
      break;
    case _EMIF_T_ULONG:
      pTmpWord = MALLOC(EMIF_T_ULONG_LEN);
      DHFAfread(pTmpWord, EMIF_T_ULONG_LEN, fp);
      *val = *((unsigned long*) pTmpWord);
      break;
    case _EMIF_T_LONG:
      pTmpWord = MALLOC(EMIF_T_LONG_LEN);
      DHFAfread(pTmpWord, EMIF_T_LONG_LEN, fp);
      *val = *((long*) pTmpWord);
      break;
    default:
      *val = 0;
      break;
  }

  /* Clean up */
  FREE(pTmpWord);
}

/***** DHFAGetDoubleValFromOffset()                            *****/
/*                                                                 */
void DHFAGetDoubleValFromOffset(FILE *fp, unsigned long offset, double *val)
{
  /* Go to file offset and read the WORD value */
  fseek(fp, offset, SEEK_SET);
  DHFAGetDoubleVal(fp, val);
}

/***** DHFAGetDoubleVal()                                      *****/
/*                                                                 */
void DHFAGetDoubleVal(FILE *fp, double *val)
{
  char* pTmpVal;

  /* Just in case the definitions change, use the type lengths */
  /* for the fread() etc.                                      */
  pTmpVal = MALLOC(EMIF_T_DOUBLE_LEN);
  DHFAfread(pTmpVal, EMIF_T_DOUBLE_LEN, fp);
  *val = *((double*) pTmpVal);
  FREE(pTmpVal);
}

/***** DHFAGetFloatValFromOffset()                            *****/
/*                                                                 */
void DHFAGetFloatValFromOffset(FILE *fp, unsigned long offset, float *val)
{
  /* Go to file offset and read the WORD value */
  fseek(fp, offset, SEEK_SET);
  DHFAGetFloatVal(fp, val);
}

/***** DHFAGetFloatVal()                                      *****/
/*                                                                 */
void DHFAGetFloatVal(FILE *fp, float *val)
{
  char* pTmpVal;

  /* Just in case the definitions change, use the type lengths */
  /* for the fread() etc.                                      */
  pTmpVal = MALLOC(EMIF_T_DOUBLE_LEN);
  DHFAfread(pTmpVal, EMIF_T_FLOAT_LEN, fp);
  *val = *((float*) pTmpVal);
  FREE(pTmpVal);
}

void DHFAGetStringValFromOffset(FILE *fp, unsigned long offset,
                                unsigned long strLen, char *str)
{
  if (strLen > 0) {
    fseek(fp, offset, SEEK_SET);
    DHFAGetStringVal(fp, strLen, str);
  }
  else {
    str = NULL;
  }
}

void DHFAGetStringVal(FILE *fp, unsigned long strLen, char *str)
{
  unsigned long i;
  char *pTmpStr;

  if (strLen > 0) {
    pTmpStr = MALLOC(strLen);
    for (i=0; i<strLen; i++) {
      fread(&pTmpStr[i], 1, 1, fp);
    }
    strncpy(str, pTmpStr, strLen);
    str[strLen] = '\0';
  }
  else {
    str = NULL;
  }
  if (pTmpStr != NULL)
    FREE(pTmpStr);
}

/***** GetDataDictionary()                                     *****/
/*                                                                 */
/* GetDataDictionary() reads the MIF HFA type data dictionary      */
/* (an ASCII string w/o nulls) from an HFA file given a file       */
/* offset.                                                         */
/*                                                                 */
void GetDataDictionary(FILE *fp, unsigned long ddOffset,char **dd)
{
  unsigned char* pTmpUchar;
  unsigned long endPos;
  unsigned long ddLen;
  int level;

  /* Allocate temporary variables */
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Count the number of bytes in the data dictionary */
  /* NOTE: The data dictionary exists at the end of the file but   */
  /* is of unknown length.  It starts at ddOffset and ends at EOF. */
  fseek(fp,0, SEEK_END);
  endPos = ftell(fp);
  ddLen = endPos - ddOffset;

  /* Allocate the data dictionary and read the data dictionary from */
  /* the file into it.                                              */
  *dd = MALLOC(1 + sizeof(unsigned char) * ddLen);
  level = 0; // Level of nesting in {} pairs
//  fseek(fp, ddOffset, SEEK_SET);

  /* Read the dictionary (a string) */
  DHFAGetStringValFromOffset(fp, ddOffset, ddLen, *dd);
  char *substr = strstr(*dd, ",.");
  if (substr != NULL && substr != *dd) {
    substr++;
    substr++;
    *substr = '\0';
  }
  else {
    asfPrintWarning("ArcGIS metadata (.aux) file's data dictionary may be\n"
        "corrupt.  Dictionary is not properly terminated.\n");
    (*dd)[ddLen] = '\0';
  }

  /* Clean up */
  FREE(pTmpUchar);
}

/***** ParseDictionary()                                           *****/
/*                                                                     */
/* ParseDictionary(...) parses the data dictionary (ASCII string)      */
/* from the MIF HFA file into a linked list of ddObjects, and a linked */
/* list of ddItems within each ddObject.                               */
/* NOTE: For convenience here, the linked list of ddObjects is         */
/* allocated as an array (with prev/next ptrs maintained), but all     */
/* code NOT in this function will use the prev/next ptrs to traverse   */
/* the list, e.g. while searching for parameters and freeing memory on */
/* the way out the door (main())                                       */
/*                                                                     */
void ParseDictionary(char *dd, ddObject ddObjects[], int lim)
{
  int numObjects;
  int i;

  /***** Parse the dictionary to produce an array of un-parsed object */
  /* strings.  Example: if the dd is                                  */
  /*                                                                  */
  /*  Example:  If the dd (a character string) is the following,      */
  /*                                                                  */
  /*     "{2:litem1,1:citem2,}obj1,{7:litem3,99:citem4,}obj2,."       */
  /*                                                                  */
  /*     then the goal is to produce an array of objects ready for    */
  /*     parsing items out of, like these:                            */
  /*                                                                  */
  /*     charArray[0].objName = "obj1"                                */
  /*     charArray[0].objStr  = "7:litem3,99:citem4,"                 */
  /*            <etc>                                                 */
  /*                                                                  */
  ParseDictionaryToObjectStrs (dd, ddObjects, &numObjects, lim);

  /***** Parse the data dictionary (string) objects into lists of *****/
  /* items.                                                           */
  /*                                                                  */
  for (i=0; i<numObjects; i++) {
    Parse_ObjectString_to_Items (ddObjects[i].objStr, ddObjects[i].ddItems,
                                 &(ddObjects[i].numItems));
  }
}

/* ParseDictionaryToObjectStrs() tokenizes the objects out of the long */
/* data dictionary string into objects (combination of an item list    */
/* and a data type name)                                               */
void ParseDictionaryToObjectStrs (char *dd, ddObject ddObjects[],
                                  int *count, int lim)
{
  ddObject tmpObj;
  char *tdd;
  BOOL foundObj;

  /* Parse out the first object string from the data dictionary */
  tdd = dd;
  *count = 0;
  do {
    foundObj = getObjectToken(&tdd, &tmpObj);

    if (foundObj) {
      strcpy(ddObjects[*count].objStr, tmpObj.objStr);
      strcpy(ddObjects[*count].objName, tmpObj.objName);
      ddObjects[*count].prev = NULL;
      ddObjects[*count].next = NULL;

      if (*count > 0) {
        ddObjects[*count].prev = &ddObjects[*count - 1];
        ddObjects[*count - 1].next = &ddObjects[*count];
      }

      (*count)++;

      /* NOTE: Not allowing *count to equal lim exactly allows some */
      /* buffer in ddObjects for extraneous characters that may     */
      /* result from nested item descriptions (this adds additional */
      /* '{' and '}' characters to the stream)                      */
      asfRequire(*count < lim,
                  "\nERROR: Infile contains too many data type objects"
                      " in the data dictionary\n");
    }
  } while (foundObj);
}

BOOL getObjectToken(char **tdd, ddObject *tmpObj)
{
  BOOL rtn = 0;
  char *pcTmp;
  char *pcTmp2;
  char *pcTmp3;
  int level;
  int charCount;

  /* Find and copy the object (string) token */
  pcTmp = strchr(*tdd, OPEN_BRACE);
  if (pcTmp != NULL && *pcTmp == OPEN_BRACE) {
    level = 1;
    charCount = 0;
    pcTmp++; /* Move just beyond '{' */
    pcTmp2 = pcTmp;
    while (level > 0 && *pcTmp2 != '\0') {
      switch (*pcTmp2) {
        case OPEN_BRACE:
          level++;
          if (level > 1) {
            charCount++;
          }
          break;
        case CLOSE_BRACE:
          if (level > 1) {
            charCount++;
          }
          level--;
          break;
        default:
          charCount++;
          break;
      }
      if (level > 0 && *pcTmp2 == '\0') {
        asfRequire(0,
                   "\ngetObjectToken() ERROR: Invalid data dictionary record in file\n");
      }
      pcTmp2++;
    }
    /* Save the object token (string) */
    strncpy(tmpObj->objStr, pcTmp, charCount);
    tmpObj->objStr[charCount] = '\0';

    /*** Find the copy the object token's name (data type) ***/
    /* Send the pointer back ready to find a new object token */
    *tdd = strchr(pcTmp2, ',');
    (*tdd)++;

    /*** Find the copy the object token's name (data type) ***/
    pcTmp = strtok_r(pcTmp2, ",", &pcTmp3);
    asfRequire(pcTmp != NULL,
               "\ngetObjectToken() ERROR: Invalid data dictionary record in file\n");
    strcpy(tmpObj->objName, pcTmp);

    rtn = 1;
  }

  return rtn;
}

/* Given a raw object string from the file's data dictionary, */
/* parse out the list of items contained therein ...note that */
/* the object is like a struct, the items are like the        */
/* struct's members, and that the file contains many such     */
/* struct-type data type definitions                          */
void Parse_ObjectString_to_Items (char objString[], ddItem *items, int *numItems)
{
  BOOL tooManyItems = 0;
  BOOL missingName;
  BOOL nestedItemFound;
  char *pcTmp = (char *)objString;
  char *pcTmp2;
  char *pcTmp3;
  char tmpStr[MAX_EHFA_OBJECTSTRING_LEN];
  int itemNo = 0;
  int numChars;
  int i;
  ddItem *item; /* Convenience pointer */
  ddObject tmpObj;

  asfRequire(pcTmp != NULL && *pcTmp != '\0',
             "\nParse_ObjectString_to_Items() ERROR: Empty object string\n");

  /* For each item in the object... */
  while (*pcTmp != '\0' && itemNo < MAX_EHFA_ITEMS_PER_OBJECT) {
    item = &(items[itemNo]); /* convenience ptr */

    /* Init prev/next and fields that may remain unused */
    item->numEnums = 0;
    strcpy(item->prevTypeName, "");
    strcpy(item->definedTypeName, "");
    item->nestedItems = NULL;
    item->indirectData = ' ';

    /* Parse out the number of items for this item type */
    while (!isdigit(*pcTmp) && *pcTmp != '\0') pcTmp++;
    pcTmp2 = pcTmp;
    numChars = 0;
    /*   ...Normally the following stops on ':' */
    while (isdigit(*pcTmp2) && *pcTmp2 != '\0') {
      numChars++;
      pcTmp2++;
    }
    strncpy(tmpStr, pcTmp, numChars);
    tmpStr[numChars] = '\0';
    item->number = atoi(tmpStr); /* Zero (0) is a valid result */
    pcTmp = pcTmp2;
    pcTmp++; /* Point at first char past the ':' */

    /* Check for 'indirect data type' and store if necessary */
    if (*pcTmp == '*' || *pcTmp == 'p') {
      item->indirectData = *pcTmp;
      pcTmp++; /* Move to char just past the '*' or 'p' */
    }

    /* Store the data type indicator (a char) */
    asfRequire(validDataType(*pcTmp) != 0,
               "\nERROR: Invalid or unsupported data type found in data dictionary\n");
    item->dataType = *pcTmp;
    pcTmp++;
    /* pcTmp now either points at the name of the type, the number of enums in an */
    /* enum type, left brace (for nested, 'defined', types) or the name of a      */
    /* previously-defined type                                                    */

    /* Perform datatype-specific parsing if necessary including recursive call to */
    /* Parse_ObjectString_to_Items() if necessary for nested types                */
    switch (item->dataType)
    {
      case _EMIF_T_ENUM:
        /* Parse out the number of items for this item type */
        while (!isdigit(*pcTmp) && *pcTmp != '\0') pcTmp++;
        pcTmp2 = pcTmp; /* pcTmp and pcTmp2 point at num of enums */
        numChars = 0;
        /*   ...Normally the following stops on ':' */
        while (isdigit(*pcTmp2) && *pcTmp2 != '\0') {
          numChars++;
          pcTmp2++;
        }
        strncpy(tmpStr, pcTmp, numChars);
        tmpStr[numChars] = '\0';
        item->numEnums = atoi(tmpStr); /* Must be integer and grt zero */
        asfRequire(item->numEnums > 0,
                   "\nERROR: Found enum type with no members in data dictionary\n");
        pcTmp += numChars + 1; /* Now points at first char of first enum string */
        /* Parse out the enum strings (names of enum types) */
        item->enumNames = (char **)MALLOC(item->numEnums * sizeof(char *));
        asfRequire(item->enumNames != NULL,
                   "\nParse_ObjectString_to_Items() ERROR: Memory allocation error\n");
        for (i=0; i < item->numEnums; i++) {
          item->enumNames[i] =
              (char *)MALLOC(MAX_EHFA_ITEMSTRING_LEN * sizeof(char));
          asfRequire(item->enumNames[i] != NULL,
                     "\nParse_ObjectString_to_Items() ERROR: Memory allocation error\n");
        }
        strcpy(tmpStr, pcTmp); /* copy remainder of object string for strtok_r() */
        pcTmp2 = strtok_r(tmpStr, ",", &pcTmp3);
        asfRequire(pcTmp2 != NULL && pcTmp2 > 0,
                   "\nERROR: Found empty enum element name in data dictionary\n");
        strcpy(item->enumNames[0], pcTmp2);
        numChars = strlen(item->enumNames[0]);
        for (i=1; i < item->numEnums; i++) {
          pcTmp2 = strtok_r(NULL, ",", &pcTmp3);
          asfRequire(pcTmp2 != NULL && pcTmp2 > 0,
                     "\nERROR: Found empty enum element name in data dictionary\n");
          strcpy(item->enumNames[i], pcTmp2);
          numChars += strlen(item->enumNames[i]);
        }
        pcTmp += numChars + i; /* Now points at data type name following last enum str */
        break;
      case _EMIF_T_PREDEFINED:
        strcpy(tmpStr, pcTmp); /* copy remainder of object string for strtok_r() */
        pcTmp2 = strtok_r(tmpStr, ",", &pcTmp3); /* get name of predefined type */
        asfRequire(pcTmp2 != NULL && pcTmp2 > 0,
                   "\nERROR: Found empty predefined type name in data dictionary\n");
        strcpy(item->prevTypeName, pcTmp2);
        numChars = strlen(item->prevTypeName);
        pcTmp += numChars + 1; /* Now points at data type name following predef data type name */
        break;
      case _EMIF_T_DEFINED:
        /* Allocate an array to hold the nested items */
        item->nestedItems = MALLOC(MAX_EHFA_NESTEDITEMS_PER_ITEM * sizeof(ddItem));
        asfRequire(item->nestedItems != NULL,
                   "\nERROR: Parse_ObjectString_to_Items() memory allocation error\n");
        /* The following borrows the getObjectToken() function since a nested item */
        /* has the same format ({...}...,) as an object string.  Upon return, the  */
        /* item string is contained in tmpObj.objStr                               */
        nestedItemFound = getObjectToken(&pcTmp, &tmpObj);
        if (nestedItemFound) {
          /* Recursive call to Parse_ObjectString_to_Items() to populate  */
          /* the nested item with the item list contained within it.      */
          Parse_ObjectString_to_Items(tmpObj.objStr, item->nestedItems,
                                      &item->numNestedItems);
          strcpy(item->definedTypeName, tmpObj.objName);
        }
        break;
        default: /* Other data types do not need further parsing */
          break;
    }

    /* Parse out & save the datatype name */
    numChars = strlen(pcTmp);
    if (*pcTmp != '\0' && strlen(pcTmp) > 0) {
      strcpy(item->name, strtok_r(pcTmp, DELIM_TYPENAME, &pcTmp2));
      pcTmp += strlen(item->name) + 1; /* Move to next item or '\0' */
      missingName = 0;
    }
    else {
      strcpy(item->name, MISSING_ASCII_DATA);
      missingName = 1;
    }
    if (missingName) {
      printf("\nERROR: Parse_ObjectString_to_Items() found missing data type name in\n"
          "item description.  Original object string:\n\n\"%s\"\n",
      objString);
    }

    /* If too many items for allocated storage, then vaMoose */
    itemNo++;
    tooManyItems = (itemNo >= MAX_EHFA_ITEMS_PER_OBJECT) ? 1 : 0;
    if (tooManyItems) {
      printf("\nParse_ObjectString_to_Items() found too many items in data\n"
          "dictionary object.  Original object string:\n\n\"%s\"\n",
      objString);
    }
    asfRequire(tooManyItems == 0,
               "\nERROR: Too many items in data dictionary object\n");
  }
  *numItems = itemNo;
}

/* Return nonzero if data type character is valid according to the */
/* ERDAS MIF HFA file standard                                     */
int validDataType(char dataType)
{
  switch (dataType) {
    case _EMIF_T_U1:
    case _EMIF_T_U2:
    case _EMIF_T_U4:
    case _EMIF_T_UCHAR:
    case _EMIF_T_CHAR:
    case _EMIF_T_ENUM:
    case _EMIF_T_USHORT:
    case _EMIF_T_SHORT:
    case _EMIF_T_TIME:
    case _EMIF_T_ULONG:
    case _EMIF_T_LONG:
    case _EMIF_T_FLOAT:
    case _EMIF_T_DOUBLE:
    case _EMIF_T_COMPLEX:
    case _EMIF_T_DCOMPLEX:
    case _EMIF_T_BASEDATA:
    case _EMIF_T_PREDEFINED:
    case _EMIF_T_DEFINED:
      return 1;
      break;
    default:
      return 0;
      break;
  }
}

/***** PrintDictionary()                                           *****/
/*                                                                     */
/* PrintDictionary(...) dumps the data dictionary (linked list) to     */
/* stdout in a more understandable human-readable form (prettified)    */
/*                                                                     */
void PrintDictionary(ddObject *ddObjects, char *dd)
{
  int i;
  ddObject *obj;

  printf("\nDATA DICTIONARY (raw, unparsed):"
      "\n====================================");
  printf("\n%s\n", dd);

  printf("\nDATA DICTIONARY (parsed):"
      "\n=============================");
  obj = ddObjects;
  i=0;
  while (obj) {
    printf("\n-------------------\n    OBJECT NO: %d\n", i+1);
    printf("  OBJECT NAME: \"%s\"\n", obj->objName);
    printf("Number of Items: %d\n", obj->numItems);
    PrintItems(obj->ddItems, obj->numItems, 1);
    obj = obj->next;
    i++;
  }
}

void PrintItems(ddItem *items, int numItems, int tabLevel)
{
  int i, j; /* loop vars */
  char tabs[TABSTRING_LEN]; /* leading indent */
  char type[32]; /* Item type name string */

  /* Build leading whitespace string for indent */
  strcpy(tabs, "");
  strncat(tabs, TABSTRING, (tabLevel * TAB_LEN > TABSTRING_LEN - 2) ?
      TABSTRING_LEN - 2 : tabLevel * TAB_LEN);
  for (i=0; i<numItems; i++) {
    /* Assign linguistic term to data type */
    switch (items[i].dataType) {
      case _EMIF_T_U1:
        strcpy(type, EMIF_T_U1_STR);
        break;
      case _EMIF_T_U2:
        strcpy(type, EMIF_T_U2_STR);
        break;
      case _EMIF_T_U4:
        strcpy(type, EMIF_T_U4_STR);
        break;
      case _EMIF_T_UCHAR:
        strcpy(type, EMIF_T_UCHAR_STR);
        break;
      case _EMIF_T_CHAR:
        strcpy(type, EMIF_T_CHAR_STR);
        break;
      case _EMIF_T_ENUM:
        strcpy(type, EMIF_T_ENUM_STR);
        break;
      case _EMIF_T_USHORT:
        strcpy(type, EMIF_T_USHORT_STR);
        break;
      case _EMIF_T_SHORT:
        strcpy(type, EMIF_T_SHORT_STR);
        break;
      case _EMIF_T_TIME:
        strcpy(type, EMIF_T_TIME_STR);
        break;
      case _EMIF_T_ULONG:
        strcpy(type, EMIF_T_ULONG_STR);
        break;
      case _EMIF_T_LONG:
        strcpy(type, EMIF_T_LONG_STR);
        break;
      case _EMIF_T_FLOAT:
        strcpy(type, EMIF_T_FLOAT_STR);
        break;
      case _EMIF_T_DOUBLE:
        strcpy(type, EMIF_T_DOUBLE_STR);
        break;
      case _EMIF_T_COMPLEX:
        strcpy(type, EMIF_T_COMPLEX_STR);
        break;
      case _EMIF_T_DCOMPLEX:
        strcpy(type, EMIF_T_DCOMPLEX_STR);
        break;
      case _EMIF_T_BASEDATA:
        strcpy(type, EMIF_T_BASEDATA_STR);
        break;
      case _EMIF_T_PREDEFINED:
        strcpy(type, EMIF_T_PREDEFINED_STR);
        break;
      case _EMIF_T_DEFINED:
        strcpy(type, EMIF_T_DEFINED_STR);
        break;
      default:
        strcpy(type, "Undefined type");
        break;
    }

    /* Dump to stdio */
    printf("\n%sITEM DESCRIPTION:\n", tabs);
    printf("%s  name: \"%s\"\n", tabs, items[i].name);
    printf("%s  number: %d\n", tabs, items[i].number);
    if (items[i].indirectData == '*' || items[i].indirectData == 'p') {
      printf("%s  indirection type: '%c'\n", tabs, items[i].indirectData);
    }
    else {
      printf("%s  indirection type: none\n", tabs);
    }
    printf("%s  data type: '%c' (%s)\n", tabs, items[i].dataType, type);
    if (items[i].dataType == _EMIF_T_DEFINED) {
      printf("%s  defined type name: \"%s\"\n", tabs, items[i].definedTypeName);
    }
    else{
      printf("%s  defined type name: none\n", tabs);
    }
    printf("%s  number of enums: %d\n", tabs, items[i].numEnums);
    for (j=0; j<items[i].numEnums; j++){
      printf("%s      \"%s\"\n", tabs, items[i].enumNames[j]);
    }
    printf("%s  number of nested items: %d\n", tabs, items[i].numNestedItems);
    if (items[i].numNestedItems > 0) {
      printf("%s  NESTED ITEMS:\n", tabs);
      PrintItems(items[i].nestedItems, items[i].numNestedItems, tabLevel + 2);
    }
    else{
      printf("%s  NESTED ITEMS: none\n", tabs);
    }
    if (items[i].dataType == _EMIF_T_PREDEFINED) {
      printf("%s  previous type defn: \"%s\"\n", tabs, items[i].prevTypeName);
    }
    else{
      printf("%s  previous type defn: none\n", tabs);
    }
  }
  printf("\n");
}

/***** traverseNodes()                                                *****/
/*                                                                        */
void traverseNodes(FILE *fp, _Ehfa_Entry *node, unsigned long nodeOffset, BOOL dumpFlag)
{
  _Ehfa_Entry newNode;

  if (dumpFlag) {
    printDataNode(node, nodeOffset);
  }
  if (node->child) {
    GetNode(fp, node->child, &newNode);
    asfRequire(newNode.prev == (unsigned long) 0,
               "\nERROR: traverseNodes() found a child with non-NULL prev pointer\n");
    traverseNodes(fp, &newNode, node->child, dumpFlag);
  }
  if (node->next) {
    GetNode(fp, node->next, &newNode);
    traverseNodes(fp, &newNode, node->next, dumpFlag);
  }
}

void freeItems(ddItem *items, int numItems)
{
  int i;
  for (i=0; i<numItems; i++) {
    freeOneItem(&items[i]);
  }
}

void freeOneItem(ddItem *item)
{
  int i;
  ddItem *_nestedItems;

  /* NOTE: The array of objects declared in main has no malloc()'d            */
  /* memory but each object contains an array of ddItems.  ddItems have no    */
  /* malloc()'d memory unless the type includes enums or nested items.        */
  /* Arrays of enum strings are malloc()'d as are arrays of ddItems           */
  /* for the nested items if applicable. For the case of nested items, note   */
  /* that freeOneItem() is called recursively for each item in order to       */
  /* handle the situation where items are nested in items to some unknown     */
  /* level.                                                                   */
  /*                                                                          */

  /* Free the enums if they exist */
  if (item->dataType == _EMIF_T_ENUM && item->numEnums > 0) {
    for (i=0; i<item->numEnums; i++) {
      FREE(item->enumNames[i]);
    }
    FREE(item->enumNames);
  }

  /* Free the nested types if they exist */
  if (item->dataType == _EMIF_T_DEFINED && item->numNestedItems > 0) {
    _nestedItems = (ddItem *)item->nestedItems;
    for (i=0; i<item->numNestedItems; i++) {
      freeOneItem(&_nestedItems[i]);
    }
    FREE(item->nestedItems);
  }
}

void printDataNode(_Ehfa_Entry *node, unsigned long nodeOffset)
{
  if (strncmp(node->name, "root", 4) != 0) {
    printf("\n    DATA NODE (at offset 0x%04x):\n", (unsigned int) nodeOffset);
    printf("        name: \"%s\"\n", node->name);
    printf("        type: \"%s\"\n", node->type);
    printf("        bytes of data: %ld\n", node->dataSize);
    printf("        data offset: 0x%04x (%ldd)\n", (unsigned int)node->data, node->data);
    printf("        time stamp: %ld\n", node->modTime);
    printf("        parent: 0x%04x (%ldd)\n", (unsigned int)node->parent, node->parent);
    printf("        child: 0x%04x (%ldd)\n", (unsigned int)node->child, node->child);
    printf("        prev: 0x%04x (%ldd)\n", (unsigned int)node->prev, node->prev);
    printf("        next: 0x%04x (%ldd)\n", (unsigned int)node->next, node->next);
  }
}

/***** FindNode()                                                   *****/
/* Performs an in-file recursive pre-order tree traversal to find a     */
/* known type of object, ex: a type "Eprj_ProParameters" data object    */
/*                                                                      */
short FindNode (FILE *fp, _Ehfa_Entry *node, char *type,
                _Ehfa_Entry *foundNode)
{
  short nodeFound = 0;
  _Ehfa_Entry newNode;

  /* Pre-order check on searched-for node */
  /* ...FOUND... STOP LOOKING...          */
  if (strncmp(node->type, type, strlen(type)) == 0) {
    strcpy(foundNode->name, node->name);
    strcpy(foundNode->type, node->type);
    foundNode->dataSize = node->dataSize;
    foundNode->modTime = node->modTime;
    foundNode->data = node->data;
    foundNode->parent = node->parent;
    foundNode->child = node->child;
    foundNode->next = node->next;
    foundNode->prev = node->prev;

    nodeFound = 1;
  }

  /* If not found yet and a child node exists, look further... */
  if (node->child && !nodeFound) {
    GetNode(fp, node->child, &newNode);
    asfRequire(newNode.prev == (unsigned long) 0,
               "\nFindNode() found a child with non-NULL prev pointer\n");
    nodeFound = FindNode(fp, &newNode, type, foundNode);
  }

  /* If not found yet and a next node exists, look further... */
  if (node->next && !nodeFound) {
    GetNode(fp, node->next, &newNode);
    nodeFound = FindNode(fp, &newNode, type, foundNode);
  }

  /* ...NOT FOUND and NO MORE NODES... Nullify all...       */
  if (!nodeFound) {
    strcpy(foundNode->name, "");
    strcpy(foundNode->type, "");
    foundNode->dataSize = (long)0;
    foundNode->modTime = (unsigned long)0;
    foundNode->data = (unsigned long)0;
    foundNode->parent = (unsigned long)0;
    foundNode->child = (unsigned long)0;
    foundNode->next = (unsigned long)0;
    foundNode->prev = (unsigned long)0;
  }

  return nodeFound;
}

void getArcgisProjParameters(char *infile, arcgisProjParms_t *proParms)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  long offset;
  int i;
  unsigned long strLen;
  unsigned long nElements;

  char    sphereName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  double  a,
          b,
          eSquared,
          radius;
  long    proNumber;
  long    proZone;
  double  proParams[ARCGIS_NUM_PROJDPARAMS];
  unsigned long proType;
  char    proExeName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char    proName[MAX_EHFA_ENTRY_NAMESTRING_LEN];

  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "\nError opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  proNumber = (unsigned long)MAGIC_UNSET_INT;
  proZone = (unsigned long)MAGIC_UNSET_INT;
  for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++){
    proParams[i] = MAGIC_UNSET_DOUBLE;
  }
  proType = 0L; // Can't use MAGIC_UNSET_INT because it's a negative number
  strcpy(proExeName, MAGIC_UNSET_STRING);
  strcpy(proName, MAGIC_UNSET_STRING);
  strcpy(sphereName, MAGIC_UNSET_STRING);
  a = MAGIC_UNSET_DOUBLE;
  b = MAGIC_UNSET_DOUBLE;
  eSquared = MAGIC_UNSET_DOUBLE;
  radius = MAGIC_UNSET_DOUBLE;

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp(hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "\nArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "\nArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_PROPARAMETERS, &foundNode); // do a get, but via a search
  if (nodeFound) {
    // Get proType (enum idx, 0 == 'EPRJ_INTERNAL' and 1 == 'EPRJ_EXTERNAL'
    DHFAGetIntegerValFromOffset(fp, foundNode.data, &proType, _EMIF_T_ENUM);

    // Get proNumber, e.g. 4 => Lambert Conformal Conic
    DHFAGetIntegerVal(fp, &proNumber, _EMIF_T_LONG);

    // Get proExeName, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    // NOTE: proExeName is the name of an executable that can convert
    // the file between the two proType types listed above, if it exists
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-p
    if (offset > 0) {
      fseek(fp, offset, SEEK_SET);
      DHFAGetIntegerVal(fp, &strLen, _EMIF_T_ULONG);
      if (strLen > 0) {
        DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
        if (offset > 0) {
          fseek(fp, offset, SEEK_SET);
          DHFAGetStringVal(fp, strLen, proExeName);
        }
      }
    }

    // Get proName, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    // NOTE: proName is the textual name of the projection type, e.g.
    // "Albers Conical Equal Area" or other
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-p
    if (offset > 0) {
      fseek(fp, offset, SEEK_SET);
      DHFAGetIntegerVal(fp, &strLen, _EMIF_T_ULONG);
      if (strLen > 0) {
        DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
        if (offset > 0) {
          fseek(fp, offset, SEEK_SET);
          DHFAGetStringVal(fp, strLen, proName);
        }
      }
    }

    // Get the proZone (only applies to UTM, but always exists in
    // the file)
    DHFAGetIntegerVal(fp, &proZone, _EMIF_T_LONG);

    // Get the proParams, the array of projection parameters (always
    // exists in the file), DOUBLE-p
    DHFAGetIntegerVal(fp, &nElements, _EMIF_T_ULONG); // Get num of elements in array
    if (nElements > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Get offset to array
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        for (i=0; i<nElements && i<ARCGIS_NUM_PROJDPARAMS; i++) {
          DHFAGetDoubleVal(fp, &proParams[i]);
        }
      }
    }

    // Get proSpheroid data from file (CHAR-*)
    DHFAGetIntegerVal(fp, &nElements, _EMIF_T_ULONG); // Get number of spheroids (should be 1)
    if (nElements > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Get offset to spheroid name
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetIntegerVal(fp, &strLen, _EMIF_T_ULONG); // Get length of spheroid name
        if (strLen > 0) {
          DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
          if (offset > 0) {
            fseek(fp, offset, SEEK_SET);
            DHFAGetStringVal(fp, strLen, sphereName); // Finally get the name!
          }
        }
      }
    }
    DHFAGetDoubleVal(fp, &a);
    DHFAGetDoubleVal(fp, &b);
    DHFAGetDoubleVal(fp, &eSquared);
    DHFAGetDoubleVal(fp, &radius);
  }

  // Populate return struct
  proParms->proNumber = proNumber;
  proParms->proZone = proZone;
  for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++){
    proParms->proParams[i] = proParams[i];
  }
  proParms->proType = (unsigned short) proType;
  strcpy(proParms->proExeName, proExeName);
  strcpy(proParms->proName, proName);
  strcpy(proParms->proSpheroid.sphereName, sphereName);
  proParms->proSpheroid.a = a;
  proParms->proSpheroid.b = b;
  proParms->proSpheroid.eSquared = eSquared;
  proParms->proSpheroid.radius = radius;

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
  }
  fclose(fp);
}

spheroid_type_t arcgisSpheroidName2spheroid(char *sphereName)
{
  spheroid_type_t rtnVal;

  if (
      strncmp(sphereName,
              ARCGIS_BESSEL_SPHEROID,
              strlen(ARCGIS_BESSEL_SPHEROID)) == 0
     )
  {
    rtnVal = BESSEL_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_CLARKE1866_SPHEROID,
                   strlen(ARCGIS_CLARKE1866_SPHEROID)) == 0
          )
  {
    rtnVal = CLARKE1866_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_CLARKE1880_SPHEROID,
                   strlen(ARCGIS_CLARKE1880_SPHEROID)) == 0
          )
  {
    rtnVal = CLARKE1880_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_GEM6_SPHEROID,
                   strlen(ARCGIS_GEM6_SPHEROID)) == 0
          )
  {
    rtnVal = GEM6_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_GEM10C_SPHEROID,
                   strlen(ARCGIS_GEM10C_SPHEROID)) == 0
          )
  {
    rtnVal = GEM10C_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_GRS1980_SPHEROID,
                   strlen(ARCGIS_GRS1980_SPHEROID)) == 0
          )
  {
    rtnVal = GRS1980_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_INTERNATIONAL1924_SPHEROID,
                   strlen(ARCGIS_INTERNATIONAL1924_SPHEROID)) == 0
          )
  {
    rtnVal = INTERNATIONAL1924_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_INTERNATIONAL1967_SPHEROID,
                   strlen(ARCGIS_INTERNATIONAL1967_SPHEROID)) == 0
          )
  {
    rtnVal = INTERNATIONAL1967_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_WGS72_SPHEROID,
                   strlen(ARCGIS_WGS72_SPHEROID)) == 0
          )
  {
    rtnVal = WGS72_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_WGS84_SPHEROID,
                   strlen(ARCGIS_WGS84_SPHEROID)) == 0
          )
  {
    rtnVal = WGS84_SPHEROID;
  }

  return rtnVal;
}

void getArcgisDatumParameters(char *infile, arcgisDatumParms_t *datumParms)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  long offset;
  int i;
  unsigned long strLen;
  unsigned long nElements;

  char          datumname[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  unsigned long type;
  double        params[ARCGIS_NUM_DATUMDPARAMS];
  char          gridname[MAX_EHFA_ENTRY_NAMESTRING_LEN];

  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "\nError opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  strcpy(datumname, MAGIC_UNSET_STRING);
  type = 0L;
  for (i=0; i<ARCGIS_NUM_DATUMDPARAMS; i++){
    params[i] = MAGIC_UNSET_DOUBLE;
  }
  strcpy(gridname, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp(hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "\nArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "\nArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_DATUM, &foundNode); // do a get, but via a search
  if (nodeFound) {
    // Get datumname, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    DHFAGetIntegerValFromOffset(fp, foundNode.data, &strLen, _EMIF_T_ULONG);
    if (strLen > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetStringVal(fp, strLen, datumname);
      }
    }

    // Get type (enum idx, 0 == 'EPRJ_DATUM_PARAMETRIC',
    // 1 == 'EPRJ_DATUM_GRID', and 2 == 'EPRJ_DATUM_REGRESSION'
    DHFAGetIntegerVal(fp, &type, _EMIF_T_ENUM);

    // Get the datum double params, the array of datum parameters (always
    // exists in the file), DOUBLE-p
    DHFAGetIntegerVal(fp, &nElements, _EMIF_T_ULONG); // Get num of elements in array
    if (nElements > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Get offset to array
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        for (i=0; i<nElements && i<ARCGIS_NUM_DATUMDPARAMS; i++) {
          DHFAGetDoubleVal(fp, &params[i]);
        }
      }
    }

    // Get gridname, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-p
    if (offset > 0) {
      fseek(fp, offset, SEEK_SET);
      DHFAGetIntegerVal(fp, &strLen, _EMIF_T_ULONG);
      if (strLen > 0) {
        DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
        if (offset > 0) {
          fseek(fp, offset, SEEK_SET);
          DHFAGetStringVal(fp, strLen, gridname);
        }
      }
    }
  }

  // Populate return struct
  strcpy(datumParms->datumname, datumname);
  datumParms->type = (unsigned short)type;
  for (i=0; i<ARCGIS_NUM_DATUMDPARAMS; i++){
    datumParms->params[i] = params[i];
  }
  strcpy(datumParms->gridname, gridname);

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
  }
  fclose(fp);
}

void getArcgisMapInfo(char *infile, arcgisMapInfo_t *arcgisMapInfo)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  long offset;
  int i;
  unsigned long strLen;
  unsigned long nElements;

  char                proName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  arcgisCoordinate_t  upperLeftCenter; // x, y
  arcgisCoordinate_t  lowerRightCenter; // x, y
  arcgisSize_t        pixelSize; // width, height
  char                units[MAX_EHFA_ENTRY_NAMESTRING_LEN];

  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "\nError opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  strcpy(proName, MAGIC_UNSET_STRING);
  upperLeftCenter.x = MAGIC_UNSET_DOUBLE;
  upperLeftCenter.y = MAGIC_UNSET_DOUBLE;
  lowerRightCenter.x = MAGIC_UNSET_DOUBLE;
  lowerRightCenter.y = MAGIC_UNSET_DOUBLE;
  pixelSize.width = MAGIC_UNSET_DOUBLE;
  pixelSize.height = MAGIC_UNSET_DOUBLE;
  strcpy(units, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp(hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "\nArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "\nArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_MAPINFO, &foundNode); // do a get, but via a search
  if (nodeFound) {
    // Get proName, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    DHFAGetIntegerValFromOffset(fp, foundNode.data, &strLen, _EMIF_T_ULONG);
    if (strLen > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetStringVal(fp, strLen, proName);
      }
    }

    // Get upperLeftCenter coordinates
    // ...read number of Eprj_Coordinate elements and offset to first one,
    // then read the doubles.  No need to loop here since the Eprj_MapInfo
    // by default only defines a single upperLeftCenter coordinate
    DHFAGetIntegerVal(fp, &nElements, _EMIF_T_ULONG);
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
    fseek(fp, offset, SEEK_SET);
    DHFAGetDoubleVal(fp, &upperLeftCenter.x);
    DHFAGetDoubleVal(fp, &upperLeftCenter.y);

    // Get lowerRightCenter coordinates
    DHFAGetIntegerVal(fp, &nElements, _EMIF_T_ULONG);
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
    fseek(fp, offset, SEEK_SET);
    DHFAGetDoubleVal(fp, &lowerRightCenter.x);
    DHFAGetDoubleVal(fp, &lowerRightCenter.y);

    // Get pixelSize
    DHFAGetIntegerVal(fp, &nElements, _EMIF_T_ULONG);
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
    fseek(fp, offset, SEEK_SET);
    DHFAGetDoubleVal(fp, &pixelSize.width);
    DHFAGetDoubleVal(fp, &pixelSize.height);

    // Get units string, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    // NOTE: This is a CHAR-* not a CHAR-p, so the first element is
    // the number of characters (followed by an offset to the string)
    DHFAGetIntegerVal(fp, &strLen, _EMIF_T_ULONG); // Length of units str
    if (strLen > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-*
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetStringVal(fp, strLen, units);
      }
    }
  }

  // Populate return struct
  strcpy(arcgisMapInfo->proName, proName);
  arcgisMapInfo->upperLeftCenter.x = upperLeftCenter.x;
  arcgisMapInfo->upperLeftCenter.y = upperLeftCenter.y;
  arcgisMapInfo->lowerRightCenter.x = lowerRightCenter.x;
  arcgisMapInfo->lowerRightCenter.y = lowerRightCenter.y;
  arcgisMapInfo->pixelSize.width = pixelSize.width;
  arcgisMapInfo->pixelSize.height = pixelSize.height;
  strcpy(arcgisMapInfo->units, units);

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
  }
  fclose(fp);
}

int PCS_2_UTM(short pcs, datum_type_t *datum, unsigned long *zone)
{
  // The GeoTIFF standard defines the UTM zones numerically in a way that
  // let's us pick off the data mathematically (NNNzz where zz is the zone
  // number):
  //
  // For NAD83 datums, Zones 3N through 23N, NNN == 269
  // For NAD27 datums, Zones 3N through 22N, NNN == 367
  // For WGS72 datums, Zones 1N through 60N, NNN == 322
  // For WGS72 datums, Zones 1S through 60S, NNN == 323
  // For WGS84 datums, Zones 1N through 60N, NNN == 326
  // For WGS84 datums, Zones 1S through 60S, NNN == 327
  // For user-defined UTMs, Northern Hemisphere, NNN == 160
  // For user-defined UTMs, Southern Hemisphere, NNN == 161
  // For other user-defined and/or unsupported UTM projections,
  //   then NNN can be a variety of other numbers (see the
  //   GeoTIFF Standard)
  //
  // NOTE: For NAD27 and NAD83, only the restricted range of zones
  // above is supported by the GeoTIFF standard.
  //
  const short NNN_NAD27 = 367;
  const short NNN_NAD83 = 269;
  const short NNN_WGS84 = 326;
  const short NNN_USER_DEFINED_NORTH = 160;
  const short NNN_USER_DEFINED_SOUTH = 161;
  int isUTM = 0;
  short datumClassifierNNN;

  datumClassifierNNN = pcs / 100;
  if (datumClassifierNNN == NNN_NAD27) {
      *datum = NAD27_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
      if (*zone < 3 || *zone > 22) {
        *datum = UNKNOWN_DATUM;
        *zone = 0;
        isUTM = 0;
      }
  }
  else if (datumClassifierNNN == NNN_NAD83) {
      *datum = NAD83_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
      if (*zone < 3 || *zone > 23) {
        *datum = UNKNOWN_DATUM;
        *zone = 0;
        isUTM = 0;
      }
  }
  else if (datumClassifierNNN == NNN_WGS84) {
      *datum = WGS84_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
      if (*zone < 1 || *zone > 60) {
        *datum = UNKNOWN_DATUM;
        *zone = 0;
        isUTM = 0;
      }
  }
  else if (datumClassifierNNN == NNN_USER_DEFINED_NORTH ||
           datumClassifierNNN == NNN_USER_DEFINED_SOUTH) {
    /*
        NOTE: For the user-defined cases, the datum should be set
        from the geokey GeographicTypeGeoKey (or possibly
        GeogGeodeticDatumGeoKey) before or after the calling of
        PCS_2_UTM(), therefore *datum is not assigned anything
        here.
    */
    *zone = pcs - (pcs / 100)*100;
    isUTM = 1;
    if (*zone < 1 || *zone > 60) {
      *datum = UNKNOWN_DATUM;
      *zone = 0;
      isUTM = 0;
    }
  }
  else {
      *datum = UNKNOWN_DATUM;
      *zone = 0;
      isUTM = 0;
  }

  return isUTM;
}

void getArcgisEimg_MapInformation (char *infile,
                 arcgisEimg_MapInformation_t *arcgisEimg_MapInformation)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  int i;
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "\nError opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  strcpy(arcgisEimg_MapInformation->projection, MAGIC_UNSET_STRING);
  strcpy(arcgisEimg_MapInformation->units, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp(hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "\nArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "\nArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EIMG_MAPINFORMATION, &foundNode); // do a get, but via a search
  if (nodeFound) {
    readArcgisEimg_MapInformation (fp, foundNode.data, arcgisEimg_MapInformation);
  }

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
  }
  fclose(fp);
}

void readArcgisEimg_MapInformation (FILE *fp, unsigned long offset,
                                    arcgisEimg_MapInformation_t *arcgisEimg_MapInformation)
{
  // TODO:  This code is UNTESTED ...so far, I have not been able
  // to find an HFA file that contains a Eimg_MapInformation node
  // in it
  long strOffset;
  unsigned long strLen;
  char  projection[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char  units[MAX_EHFA_ENTRY_NAMESTRING_LEN];


  // Populate values to be read from the file with initial values
  strcpy(projection, MAGIC_UNSET_STRING);
  strcpy(units, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  fseek(fp, offset, SEEK_SET);
  // Get projection string, first val is a ushort string length and
  // if greater than zero, immediately followed by an offset to
  // the string of characters (otherwise followed by next data item)
  DHFAGetIntegerValFromOffset(fp, offset, &strLen, _EMIF_T_ULONG);
  if (strLen > 0) {
    DHFAGetIntegerVal(fp, &strOffset, _EMIF_T_ULONG); // Offset to str itself
    if (strOffset > 0) {
      fseek(fp, strOffset, SEEK_SET);
      DHFAGetStringVal(fp, strLen, projection);
    }
  }

  // Get units string, first val is a ushort string length and
  // if greater than zero, immediately followed by an offset to
  // the string of characters (otherwise followed by next data item)
  // NOTE: This is a CHAR-* not a CHAR-p, so the first element is
  // the number of characters (followed by an offset to the string)
  DHFAGetIntegerVal(fp, &strLen, _EMIF_T_ULONG); // Length of units str
  if (strLen > 0) {
    DHFAGetIntegerVal(fp, &strOffset, _EMIF_T_ULONG); // Offset to CHAR-*
    if (strOffset > 0) {
      fseek(fp, strOffset, SEEK_SET);
      DHFAGetStringVal(fp, strLen, units);
    }
  }

  // Populate return struct
  strcpy(arcgisEimg_MapInformation->projection, projection);
  strcpy(arcgisEimg_MapInformation->units, units);
}

void copy_proj_parms(meta_projection *dest, meta_projection *src)
{
  dest->type = src->type;
  dest->startX = src->startX;
  dest->startY = src->startY;
  dest->perX = src->perX;
  dest->perY = src->perY;
  strcpy (dest->units, src->units);
  dest->hem = src->hem;
  dest->spheroid = src->spheroid;
  dest->re_major = src->re_major;
  dest->re_minor = src->re_minor;
  dest->datum = src->datum;
  dest->height = src->height;

  switch (src->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      dest->param.utm.zone = src->param.utm.zone;
      dest->param.utm.lat0 = D2R*src->param.utm.lat0;
      dest->param.utm.lon0 = D2R*src->param.utm.lon0;
      dest->param.utm.false_easting = src->param.utm.false_easting;
      dest->param.utm.false_northing = src->param.utm.false_northing;
      dest->param.utm.scale_factor = src->param.utm.scale_factor;
      break;
    case POLAR_STEREOGRAPHIC:
      dest->param.ps.slat = D2R*src->param.ps.slat;
      dest->param.ps.slon = D2R*src->param.ps.slon;
      dest->param.ps.is_north_pole = src->param.ps.is_north_pole;
      dest->param.ps.false_easting = src->param.ps.false_easting;
      dest->param.ps.false_northing = src->param.ps.false_northing;
      break;
    case ALBERS_EQUAL_AREA:
      dest->param.albers.std_parallel1 = D2R*src->param.albers.std_parallel1;
      dest->param.albers.std_parallel2 = D2R*src->param.albers.std_parallel2;
      dest->param.albers.center_meridian = D2R*src->param.albers.center_meridian;
      dest->param.albers.orig_latitude = D2R*src->param.albers.orig_latitude;
      dest->param.albers.false_easting = src->param.albers.false_easting;
      dest->param.albers.false_northing = src->param.albers.false_northing;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      dest->param.lamcc.plat1 = D2R*src->param.lamcc.plat1;
      dest->param.lamcc.plat2 = D2R*src->param.lamcc.plat2;
      dest->param.lamcc.lat0 = D2R*src->param.lamcc.lat0;
      dest->param.lamcc.lon0 = D2R*src->param.lamcc.lon0;
      dest->param.lamcc.false_easting = src->param.lamcc.false_easting;
      dest->param.lamcc.false_northing = src->param.lamcc.false_northing;
      dest->param.lamcc.scale_factor = src->param.lamcc.scale_factor;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      dest->param.lamaz.center_lon = D2R*src->param.lamaz.center_lon;
      dest->param.lamaz.center_lat = D2R*src->param.lamaz.center_lat;
      dest->param.lamaz.false_easting = src->param.lamaz.false_easting;
      dest->param.lamaz.false_northing = src->param.lamaz.false_northing;
      break;
    default:
      asfRequire(0,"Unsupported projection type found.\n");
      break;
  }
}

// Print minimalistic usage info & exit
void usage(const char *name)
{
  fprintf(stderr,"\nUsage:\n  dumphfa [-d] <basename>\n\n");
  fprintf(stderr,"  Description:\n");
  fprintf(stderr,"    Parses an hierarchical file archive (HFA), machine independent data format (MIF)\n");
  fprintf(stderr,"    type file for projection and datum parameters.  Intended usage is for ArcGIS type\n");
  fprintf(stderr,"    .AUX metadata files which accompany ArcGIS-exported GeoTIFF files.\n\n");
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"    d - Enable printing of data dictionary (raw and parsed) (typically large)\n\n");
}

void print_projection_parameters (arcgisProjParms_t *proParms) {
  printf("proType: %s\n", proParms->proType == 0 ? "EPRJ_INTERNAL" : proParms->proType == 1 ? "EPRJ_EXTERNAL" : "UNKNOWN");
  printf("proExeName: %s (normally blank)\n", proParms->proExeName);
  printf("proNumber: %ld (", proParms->proNumber);
  switch (proParms->proNumber) {
    case 0:
      printf("Geographic (Latitude/Longitude))\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[]: (None used)\n");
      break;
    case 1:
      printf("UTM)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proZone: %ld\n", proParms->proZone);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[3]: %lf (%s)\n", proParms->proParams[3],
             proParms->proParams[3] == 1 ? "North" : proParms->proParams[3] == -1 ? "South" : "Unknown");
      break;
    case 2:
      printf("State Plane)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proZone: %ld\n", proParms->proZone);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[0]: %lf (%s)\n", proParms->proParams[0],
             proParms->proParams[0] == 0 ? "NAD27" : proParms->proParams[0] == 1 ? "NAD83" : "Unknown Datum");
      break;
    case 3:
      printf("Albers Conical Equal Area)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[2]: %lf rad (%lf deg, %s)\n", proParms->proParams[2], R2D*proParms->proParams[2], "Latitude of 1st standard parallel");
      printf("proParams[3]: %lf rad (%lf deg, %s)\n", proParms->proParams[3], R2D*proParms->proParams[3], "Latitude of 2nd standard parallel");
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridan");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of origin of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 4:
      printf("Lambert Conformal Conic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[2]: %lf rad (%lf deg, %s)\n", proParms->proParams[2], R2D*proParms->proParams[2], "Latitude of 1st standard parallel");
      printf("proParams[3]: %lf rad (%lf deg, %s)\n", proParms->proParams[3], R2D*proParms->proParams[3], "Latitude of 2nd standard parallel");
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridan");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of origin of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 5:
      printf("Mercator)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridan");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of origin of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 6:
      printf("Polar Stereographic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4],
             "Longitude directed straight down below pole of map");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of true scale");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 7:
      printf("Polyconic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridan");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of origin of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 8:
      printf("Equidistant Conic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      if (proParms->proParams[8] == 0) {
        printf("proParams[2]: %lf rad (%lf deg, %s)\n", proParms->proParams[2], R2D*proParms->proParams[2],
               "Latitude of the (only) standard parallel (Case 0), e.g. a tangent conic projection,\n");
      }
      else if (proParms->proParams[8] == 1) {
        printf("proParams[2]: %lf rad (%lf deg, %s)\n", proParms->proParams[2], R2D*proParms->proParams[2],
               "Latitude of the 1st standard parallel (Case 1), e.g. a secant conic projection,\n");
      }
      else {
        printf("proParams[2]: (%s)\n",
               "Unknown Case\n");
      }
      if (proParms->proParams[8] == 1) {
        printf("proParams[3]: %lf rad (%lf deg, %s)\n", proParms->proParams[3], R2D*proParms->proParams[3],
               "Latitude of 2nd standard parallel (Case 1)");
      }
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridan");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of origin of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      printf("proParams[8]: %lf (%s)\n", proParms->proParams[8],
             proParms->proParams[8] == 0 ? "Case 0" : proParms->proParams[8] == 1 ? "Case 1" : "Unknown Case");
      break;
    case 9:
      printf("Transverse Mercator)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[2]: %lf (%s)\n", proParms->proParams[2], "Scale Factor at Central Meridian");
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 10:
      printf("Stereographic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 11:
      printf("Lambert Azimuthal Equal Area)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 12:
      printf("Azimuthal Equidistant)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 13:
      printf("Gnomonic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 14:
      printf("Orthographic)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 15:
      printf("General Vertical Near-Side Perspective)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[2]: %lf (%s, m)\n", proParms->proParams[2], "Height of perspective point above sphere");
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of center of projection");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*R2D*proParms->proParams[5], "Latitude of center of projection");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 16:
      printf("Sinusoidal)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridian");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 17:
      printf("Equirectangular)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridian");
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of True Scale");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 18:
      printf("Miller Cylindrical)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridian");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 19:
      printf("Van der Grinten I)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4], "Longitude of central meridian");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 20:
      printf("Oblique Mercator (Hotine))\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[2]: %lf (%s)\n", proParms->proParams[2], "Scale Factor at center of projection");
      if (proParms->proParams[12] == 1) {
        printf("proParams[3]: %lf rad (%lf deg, %s)\n", proParms->proParams[3], R2D*proParms->proParams[3],
               "Azimuth east of north for central line (Case 1)");
        printf("proParams[4]: %lf rad (%lf deg, %s)\n", proParms->proParams[4], R2D*proParms->proParams[4],
               "Longitude of point of origin (Case 1)");
      }
      printf("proParams[5]: %lf rad (%lf deg, %s)\n", proParms->proParams[5], R2D*proParms->proParams[5], "Latitude of point of origin");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      if (proParms->proParams[12] == 0) {
        printf("proParams[8]: %lf rad (%lf deg, %s)\n", proParms->proParams[8], R2D*proParms->proParams[8],
              "Longitude of 1st Point defining central line (Case 0)");
        printf("proParams[9]: %lf rad (%lf deg, %s)\n", proParms->proParams[9], R2D*proParms->proParams[9],
              "Latitude of 1st Point defining central line (Case 0)");
        printf("proParams[10]: %lf rad (%lf deg, %s)\n", proParms->proParams[10], R2D*proParms->proParams[10],
              "Longitude of 2nd Point defining central line (Case 0)");
        printf("proParams[11]: %lf rad (%lf deg, %s)\n", proParms->proParams[11], R2D*proParms->proParams[11],
              "Latitude of 2nd Point defining central line (Case 0)");
      }
      printf("proParams[12]: %lf (%s)\n", proParms->proParams[12],
             proParms->proParams[12] == 0 ? "Case 0" : proParms->proParams[12] == 1 ? "Case 1" : "Unknown Case");
      break;
    case 21:
      printf("Space Oblique Mercator)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[4]: %lf (%s)\n", proParms->proParams[4], "Landsat Vehicle ID (1-5)");
      printf("proParams[5]: %lf (%s)\n", proParms->proParams[5], "Orbital Path Number (1-251 or 1-233)");
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    case 22:
      printf("Modified Transverse Mercator)\n");
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      printf("proParams[6]: %lf rad (%lf deg, %s)\n", proParms->proParams[6], R2D*proParms->proParams[6], "False Easting");
      printf("proParams[7]: %lf rad (%lf deg, %s)\n", proParms->proParams[7], R2D*proParms->proParams[7], "False Northing");
      break;
    default:
      printf("Unknown Type of Projection)\n");
      printf("proZone: %ld\n", proParms->proZone);
      printf("proName: %s\n", proParms->proName);
      printf("proSpheroid.sphereName = %s (Spheroid Name)\n", proParms->proSpheroid.sphereName);
      printf("proSpheroid.a = %lf (re_major, m)\n", proParms->proSpheroid.a);
      printf("proSpheroid.b = %lf (re_minor, m)\n", proParms->proSpheroid.b);
      printf("proSpheroid.eSquared = %lf (eccentricity squared)\n", proParms->proSpheroid.eSquared);
      printf("proSpheroid.radius = %lf (radius, m)\n", proParms->proSpheroid.radius);
      int i;
      for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++) {
        printf("proParams[%d]: %lf (%s)\n", i, proParms->proParams[i], "Unknown Interpretation");
      }
      break;
  }
}

void print_datum_parameters (arcgisDatumParms_t *datumParms) {
  int i;

  printf("datumName: %s\n", datumParms->datumname);
  printf("gridName : %s\n", datumParms->gridname);
  switch(datumParms->type) {
    case 0: // EPRJ_DATUM_PARAMETRIC
      printf("type: %d (%s)\n", datumParms->type, "Parametric type datum");
      break;
    case 1: // EPRJ_DATUM_GRID
      printf("type: %d (%s)\n", datumParms->type, "Grid type datum");
      break;
    case 2: // EPRJ_DATUM_REGRESSION
      printf("type: %d (%s)\n", datumParms->type, "Regression type datum");
      break;
    default:
      printf("type: (Unknown)\n");
      break;
  }
  printf("\n** Datum Translation, Rotation, and Scale parameters for converting datum to WGS-84 **\n");
  printf(" ------------------------------------------------------------------------------------\n");
  for (i=0; i< ARCGIS_NUM_DATUMDPARAMS; i++) {
    printf("datumParm[%d]: %lf\n", i, datumParms->params[i]);
  }
}




