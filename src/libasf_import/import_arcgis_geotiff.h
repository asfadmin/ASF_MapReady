#ifndef _IMPORT_ARCGIS_GEOTIFF_H_
#define _IMPORT_ARCGIS_GEOTIFF_


/***** ERDAS MIF data types & lengths, in bytes (below)                 *****/
/*                                                                          */
/* NOTE: The entire MIF file contains only little-endian data (LSB at       */
/* lowest address), e.g. Intel little-endian byte order                     */
/* NOTE: Please refer to document iau_docu1.pdf from ERDAS for more         */
/* information on their machine independent format (MIF) files.             */
/* NOTE: The MIF formatted hierarchical file archive (HFA) is               */
/* used for Imagine (.img) and other binary files coming from ERDAS         */
/* such as the ArcGIS auxiliary metadata file (.aux or .tif.aux)            */
/* NOTE: The iau_docu1.pdf document sometimes refers to the MIF types       */
/* with the "a.k.a. <type name>" shown in the comments below, but uses      */
/* the single-character name when describing data in the data               */
/* dictionary (in the file) format.                                         */
/* NOTE: "EMIF_T_" means "ERDAS MIF type" and reflects std ERDAS naming.    */
/* NOTE: A length of zero (0) means 'dynamically defined'.  In other words  */
/* the actual data element itself will have 'count' and 'number of bytes    */
/* per item' information associated with it in the data store itself.       */
/*                                                                          */

/* ERDAS MIF data types */
#define _EMIF_T_U1            '1'
#define _EMIF_T_U2            '2'
#define _EMIF_T_U4            '4'
#define _EMIF_T_UCHAR         'c'
#define _EMIF_T_CHAR          'C'
#define _EMIF_T_ENUM          'e'
#define _EMIF_T_USHORT        's'
#define _EMIF_T_SHORT         'S'
#define _EMIF_T_TIME          't'
#define _EMIF_T_ULONG         'l'
#define _EMIF_T_LONG          'L'
#define _EMIF_T_FLOAT         'f'
#define _EMIF_T_DOUBLE        'd'
#define _EMIF_T_COMPLEX       'm'
#define _EMIF_T_DCOMPLEX      'M'
#define _EMIF_T_BASEDATA      'b'
#define _EMIF_T_PREDEFINED    'o'
#define _EMIF_T_DEFINED       'x'

/* Length of ERDAS MIF data types, in bytes */
#define EMIF_T_U1_LEN          1
#define EMIF_T_U2_LEN          1
#define EMIF_T_U4_LEN          1
#define EMIF_T_UCHAR_LEN       1
#define EMIF_T_CHAR_LEN        1
#define EMIF_T_ENUM_LEN        2
#define EMIF_T_USHORT_LEN      2
#define EMIF_T_SHORT_LEN       2
#define EMIF_T_TIME_LEN        4
#define EMIF_T_ULONG_LEN       4
#define EMIF_T_LONG_LEN        4
#define EMIF_T_FLOAT_LEN       4
#define EMIF_T_DOUBLE_LEN      8
#define EMIF_T_COMPLEX_LEN     8
#define EMIF_T_DCOMPLEX_LEN    16
#define EMIF_T_BASEDATA_LEN    0
#define EMIF_T_PREDEFINED_LEN  0
#define EMIF_T_DEFINED_LEN     0

/* String representations of ERDAS MIF data types */
#define EMIF_T_U1_STR          "EMIF_T_U1"
#define EMIF_T_U2_STR          "EMIF_T_U2"
#define EMIF_T_U4_STR          "EMIF_T_U4"
#define EMIF_T_UCHAR_STR       "EMIF_T_UCHAR"
#define EMIF_T_CHAR_STR        "EMIF_T_CHAR"
#define EMIF_T_ENUM_STR        "EMIF_T_ENUM"
#define EMIF_T_USHORT_STR      "EMIF_T_USHORT"
#define EMIF_T_SHORT_STR       "EMIF_T_SHORT"
#define EMIF_T_TIME_STR        "EMIF_T_TIME"
#define EMIF_T_ULONG_STR       "EMIF_T_ULONG"
#define EMIF_T_LONG_STR        "EMIF_T_LONG"
#define EMIF_T_FLOAT_STR       "EMIF_T_FLOAT"
#define EMIF_T_DOUBLE_STR      "EMIF_T_DOUBLE"
#define EMIF_T_COMPLEX_STR     "EMIF_T_COMPLEX"
#define EMIF_T_DCOMPLEX_STR    "EMIF_T_DCOMPLEX"
#define EMIF_T_BASEDATA_STR    "EMIF_T_BASEDATA"
#define EMIF_T_PREDEFINED_STR  "EMIF_T_PREDEFINED"
#define EMIF_T_DEFINED_STR     "EMIF_T_DEFINED"

/* ERDAS MIF data object types, each containing ERDAS MIF data items.       */
/* This data type structure is analogous to a "typedef struct" where        */
/* the name of the type is the data object name, e.g. "Eprj_ProParameters", */
/* and the elements of the typed struct are the data items, e.g.            */
/* "ProType" (type EMIF_T_ULONG), "proNAME" (type EMIF_T_UCHAR, an array)   */
/* etcetera                                                                 */
/*                                                                          */
#define EIMG_LAYER                "Eimg_Layer"
#define EIMG_LAYER_SUBSAMPLE      "Eimg_Layer_SubSample"
#define EIMG_NONINITIALIZEDVALUE  "Eimg_NonInitializedValue"

#define EHFA_LAYER                "Ehfa_Layer"

#define EDMS_VIRTUALBLOCKINFO     "Edms_VirtualBlockInfo"
#define EDMS_FREEIDLIST           "Edms_FreeIDList"
#define EDMS_STATE                "Edms_State"

#define EDSC_TABLE                "Edsc_Table"
#define EDSC_BINFUNCTION          "Edsc_BinFunction"
#define EDSC_COLUMN               "Edsc_Column"

#define EDED_COLUMNATTRIBUTES_1   "Eded_ColumnAttributes_1"

#define ESTA_STATISTICS           "Esta_Statistics"
#define ESTA_COVARIANCE           "Esta_Covariance"
#define ESTA_SKIPFACTORS          "Esta_SkipFactors"
#define ESTA_EXCLUDEDVALUES       "Esta_ExcludedValues"

#define EPRJ_DATUM                "Eprj_Datum"
#define EPRJ_SPHEROID             "Eprj_Spheroid"
#define EPRJ_PROPARAMETERS        "Eprj_ProParameters"
#define EPRJ_COORDINATE           "Eprj_Coordinate"
#define EPRJ_SIZE                 "Eprj_Size"
#define EPRJ_MAPINFO              "Eprj_MapInfo"

#define EFGA_POLYNOMIAL           "Efga_Polynomial"
#define CALIBRATION_NODE          "Calibration_Node"
#define EMIF_ROOTNODE             "root"


/* ERDAS MIF file header string and length & misc other MIF constants */
#define EHFA_HEADER_TAG                 "EHFA_HEADER_TAG"
#define EHFA_HEADER_TAG_LEN             16
#define MAX_EHFA_ENTRY_NAMESTRING_LEN   64
#define MAX_EHFA_ENTRY_TYPESTRING_LEN   32
#define MAX_EHFA_ITEMS_PER_OBJECT       64
#define MAX_EHFA_NESTEDITEMS_PER_ITEM   MAX_EHFA_OBJECTS_PER_DICTIONARY
#define MAX_EHFA_OBJECTS_PER_DICTIONARY 64
#define MAX_EHFA_ITEMSTRING_LEN         MAX_EHFA_ENTRY_NAMESTRING_LEN
#define MAX_EHFA_OBJECTSTRING_LEN       ((MAX_EHFA_ITEMSTRING_LEN+5)*MAX_EHFA_ITEMS_PER_OBJECT)
#define DELIM_TYPENAME                  "},"

/* Other defines */
#ifndef BOOL
# define BOOL char
#endif
#define MAX_FILENAME_LEN 256
/* ASCII Codes for '{' and '}' */
#define OPEN_BRACE              (123)
#define CLOSE_BRACE             (125)
#define MISSING_ASCII_DATA      "???"
#define DHFA_UNKNOWN_PROJECTION (-1)

/* Defines for output to stdio etc */
#define TABSTRING "                                                                                          "
#define TABSTRING_LEN 80
#define TAB_LEN 2

/***** ERDAS MIF HFA related typedefs                                  *****/
/*                                                                         */
/* ERDAS MIF Ehfa_HeaderTag                                                */
/* {16:clabel,1:lheaderPtr,}Ehfa_HeaderTag                                 */
/*                                                                         */
typedef struct {
  unsigned char label[EHFA_HEADER_TAG_LEN]; /* Label of file type.  Should always be "EHFA_HEADER_TAG" */
  unsigned long headerPtr; /* Pointer to header data.  See _Ehfa_File. */
} _Ehfa_HeaderTag;

/* ERDAS MIF Ehfa_File */
/* {1:Lversion,1:lfreeList,1:lrootEntryPtr,1:SentryHeaderLength,1:ldictionaryPtr,} */
/* NOTE: Each node in the data tree consists of 2 parts.  The first part is the    */
/*       entry portion (node name, node type, parent/child info).  The second      */
/*       portion is the data for the node.                                         */
typedef struct {
  long version; /* ERDAS MIF version.  Should always be '1' */
  unsigned long freeList; /* Offset to freed blocks list within file */
  unsigned long rootEntryPtr; /* Offset to root entry of data tree */
  short int entryHeaderLength; /* Length of entry portion of a tree node */
  unsigned long dictionaryPtr; /* Offset to data dictionary at end of file */
} _Ehfa_File;

/* ERDAS MIF Ehfa_Entry */
/* {1:lnext,1:lprev,1:lparent,1:lchild,1:ldata,1:LdataSize,64:cname,32:ctype, */
/*  1:tmodTime,}Ehfa_Entry                                                    */
/* NOTE: See the note for _Ehfa_File typedef above.                           */
/* NOTE: 1) Child nodes are oriented horizontally within a level, 2) Each     */
/* child has next and prev pointers to access other children to the right and */
/* left of itself respectively, 3) If a node has children, then the child     */
/* pointer points to the left-most child in the next level down, while 4) the */
/* parent pointer within any node (any child etc) points to the parent node   */
/* which owns the children. 5) A zero (0) in any next/prev/parent/child node  */
/* indicates that the current node is the last in that particular direction.  */
/*                                                                            */
typedef struct {
  char name[MAX_EHFA_ENTRY_NAMESTRING_LEN]; /* Name of this node */
  char type[MAX_EHFA_ENTRY_TYPESTRING_LEN]; /* Type of data in this node */
  long dataSize; /* Number of bytes in data record for this node */
  unsigned long data; /* Offset to data record for this node */
  unsigned long modTime; /* Time of last modification of this node */
  unsigned long parent; /* Offset to parent node in data tree */
  unsigned long child; /* Offset to first child node */
  unsigned long next; /* Offset to next node in data tree */
  unsigned long prev; /* Offset to previous node in data tree */
}_Ehfa_Entry;

/* The terms 'object' and 'item' etc are defined in the ERDAS MIF HFA */
/* document iau_docu1.pdf.  The following typedefs are named to       */
/* reflect these definitions (even though the names are somewhat      */
/* ambiguous)                                                         */
/*                                                                    */
/* The data dictionary is an ASCII string containing a list (unknown  */
/* length) of 'object's, and each 'object' in the list contains a     */
/* list (unknown length) of 'item's.  Each 'item' contains some basic */
/* info associated with data such as counts, pointers to data, the    */
/* data type associated with the data type name, etcetera (see ddItem */
/* type below)                                                        */
/*                                                                    */
typedef struct {
  int number; /* Number of data elements in the data store */
  char indirectData; /* Optional - '*' or 'p' indirection indicator */
  char name[MAX_EHFA_ITEMSTRING_LEN]; /* Name of this item */
  char dataType; /* Character indicating the data type */
  char definedTypeName[MAX_EHFA_ITEMSTRING_LEN]; /* Name of 'defined' type if applicable */
  char **enumNames; /* Array of enum names (strings) */
  int numEnums; /* Number of enum names */
  char prevTypeName[MAX_EHFA_ITEMSTRING_LEN];
  void *nestedItems; /* a (nested) array of ddItems for type 'x' */
  int numNestedItems;
} ddItem;

typedef struct {
  ddItem ddItems[MAX_EHFA_ITEMS_PER_OBJECT]; /* Data items (struct elements) in the data object (struct) */
  int numItems;
  char objStr[MAX_EHFA_OBJECTSTRING_LEN]; /* TODO: Refine later ...no need to store this string in each object once the Items are parsed out */
  char objName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  void *prev; /* Previous object in the list of objects */
  void *next; /* Next object in the list of objects */
} ddObject;

/***** ERDAS MIF HFA related Prototypes                         *****/
/*                                                                  */
void GetAuxHeader(FILE *fp, _Ehfa_HeaderTag* hdr);
void GetDataHeader(FILE *fp, _Ehfa_File* dhdr, _Ehfa_HeaderTag* hdr);
void GetDataDictionary(FILE *fp, unsigned long ddOffset,char **dd);
void GetNode(FILE *fp, unsigned long nodeOffset, _Ehfa_Entry *nodeEntry);
short FindNode (FILE *fp, _Ehfa_Entry *rootNode, char *type, _Ehfa_Entry *foundNode);
void printDataNode(_Ehfa_Entry *node, unsigned long nodeOffset);
void traverseNodes(FILE *fp, _Ehfa_Entry *node,unsigned long nodeOffset, BOOL dumpFlag);
void ParseDictionary(char *dd, ddObject ddObjects[], int lim);
BOOL getObjectToken(char **tdd, ddObject *tmpObj);
short getArcgisProjType(const char *file);
void ParseDictionaryToObjectStrs(char *dd, ddObject Objects[], int *count, int lim);
int  validDataType(char dataType);
void Parse_ObjectString_to_Items (char objString[], ddItem *items, int *numItems);
void PrintDictionary(ddObject *ddObjects, char *dd);
void PrintItems(ddItem *items, int numItems, int tabLevel);
void DHFAswab(char *from, char *to, unsigned int numBytes);
void DHFAfread(char *ptr, size_t size, FILE *stream);
void DHFAGetIntegerVal(FILE *fp, long *val, char type);
void DHFAGetIntegerValFromOffset(FILE *fp, unsigned long offset,
                                 long *val, char type);
void DHFAGetString(FILE *fp, unsigned int maxSize, unsigned char *str);
void DHFAGetStringFromOffset(FILE *fp, unsigned char strOffset,
                             unsigned int maxSize, unsigned char *str);
void freeItems(ddItem *items, int numItems);
void freeOneItem(ddItem *item);
void usage (const char *name);
unsigned char local_machine_is_little_endian();

#endif // _IMPORT_ARCGIS_GEOTIFF_H_
