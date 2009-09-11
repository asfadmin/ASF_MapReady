/*****************************************
coniFetch:
	A collection of utilies for extracting various
data from VEXCEL Level-Zero Granule and Parameter files,
which are in the CONI heirarchical text file format.

	This is a simple, ASCII-based line-oriented
format.  The basic structure is:
<field>: <value1>
<struct> {
	<subField>: <subValue>
}

	Note that these routines allocate their
return strings in the heap, so their strings must
be free'd afterward, or memory will leak.
*/

/*Possible error messages:*/
#define CONI_OK 0        /*No error.*/
#define CONI_MISSING 1   /*Field can't be found in file.*/
#define CONI_NOTINT 2    /*Field present, but not an integer.*/
#define CONI_NOTDOUBLE 3 /*Field present, but not a double.*/

struct coniStruct;/*Real coniStruct is hidden.*/

/*coniOpen allocates a coniStruct and does a 
fopen(fName,dir==asciiIn?"r":"w");*/
typedef enum {asciiIn,asciiOut} coniDir;/*Input or output direction.*/
coniStruct *coniOpen(const char *fName,coniDir dir);

/** Return 1 if the CONI file is being read; 0 if it's being written */
int coniIsUnpacking(coniStruct *coni);

/*coniReopen seeks to beginning of coni file.*/
void coniReopen(coniStruct *coni);

/*ConiClose closes & disposes of a coni structure.*/
void coniClose(coniStruct *coni);

/*Each of these routines read a single value from the CONI file.*/
double coniDouble(coniStruct *coni,const char *desiredParam,int *err);
int coniInt(coniStruct *coni,const char *desiredParam,int *err);
char * coniStr(coniStruct *coni,const char *desiredParam,int *err);

/*coniWrite just does a 
fprintf(coni->file,"\t%s %s ;%s\n",param,value,comment);*/
void coniWrite(coniStruct *coni,const char *param,const char *value,const char *comment);

/*ConiIO routines call the above procedures to
read or write, depending on coniOpen's direction.
The comment fields can be NULL, indicating no comment.*/
void coniIO_structOpen (coniStruct *coni,const char *name,const char *comment);
void coniIO_structClose(coniStruct *coni,const char *name,const char *comment);
int  coniIO_structProbe(coniStruct *coni,const char *name);
void coniIO_str    (coniStruct *coni,const char *name,char *value,int valueSize,const char *comment);

/** This macro lets you use coniIO with a fixed-size "char buf[n]" */
#define coniIO_fixstr(coni,name,value,comment) \
	coniIO_str(coni,name,value,sizeof(value),comment)

void coniIO_int    (coniStruct *coni,const char *name,int &value,const char *comment);
void coniIO_double (coniStruct *coni,const char *name,double &value,const char *comment);
void coniIO_char   (coniStruct *coni,const char *name,char &value,const char *comment);
