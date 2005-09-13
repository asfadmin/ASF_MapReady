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

typedef void coniStruct;/*Real coniStruct is hidden.*/

/*coniOpen allocates a coniStruct and does a 
fopen(fName,dir==asciiIn?"r":"w");*/
typedef enum {asciiIn,asciiOut} coniDir;/*Input or output direction.*/
coniStruct *coniOpen(const char *fName,coniDir dir);

/*coniReopen seeks to beginning of coni file.*/
void coniReopen(coniStruct *coni);

/*ConiClose closes & disposes of a coni structure.*/
void coniClose(coniStruct *coni);

/*Each of these routines read a single value from the CONI file.*/
double coniDouble(coniStruct *coni,char *desiredParam,int *err);
int coniInt(coniStruct *coni,char *desiredParam,int *err);
char * coniStr(coniStruct *coni,char *desiredParam,int *err);

/*coniWrite just does a 
fprintf(coni->file,"\t%s %s ;%s\n",param,value,comment);*/
void coniWrite(coniStruct *coni,char *param,char *value,char *comment);

/*ConiIO routines call the above procedures to
read or write, depending on coniOpen's direction.
The comment fields can be NULL, indicating no comment.*/
void coniIO_structOpen (coniStruct *coni,char *name,char *comment);
void coniIO_structClose(coniStruct *coni,char *comment);
void coniIO_str    (coniStruct *coni,char *loc,char *name,char *value,char *comment);
void coniIO_int    (coniStruct *coni,char *loc,char *name,int *value,char *comment);
void coniIO_double (coniStruct *coni,char *loc,char *name,double *value,char *comment);
void coniIO_char   (coniStruct *coni,char *loc,char *name,char *value,char *comment);
