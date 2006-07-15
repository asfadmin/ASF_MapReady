/*****************************************
coniFetch:
	A collection of utilies for extracting various
data from:
	- ASF .meta files.
	- VEXCEL Level-0 Granule files (.par files).
	- JPL ODL files.

	Note that these routines allocate their
return strings in the heap, so their strings must
be free'd afterward, or memory will leak.
*/
#include "asf/caplib.h"
#include "coniFetch.h"
#include <string> /* std::string--the coolest thing about C++! */

/**************************************************
coniOpen:
	Opens the given CONI file, and prepares a coniStruct.
*/

enum {maxDepth=16};

/*Private definition of the real coniStruct.*/
struct coniStruct {
	int lineNo;/*Current line number in file.*/
	FILE *fp;/*CONI file pointer.*/
	char fName[255];/*CONI file name. (for error messages)*/
	coniDir dir;/*Direction-- can be ascii input or ascii output.*/
	
/* These fields are used for traversing the file */
	int depth;/*Current level of name stack.*/
	char name[maxDepth][256];/*Stack of nested structure names.*/

/* These fields are part of the user interface code */
	/** Fully qualified address of struct we're describing: foo.bar.baz */
	std::string loc;
	/** The length of the "loc" string before entering this level */
	int locLength[maxDepth];
};

/*Private initialization routine*/
void coniInit(coniStruct *coni,const char *fName,coniDir dir)
{
/*Initialize fields.*/
	coni->depth=0;
	strcpy(coni->name[coni->depth],"");
	coni->lineNo=0;
	strcpy(coni->fName,fName);
	coni->dir=dir;
	
/*Open coni file.*/
	coni->fp=fopen(coni->fName,coni->dir==asciiIn?"r":"w");
	
	if (coni->fp==NULL)
	{/*Couldn't open file.*/
		printf("ERROR!  Couldn't open CONI file '%s'!\n",fName);
		exit(1);
	}
}

coniStruct *coniOpen(const char *fName,coniDir dir)
{
/*Allocate coni structure.*/
	coniStruct *coni=new coniStruct;
/*Initialize and return.*/
	coni->loc="";
	coniInit(coni,fName,dir);
	return (coniStruct *)coni;
}
void coniReopen(coniStruct *coni)
{
	fclose(coni->fp);
	coniInit(coni,coni->fName,coni->dir);
	
}
/** Return 1 if the CONI file is being read; 0 if it's being written */
int coniIsUnpacking(coniStruct *coni)
{
	return coni->dir==asciiIn;
}
/**************************************************
coniClose:
	Closes and de-allocates the given coniStruct.
*/
void coniClose(coniStruct *coni)
{
/*Close input file.*/
	fclose(coni->fp);
	
/*Dispose of coni structure memory.*/
	delete (coni);
}

/***************************************************
coniDouble:
	Usage is identical to coniStr, except
converts returned value to floating-point number.
*/
double coniDouble(coniStruct *coni,char *desiredParam,int *err)
{
	double ret;
	char *str=coniStr(coni,desiredParam,err);
	if ((err!=NULL)&&(*err!=0)) return 0.0;
	if (1!=sscanf(str,"%lg",&ret))
	{
		if (err==NULL)
		{
			printf("ERROR! Couldn't convert '%s' to a float.\n",str);
			exit(1);
		} else
			*err=CONI_NOTDOUBLE;
	}
	return ret;
}
/***************************************************
coniInt:
	Usage is identical to coniStr, except
converts returned value to an integer.
*/
int coniInt(coniStruct *coni,const char *desiredParam,int *err)
{
	int ret;
	char *str=coniStr(coni,desiredParam,err);
	if ((err!=NULL)&&(*err!=0)) return 0;
	if (1!=sscanf(str,"%i",&ret))
	{
		if (err==NULL)
		{
			printf("ERROR! Couldn't convert '%s' to an integer.\n",str);
			exit(1);
		} else
			*err=CONI_NOTINT;
	}
	return ret;
}
/***************************************************
coniStr:
	Extracts the given string from the
given coni file.  Usage:
To extract the string "123" from file "g.gran",
with listing:
granule {
	bob: 0
	bill: 2
	thing {
		subThing {
			value: 123
		}
	}
}
use:
coni=coniOpen("g.gran","r");
coniStr(coni,"granule.thing.subThing.value:",NULL);
coniClose(coni);
*/
void coniPush(coniStruct *coni,const char *structName)
	{strcpy(coni->name[++coni->depth],structName);}
char *coniPeek(coniStruct *coni)
	{return coni->name[coni->depth];}
void coniPop(coniStruct *coni)
{
	coni->depth--;
	if (coni->depth<0)
	{
		printf("ERROR!  Too many '}' by line %d of file '%s'!\n",
			coni->lineNo,coni->fName);
		exit(1);
	}
}
enum searchMode {
	findStruct=1, // Look up a bare struct name; return argument
	findValue=2 // Look up a fully-qualified field name; return heap-allocated
};
char *coniStr_priv(coniStruct *coni,const char *desired,searchMode mode)
{
	char line[255];/*Input buffer.*/
/*Loop through all lines in input file, looking for desiredParam.*/
	while (NULL!=fgets(line,255,coni->fp))
	{
		coni->lineNo++;
		strtok(line,"#");/*Hack comments off the end of the line.*/
		if (line[0]=='#') continue;/*Skip comments at beginning of line.*/
		
		if (strchr(line,'{')!=NULL)
		{ /* When opening a new structure, we "push" the name of the old structure, saving it.*/
			char newBase[255];/*The base name of the new structure.*/
			char newStruct[255];/*The fully qualified name of the new structure.*/
			sscanf(line,"%s",newBase);/*Extract the new structure name*/
			strcpy(newStruct,coniPeek(coni));/*Start with the old name*/
			strcat(newStruct,newBase);/*Append the new base name.*/
			strcat(newStruct,".");/*Append a period.*/
			coniPush(coni,newStruct);/*Push the resulting name.*/
			if (mode==findStruct && 0==strcmp(newBase,desired)) 
				return (char *)desired;
		} 
		else if (strchr(line,'}')!=NULL)
		/* A structure closed, so we just do a "pop", restoring the old structure.*/
			coniPop(coni);
		else if (mode==findValue)
		{ /* This line just describes a parameter.*/
			char thisBase[255];/*This parameter base name.*/
			char thisParam[255];/*Fully qualified struct name.*/
			sscanf(line,"%s",thisBase);
			strcpy(thisParam,coniPeek(coni));
			strcat(thisParam,thisBase);
			if (0==strcmp(thisParam,desired))
			{/*We found the desired parameter!*/
				int end;
				char *start;
				char *ret=(char *)MALLOC(sizeof(char)*255);
				
				start = strchr(line,':');
				start++; start++;/*Skip over ':' and space.*/
				strcpy(ret,start);
				end=strlen(ret)-1;
				while (ret[end]==' ') ret[end--]=0;/*Remove spaces from end of string.*/
				return ret;
			}
		}
	}
/*If we haven't returned by now, the specified field isn't there.  Reset the file to the start. */
	coniReopen(coni);
	return NULL;
}

char * coniStr(coniStruct *coni,const char *desiredParam,int *err)
{
	char *ret;
	
	if (coni->dir!=asciiIn)
		{printf("ERROR!  You cannot read from the output file '%s'!\n",coni->fName);exit(1);}
		
	if (err!=NULL) 
		*err=CONI_OK;

/*First, run down the remainder of the file.*/
	ret=coniStr_priv(coni,desiredParam,findValue);
	if (ret) return ret;

/*If we didn't find it the first time, seek back to the beginning of the file
	and try one more time-- the field could be near the beginning of the file.*/
	ret=coniStr_priv(coni,desiredParam,findValue);
	if (ret) return ret;
	
/*If we haven't returned by now, the specified field isn't in the file.*/
	if (err==NULL)
	{/*User doesn't care about errors.  Print our own message and quit.*/
		printf("ERROR!\n"
			"Couldn't find field '%s' in\n"
			"coni file '%s'.\n",desiredParam,coni->fName);
		exit(1);
	} else
		*err=CONI_MISSING;
	return NULL;
}

/***********************************************
coniWrite:
	Writes the given parameter, name, value, and comment 
prettily to the given coni file.
Maintains the line count and indentation.
Note that unlike coniStr, above, param should be an un-qualified
name ("slant:", not "geo.slant:").

Ex:
	coniWrite(coni,"geo {","","Geolocation-related parameters");
	coniWrite(coni,"slant:","832000","Slant range to first pixel, in meters.");
	coniWrite(coni,"}","","end geo");
writes out
geo {                               ; Geolocation-related parameters
	slant: 832000                     ; Slant range to first pixel, in meters.
}                                   ; end geo
*/
void coniWrite(coniStruct *coni,const char *param,const char *value,const char *comment)
{
	int i;
	
	if (coni->dir!=asciiOut)
		{printf("ERROR!  You cannot write to the input file '%s'!\n",coni->fName);exit(1);}
	
/*Deal with indentation.*/
	for (i=0;i<coni->depth;i++)
		fprintf(coni->fp,"    ");/*Indent the appropriate number of spaces.*/

/*Append parameter and value.*/
	if (param!=NULL) {
		const char *mid=":";
		if (value==0) {mid=" "; value="";}
		/* figure out how many spaces are needed to keep the edge straight */
		int nSpaces=40-(strlen(param)+strlen(value));
		fprintf(coni->fp,"%s%s %s %-*s",param,mid,value,nSpaces," ");
	}
	
/*Add the comment.*/
	if (comment!=NULL)
	{
		fprintf(coni->fp," # %s",comment);
	}

/*Finally, write the line to the file.*/
	fprintf(coni->fp,"\n");
	coni->lineNo++;
}

/**********************************************************************
coniIO routines:
	These routines read or write the given parameters to/from the 
CONI file, depending on how coniOpen was called (the direction).
This way, we can have just one routine that maps both ways between
a structure in a CONI file and a structure in memory. See
asf_meta/meta_coni.c for an example.
*/
void coniIO_str(coniStruct *coni,const char *name,char *value,int valueSize,const char *comment)
{
	if (coni->dir==asciiOut) {
		coniWrite(coni,name,value,comment);
	} 
	else
	{
		const char *inStr=coniStr(coni,
			(coni->loc+name+":").c_str(),NULL);
		if ((int)strlen(inStr)<valueSize) 
		{ /* return string is long enough for value and NUL */
			strcpy(value,inStr);
		} else { /* File value is too big for return string! */
			printf("Value in CONI input file too big for input!\n");
			exit(1);
		}
		free((void *)inStr);
	}
}
int coniIO_structProbe(coniStruct *coni,const char *name) {
	return coniStr_priv(coni,name,findStruct)?1:0;
}
void coniIO_structOpen(coniStruct *coni,const char *name,const char *comment)
{
	if (coni->dir==asciiOut) {
		coniWrite(coni,(std::string(name)+" {").c_str(),0,comment);
	}
	
	/* Append this struct's name to our location */
	coni->locLength[coni->depth]=coni->loc.size();
	coni->depth++;
	coni->loc+=name;
	coni->loc+=".";
}
void coniIO_structClose(coniStruct *coni,const char *name,const char *comment)
{
	coni->depth--;
	int len=coni->locLength[coni->depth]; /* length of remaining substring */
	std::string lastName=coni->loc.substr(len,coni->loc.size()-1-len);
	if (lastName!=name) {
		printf("coniIO_structClose mismatch!  Expected end of struct '%s', but got end of struct '%s'!\n",
			lastName.c_str(),name);
		exit(1);
	}
	coni->loc=coni->loc.substr(0,len); /* clip off name and dot */
	if (coni->dir==asciiOut) {
		coniWrite(coni,"}",0,comment);
	}
	
}
void coniIO_int(coniStruct *coni,const char *name,int &value,const char *comment)
{
	char buf[255];
	if (coni->dir==asciiOut)
		sprintf(buf,"%i",(int)value);
	coniIO_fixstr(coni,name,buf,comment);
	if (coni->dir==asciiIn)
		sscanf(buf,"%i",&value);
}
void coniIO_double(coniStruct *coni,const char *name,double &value,const char *comment)
{
	char buf[255];
	if (coni->dir==asciiOut)
		sprintf(buf,"%-16.11g",(double)value);
	coniIO_fixstr(coni,name,buf,comment);
	if (coni->dir==asciiIn)
		sscanf(buf,"%lg",&value);
}
void coniIO_char(coniStruct *coni,const char *name,char &value,const char *comment)
{
	char buf[255];
	if (coni->dir==asciiOut)
		sprintf(buf,"%c",(char)value);
	coniIO_fixstr(coni,name,buf,comment);
	if (coni->dir==asciiIn)
		sscanf(buf,"%c",&value);
}
