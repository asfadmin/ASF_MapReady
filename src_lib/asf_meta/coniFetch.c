/*****************************************
coniFetch:
	A collection of utilies for extracting various
data from VEXCEL Level-0 Granule files.

	Note that these routines allocate their
return strings in the heap, so their strings must
be free'd afterward, or memory will leak.
*/
#include "asf.h"
#include "coniFetch.h"


/**************************************************
coniOpen:
	Opens the given CONI file, and prepares a coniStruct.
*/
/*Private definition of the real coniStruct.*/
typedef struct {
	int depth;/*Current level of name stack.*/
	char name[16][256];/*Stack of nested structure names.*/
	int lineNo;/*Current line number in file.*/
	FILE *fp;/*CONI file pointer.*/
	char fName[255];/*CONI file name. (for error messages)*/
	coniDir dir;/*Direction-- can be ascii input or ascii output.*/
} real_coniStruct;

/*Private initialization routine*/
void coniInit(real_coniStruct *coni,const char *fName,coniDir dir)
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
	real_coniStruct *coni=(real_coniStruct *)MALLOC(sizeof(real_coniStruct));
/*Initialize and return.*/
	coniInit(coni,fName,dir);
	return (coniStruct *)coni;
}
void coniReopen(coniStruct *coniN)
{
	real_coniStruct *coni=(real_coniStruct *)coniN;
	
	fclose(coni->fp);
	coniInit(coni,coni->fName,coni->dir);
	
}
/**************************************************
coniClose:
	Closes and de-allocates the given coniStruct.
*/
void coniClose(coniStruct *coniN)
{
	real_coniStruct *coni=(real_coniStruct *)coniN;

/*Close input file.*/
	fclose(coni->fp);
	
/*Dispose of coni structure memory.*/
	free(coni);
}

/***************************************************
coniDouble:
	Usage is identical to coniStr, except
converts returned value to floating-point number.
*/
double coniDouble(coniStruct *coniN,char *desiredParam,int *err)
{
	double ret;
	char *str=coniStr(coniN,desiredParam,err);
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
int coniInt(coniStruct *coniN,char *desiredParam,int *err)
{
	int ret;
	char *str=coniStr(coniN,desiredParam,err);
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
void coniPush(real_coniStruct *coni,char *structName)
	{strcpy(coni->name[++coni->depth],structName);}
char *coniPeek(real_coniStruct *coni)
	{return coni->name[coni->depth];}
void coniPop(real_coniStruct *coni)
{
	coni->depth--;
	if (coni->depth<0)
	{
		printf("ERROR!  Too many '}' by line %d of file '%s'!\n",
			coni->lineNo,coni->fName);
		exit(1);
	}
}
char *coniStr_priv(real_coniStruct *coni,char *desiredParam)
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
		} 
		else if (strchr(line,'}')!=NULL)
		/* A structure closed, so we just do a "pop", restoring the old structure.*/
			coniPop(coni);
		else
		{ /* This line just describes a parameter.*/
			char thisBase[255];/*This parameter base name.*/
			char thisParam[255];/*Fully qualified struct name.*/
			sscanf(line,"%s",thisBase);
			strcpy(thisParam,coniPeek(coni));
			strcat(thisParam,thisBase);
			if (0==strcmp(thisParam,desiredParam))
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
/*If we haven't returned by now, the specified field isn't there.*/
	return NULL;
}

char * coniStr(coniStruct *coniN,char *desiredParam,int *err)
{
	char *ret;
	real_coniStruct *coni=(real_coniStruct *)coniN;
	
	if (coni->dir!=asciiIn)
		{printf("ERROR!  You cannot read from the output file '%s'!\n",coni->fName);exit(1);}
		
	if (err!=NULL) 
		*err=CONI_OK;

/*First, run down the remainder of the file.*/
	ret=coniStr_priv(coni,desiredParam);
	if (ret) return ret;

/*If we didn't find it the first time, seek back to the beginning of the file
	and try again-- the field could be near the beginning of the file.*/
	coniReopen(coniN);
	ret=coniStr_priv(coni,desiredParam);
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
void coniWrite(coniStruct *coniN,char *param,char *value,char *comment)
{
	int i;
	char line[255];/*The line to be written to the file.*/
	real_coniStruct *coni=(real_coniStruct *)coniN;
	strcpy(line,"");
	
	if (coni->dir!=asciiOut)
		{printf("ERROR!  You cannot write to the input file '%s'!\n",coni->fName);exit(1);}
	
/*Deal with indentation.*/
	if (strchr(param,'}'))/*If the string has a closing brace, indent less.*/
		coni->depth--;
	
	for (i=0;i<coni->depth;i++)
		strcat(line,"    ");/*Indent the appropriate number of spaces.*/

/*Append parameter and value.*/
	strcat(line,param);/*Append parameter name*/
	strcat(line," ");
	strcat(line,value);/*Append parameter value.*/

	if (comment!=NULL)
	{
	/*Space over to the comment section.*/
		i=strlen(line);
		while (i<32+coni->depth*4) /*Fill spaces out to about column 40.*/
			line[i++]=' ';
		line[i++]=0;/*Append trailing NULL.*/
	
	/*Add the comment.*/
		strcat(line," # "); /*Signal beginning of comment.*/
		strcat(line,comment);/*Append comment.*/
	}

/*More indentation.*/	
	if (strchr(param,'{'))/*If the string has an opening brace, indent more.*/
		coni->depth++;

/*Finally, write the line to the file.*/
	fprintf(coni->fp,"%s\n",line);
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
#define coniDir (((real_coniStruct *)coni)->dir)
void coniIO_str(coniStruct *coniN,char *loc,char *name,char *value,char *comment)
{
	real_coniStruct *coni=(real_coniStruct *)coniN;
	if (coniDir==asciiOut)
		coniWrite(coniN,name,value,comment);
	else
	{
		char paramName[255];
		char *inStr;
		strcpy(paramName,loc);
		strcat(paramName,name);
		inStr=coniStr(coniN,paramName,NULL);
		strcpy(value,inStr);
		free(inStr);
	}
}
void coniIO_structOpen(coniStruct *coni,char *name,char *comment)
{
	if (coniDir==asciiOut)
		coniIO_str(coni,"",name,"",comment);
}
void coniIO_structClose(coniStruct *coni,char *comment)
{
	if (coniDir==asciiOut)
		coniIO_str(coni,"","}","",comment);
}
void coniIO_int(coniStruct *coni,char *loc,char *name,int *value,char *comment)
{
	char buf[255];
	if (coniDir==asciiOut)
		sprintf(buf,"%i",*value);
	coniIO_str(coni,loc,name,buf,comment);
	if (coniDir==asciiIn)
		sscanf(buf,"%i",value);
}
void coniIO_double(coniStruct *coni,char *loc,char *name,double *value,char *comment)
{
	char buf[255];
	if (coniDir==asciiOut)
		sprintf(buf,"%-16.11g",*value);
	coniIO_str(coni,loc,name,buf,comment);
	if (coniDir==asciiIn)
		sscanf(buf,"%lg",value);
}
void coniIO_char(coniStruct *coni,char *loc,char *name,char *value,char *comment)
{
	char buf[255];
	if (coniDir==asciiOut)
		sprintf(buf,"%c",*value);
	coniIO_str(coni,loc,name,buf,comment);
	if (coniDir==asciiIn)
		sscanf(buf,"%c",value);
}
