/*****************************************
lzFetch:
	A collection of utilies for extracting various
data from VEXCEL Level-0 Granule files.

	Note that these routines allocate their
return strings in the heap, so their strings must
be free'd afterward, or memory will leak.
*/

#include "asf.h"
#include "lzFetch.h"
#include <math.h>

/***************************************************
lzDouble:
	Usage is identical to lzStr, except
converts returned value to floating-point number.
*/
double lzDouble(char *lzN,char *desiredParam,int *err)
{
	double ret;
	char *str=lzStr(lzN,desiredParam,err);
	if ((err!=NULL)&&(*err!=0)) return 0.0;
	if (1!=sscanf(str,"%lf",&ret))
	{
		if (err==NULL)
		{
			sprintf(errbuf,"   ERROR: Couldn't convert '%s' to a float.\n",str);
			printErr(errbuf);
		} else
			*err=2;
	}
	return ret;
}
/***************************************************
lzInt:
	Usage is identical to lzStr, except
converts returned value to an integer.
*/
int lzInt(char *lzN,char *desiredParam,int *err)
{
	int ret;
	char *str=lzStr(lzN,desiredParam,err);
	if ((err!=NULL)&&(*err!=0)) return 0;
	if (1!=sscanf(str,"%d",&ret))
	{
		if (err==NULL)
		{
			sprintf(errbuf,"   ERROR: Couldn't convert '%s' to an integer.\n",str);
			printErr(errbuf);
		} else
			*err=3;
	}
	return ret;
}
/***************************************************
lzStr:
	Extracts the given string from the
given level-zero file.  Usage:
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
lzStr("g.gran","granule.thing.subThing.value:",NULL);
*/
char * lzStr(char *lzN,char *desiredParam,int *err)
{
/*These two variables form a "stack of structures",
which we use to keep track of where we are in the file's structure.*/
	int structDepth, indexVec=0;
	char structName[16][255];
	char line[255];/*Input buffer.*/
	FILE *in=fopen(lzN,"r");
	
	if (in==NULL)
	{/*Couldn't open file.*/
		if (err==NULL) {
		  sprintf(errbuf,"   ERROR: Couldn't open VEXCEL Level-Zero file '%s'!\n",lzN);
		  printErr(errbuf);
		}
		else
			{*err=3;return NULL;}
	}

/*Initially, stack contains only the empty string.*/
	structDepth=0;
	strcpy(structName[structDepth],"");
	
/*Loop through all lines in input file, looking for desiredParam.*/
	while (NULL!=fgets(line,255,in))
	{
		if (strchr(line,'{')!=NULL)
		{ /* When opening a new structure, we "push" the name of the old structure, saving it.*/
			char newStruct[255];/*The base name of the new structure.*/
			char index[255];/*The index (e.g. "[2]"), or the empty string.*/
			sscanf(line,"%s",newStruct);
			if (0==strcmp(newStruct,"location"))
			{/*the new structure is an indexable structure, like a beam or location.*/
				char throwaway[255];
				int indexNo;
				fgets(line,255,in);/*Get the index # from the next line.*/
				sscanf(line,"%s %d",throwaway,&indexNo);
				sprintf(index,"[%d]",indexNo);
			} 
			else if (0==strcmp(newStruct,"state_vector")) {
				sprintf(index,"[%d]",indexVec);
				indexVec++;
			}
			else strcpy(index,"");
			strcpy(structName[structDepth+1],structName[structDepth]);
			if (structDepth!=0)
				strcat(structName[structDepth+1],".");
			strcat(structName[structDepth+1],newStruct);
			strcat(structName[structDepth+1],index);
			structDepth++;
			/*printf("  Parsing depth: '%s'\n",structName[structDepth]);*/
		} else if (strchr(line,'}')!=NULL)
		/* When a structure closes, we just do a struct "pop", restoring the old structure.*/
			structDepth--;
		else
		{ /* is just a parameter.*/
			char thisParamName[255];
			char thisParam[255];/*Fully qualified struct name.*/
			sscanf(line,"%s",thisParamName);
			strcpy(thisParam,structName[structDepth]);
			if (structDepth!=0)
				strcat(thisParam,".");
			strcat(thisParam,thisParamName);
			if (0==strcmp(thisParam,desiredParam))
			{/*We found it!*/
				char *start;
				char *ret=(char *)malloc(sizeof(char)*255);
				/*sscanf(line,"%s %s",thisParamName,ret);*/
				start = strchr(line,':');
				start++; start++;
				strcpy(ret,start);
				if (err!=NULL)
					*err=0;
				FCLOSE(in);
				return ret;
			}
		}
	}
	
/*If we haven't returned by now, the specified field isn't there.*/
	if (err==NULL)
	{/*User doesn't care about errors.  Print our own message and quit.*/
		sprintf(errbuf,"   ERROR: Couldn't find field '%s' in\n"
		"   granule file '%s'.\n",desiredParam,lzN);
		printErr(errbuf);
	} 
	*err=1;
	return NULL;
}
