/*******************************************************************************
 * meta_fetch:
 *	A collection of utilies for extracting various data from metadata files
 *    and generic parameters structures.
 ******************************************************************************/
#include <ctype.h>
#include <stdio.h>
#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "meta_fetch.h"
#include <math.h>


/*******************************************************************************
 * metaDouble:
 * Searches through generic parameter structure and returns double value 
 * if parameter is found in the structure */
double metaDouble(meta_parameter_t *meta, char *desiredParam)
{
  int ii;
  double ret = MAGIC_UNSET_DOUBLE;

  for (ii=0; ii<meta->number; ii++)
    if (strcmp(meta->parameter[ii], desiredParam) == 0)
      ret = atof(meta->value[ii]);

  return ret;
}

/*******************************************************************************
 * metaInt:
 * Searches through generic parameter structure and returns integer value 
 * if parameter is found in the structure */
int metaInt(meta_parameter_t *meta, char *desiredParam)
{
  int ii;
  int ret = MAGIC_UNSET_INT;
  
  for (ii=0; ii<meta->number; ii++)
  if (strcmp(meta->parameter[ii], desiredParam) == 0)
  ret = atoi(meta->value[ii]);
  
  return ret;
}

/*******************************************************************************
 * metaChar:
 * Searches through generic parameter structure and returns char value 
 * if parameter is found in the structure */
char metaChar(meta_parameter_t *meta, char *desiredParam)
{
  int ii;
  char ret = MAGIC_UNSET_CHAR;

  for (ii=0; ii<meta->number; ii++)
    if (strcmp(meta->parameter[ii], desiredParam) == 0)
      ret = meta->value[ii][0];

  return ret;
}

/*******************************************************************************
 * metaString:
 * Searches through generic parameter structure and returns string 
 * if parameter is found in the structure */
char *metaString(meta_parameter_t *meta, char *desiredParam)
{
  int ii, kk;
  char *str;
  char *ret = (char *) MALLOC(255*sizeof(char));
  /*  char *ret = (char *) MALLOC(255*sizeof(char));*/
  for (ii=0; ii<meta->number; ii++) {
    if (strcmp(meta->parameter[ii], desiredParam) == 0) {
      for (str = meta->value[ii], kk=0; ; kk++) {
	ret[kk] = *str;
	str++;
	if (isspace(*str))
	  break;
      }
    }
  }

  return ret;
}

int is_empty(char *string)
{
  int ii;
  for (ii=0; ii<strlen(string); ii++) {
    if (!isspace(string[ii])) return 0;
  }
  return 1;
}

void meta_put_string(FILE *meta_file,char *name,char *value,char *comment)
{
  int ii;
  int malloc_flag=0;
  char line[255];/*The line to be written to the file.*/
  static int depth=0;
  strcpy(line,"");
  
  /*Deal with indentation.*/
  if (strchr(name,'}'))/*If the string has a closing brace, indent less.*/
    depth--;
  if (depth<0)
    {printf("ERROR!  Too many '}' in meta file!\n"); exit(1);}
  
  for (ii=0; ii<depth; ii++)
    strcat(line,"    ");/*Indent the appropriate number of spaces.*/
  
  /*Append parameter and value.*/
  strcat(line,name);/*Append parameter name*/
  strcat(line," ");
  if (is_empty(value) && !strchr(name,'{') && !strchr(name,'}')){
    value = (char*)MALLOC(sizeof(char)*4);
    malloc_flag=1;
    strcpy(value,MAGIC_UNSET_STRING);
  }
  strcat(line,value);/*Append parameter value.*/
  if (malloc_flag==1) {free(value);}
  
  /* Append comment if applicable */
  if (comment!=NULL)
    {
      /*Space over to the comment section.*/
      ii=strlen(line);
      while (ii < 42+depth*4) /*Fill spaces out to about column 50.*/
	line[ii++]=' ';
      line[ii++]='\0';        /*Append trailing NULL.*/
      
      /*Add the comment.*/
      strcat(line," # ");     /*Signal beginning of comment.*/
      strcat(line,comment);   /*Append comment.*/
    }
  
  /*If the string has a closing brace, append newline*/
  if (strchr(name,'}') && (depth==0))
    strcat(line,"\n");
  
  /*More indentation.*/
  if (strchr(name,'{'))/*If the string has an opening brace, indent more.*/
    depth++;

  /*Finally, write the line to the file.*/
  fprintf(meta_file,"%s\n",line);
}

void meta_put_double(FILE *meta_file,char *name,double value,char *comment)
{
  char param[64];
  sprintf(param,"%-16.11g",value);
  strtok(param," ");/*remove all trailing spaces */
  if (is_empty(param)) { strcpy(param,"NaN"); }
  meta_put_string(meta_file,name,param,comment);
}


void meta_put_int(FILE *meta_file,char *name,int value,char *comment)
{
  char param[64];
  sprintf(param,"%i",value);
  if (is_empty(param)) { strcpy(param,"-999999999"); }
  meta_put_string(meta_file,name,param,comment);
}

void meta_put_char(FILE *meta_file,char *name,char value,char *comment)
{
  char param[2];
  sprintf(param,"%c",value);
  if (is_empty(param)) { strcpy(param,"?"); }
  meta_put_string(meta_file,name,param,comment);
}

