#include "asf.h"
#include "meta_fetch.h"
#include <ctype.h>
#include <assert.h>

int check_parameters(char *inFile)
{
  int n=0;
  int structDepth, indexVec=0;
  char line[255];/*Input buffer.*/
  char structName[16][255], *start;
  FILE *in = FOPEN(inFile, "r");
				    
  /*Initially, stack contains only the empty string.*/
  structDepth=0;
  strcpy(structName[structDepth],"");
  
  /*Loop through all lines in input file, looking for desiredParam.*/
  while (NULL!=fgets(line,255,in))
    {
      if (line[0] == '#')
	/* Just a comment line */
	;
      else if (strchr(line,'{')!=NULL)
	{ /* When opening a new structure, we "push" the name 
	     of the old structure, saving it.*/
	  char newStruct[255];/* The base name of the new structure.*/
	  char index[255];/* The index (e.g. "[2]"), or the empty string.*/
	  sscanf(line,"%s",newStruct);
	  if (0==strcmp(newStruct,"vector")) {
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
	  /*	printf("  Parsing depth: '%s'\n",
		structName[structDepth]); */
	} 
      else if (strchr(line,'}')!=NULL)
	/* When a structure closes, we just do a struct "pop", 
	   restoring the old structure.*/
	structDepth--;
      else {
	start = strchr(line,':');
	if (start != NULL)
	  n++; /* is just a parameter.*/
      }
    }
  
  FCLOSE(in);

  return n;
}

/*******************************************************************************
 * extract_meta_parameters:
 * Reads a metadata file. Fills the parameters and their values into a generic 
 * parameter structure */
meta_parameter_t *extract_meta_parameters(char *inFile)
{
  meta_parameter_t *meta=NULL;
  int structDepth, indexVec=0, n=1, num;
  char line[255], structName[16][255], thisParamName[255], thisParam[255];
  char *start, newStruct[255], index[255];
  FILE *in=FOPEN(inFile, "r");

  /* Check how many parameters to be read and allocate memory for structure */
  num = check_parameters(inFile);
  meta = (meta_parameter_t *) MALLOC(sizeof(meta_parameter_t));
  meta->number = num+1; /* Reserve a parameter for stats flag */
  meta->parameter = (char **) MALLOC((num+1)*255*sizeof(char));
  meta->value = (char **) MALLOC((num+1)*100*sizeof(char));
				    
  /* Initially, stack contains only the empty string. */
  structDepth=0;
  strcpy(structName[structDepth],"");
  (meta->parameter)[0] = (char *) MALLOC(255*sizeof(char));
  strcpy((meta->parameter)[0], "stats:");
  (meta->value)[0] = (char *) MALLOC(255*sizeof(char));
  strcpy((meta->value)[0], "0");
  
  /*Loop through all lines in input file */
  while (NULL!=fgets(line,255,in))
    {
      if (line[0] == '#')
	/* Just a comment line */
	;
      else if (strchr(line,'{')!=NULL)
	{ /* When opening a new structure, we "push" the name 
	     of the old structure, saving it.*/
	  sscanf(line,"%s",newStruct);
	  if (0==strcmp(newStruct,"vector")) {
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
	} 
      else if (strchr(line,'}')!=NULL)
	/* When a structure closes, we just do a struct "pop", 
	   restoring the old structure.*/
	structDepth--;
      else
	{ /* is just a parameter.*/
	  sscanf(line,"%s",thisParamName);
	  strcpy(thisParam,structName[structDepth]);
	  if (strcmp(thisParam, "stats") == 0)
	    sprintf((meta->value)[0], "1");
	  if (structDepth!=0)
	    strcat(thisParam,".");
	  strcat(thisParam,thisParamName);
	  start = strchr(line,':');
	  if (start != NULL) {
	    (meta->parameter)[n] = (char *) MALLOC(255*sizeof(char));
	    strcpy((meta->parameter)[n], thisParam);
	    /*	    printf("parameter[%i]: %s\n", n, (meta->parameter)[n]);*/
	    start++;
	    while (isspace(*start)) start++;
	    (meta->value)[n] = (char *) MALLOC(255*sizeof(char));
	    strcpy((meta->value)[n], start);
	    /*	    printf("value[%i]: %s", n, (meta->value)[n]);*/
	    n++;
	  }
	}
     }

  FCLOSE(in);

  return meta;
}

/*******************************************************************************
 * read_meta_checks:
 * Reads a check file. Allocates the memory for the check structure. Fills the 
 * parameters, their criticality, their valid value range and quality thresholds 
 * levels into a meta check parameter structure. It returns the structure with 
 * the number of parameters read */
void read_meta_checks(char *inFile, meta_check_t *meta_check)
{
  FILE *fp;
  char line[1000];
  int ii, num=0, ret;

  /* Determine the number of the number of checks */
  fp = FOPEN(inFile, "r");
  while (fgets(line, 1000, fp))
    num++;
  FCLOSE(fp);

  /* Allocate memory for structure */
  meta_check->number = num;
  meta_check->parameter = (char **) MALLOC(num*255*sizeof(char));
  meta_check->check = (char **) MALLOC(num*5*sizeof(char));
  meta_check->value = (char **) MALLOC(num*100*sizeof(char));
  meta_check->range_low = (double *) MALLOC(num*sizeof(double));
  meta_check->range_high = (double *) MALLOC(num*sizeof(double));
  meta_check->moderate = (double *) MALLOC(num*sizeof(double));
  meta_check->bad = (double *) MALLOC(num*sizeof(double));

  /* Read the check parameters into structure */
  fp = FOPEN(inFile, "r");
  fgets(line, 1000, fp);
  num = atoi(line);
  for (ii=0; ii<num; ii++) {
    fgets(line, 1000, fp);
    ret = sscanf(line, "%s\t%s\t%lf\t%lf\t%lf\%lf",
           (meta_check->parameter)[ii], (meta_check->check)[ii],
           &((meta_check->range_low)[ii]), &((meta_check->range_high)[ii]),
           &((meta_check->moderate)[ii]), &((meta_check->bad)[ii]));
    assert(ret == 6);
  }
  FCLOSE(fp);

}
