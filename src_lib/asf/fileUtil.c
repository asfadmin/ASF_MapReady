/*****************************************
FileUtil:
	A collection of file I/O utilities.
*/

#include <assert.h>

#include "asf.h"
#include "asf_endian.h"

int extExists(const char *name,const char *newExt)
{
	char *fName=appendExt(name,newExt);
	int exists=fileExists(fName);
	free((void *)fName);
	return exists;
}

int fileExists(const char *name)
{
	FILE *f=fopen(name,"r");
	if (f==NULL)
		return 0;
	fclose(f);
	return 1;
}

char *findExt(char *name)
{
	int i;
	i=strlen(name)-1;/*Start at end of name.*/
	while ((i>0) && (name[i]!='.') && (name[i]!='/'))
		i--;/*Work backwards until we hit a directory separator or extension separator.*/
	
	if ((i>0)&&(name[i]=='.'))
	/*We found an extension!*/
		return &name[i];
	else
	/*We couldn't find an extension.*/
		return NULL;
}
char *appendExt(const char *name,const char *newExt)
{
	char *ext, *ret = (char *) MALLOC(sizeof(char) 
	    * (MAX_APPENDEXT_RESULT_STRING_LENGTH + 1));

	assert(strlen(name) <= MAX_APPENDEXT_RESULT_STRING_LENGTH);
	  
	strcpy(ret,name);
	
	if ( newExt == NULL )
		return ret;
	
	ext=findExt(ret);

	if ( ext != NULL )
		/* We found an existing extension...  */
		*ext = 0;	/* Clip it off.  */

	assert(strlen(ret) + strlen(newExt) 
	       <= MAX_APPENDEXT_RESULT_STRING_LENGTH);

	strcat(ret, newExt);	/* Put new extension on the end.  */

	return ret;
}
void create_name(char *out,const char *in,const char *newExt)
{
	char *ext;
	strcpy(out,in);
	ext=findExt(out);
	if (ext!=NULL)
		*ext=0;
	strcat(out,newExt);
}

FILE *fopenImage(const char *fName,const char *access)
{
	int forWriting=0;
	FILE *fRet=NULL;
	char *openName=NULL;
        /* Check to make sure that the (bone-headed) user didn't ask
           us to open a .ddr image or .meta file.  findExt may return
           a pointer into its argument string, so fName isn't const
           anymore.  */
	char *ext=findExt( (char *) fName);
	if (NULL!=ext)
	{
	  if ( (0==strcmp(ext,".ddr")) || (0==strcmp(ext,".meta")))
	    ext[0]=0;/*Clip off stupid extention-- will append .img later*/
	  else if ( ((0==strcmp(ext,".D"))||(0==strcmp(ext,".L"))) && access[0]=='w')
	    ext[0]=0;/*Clip off stupid extention-- will append .img later*/
	}

/*Find the file's actual name, with the correct extension.*/
	if (access[0]=='w')
	{/*We're opening for writing-- we have to be conservative.*/
		forWriting=1;
		if (NULL==findExt((char *)fName))/*If there is no extension,*/
			openName=appendExt(fName,".img");/*Append .img*/
		else /*There was an extension, so */
			openName=appendExt(fName,NULL);/*Do nothing to the name.*/
	} 
	else
	{/*We're opening for reading-- search many extensions for the image.*/
		char *extTable[]={".img",".dem",".ht",".coh",NULL};
		int extNo=-1;
		do
		{
			char *ext=(extNo==-1)?NULL:extTable[extNo];
			if (extExists(fName,ext))
			{/*We found a file with the given basename and extension!*/
				openName=appendExt(fName,ext);
				break;/*Once we find one, we're done.*/
			}
			extNo++;
		}
		while (extTable[extNo]!=NULL);
	}
/*Try to open this name.*/
	if (NULL!=openName)
	{
		fRet=fopen(openName,access);
		if (fRet!=NULL)
		{/*We've sucessfully opened the file.*/
			free(openName);/*Free the name.*/
			return fRet;/*Return the file pointer.*/
		}
	}
/*An error occured-- tell the user and quit.*/
	fprintf(stderr,
			"********************** ERROR! ***********************\n"
			"An error occured trying to open for %s the\n"
			"file named '%s'.\n\n",forWriting?"write or create":"reading",
			fName);
	if (forWriting)
		fprintf(stderr,
			"This could be because you don't have write permissions\n"
			"in this directory, because your disk quota is full, or \n"
			"because your disk is full.\n");
	else /*for reading*/
		fprintf(stderr,
				"This could be because the file doesn't exist in this\n"
				"directory, or you don't have read permissions for the\n"
				"file.  I even searched for common image extensions.\n");
	exit(102);
	return NULL;
}

/***********************************************************************************
 * Get a single line of data in floating point format, performing rounding, padding,
 * and endian conversion as needed.  The line_number argument is the zero-indexed
 * line number to get.  The dest argument must be a pointer to existing memory.   */
int get_float_line(FILE *file, meta_parameters *meta, int line_number, float *dest)
{
  int ii;               /* Sample index.  */
  int samples_gotten;   /* Number of samples retrieved */
  size_t sample_size;   /* Sample size in bytes.  */
  void *temp_buffer;    /* Buffer for unconverted data.  */
  int sample_count = meta->general->sample_count;
  int data_type    = meta->general->data_type;

  assert(line_number <= meta->general->line_count - 1);

  /* Determine sample size.  */
  switch (data_type) {
    case BYTE:      sample_size = 1; break;
    case INTEGER16: sample_size = 2; break;
    case INTEGER32: sample_size = 4; break;
    case REAL32:    sample_size = 4; break;
    case REAL64:    sample_size = 8; break;
    default:
      printf("get_float_line: Unrecognized data type. Value must be BYTE, INTEGER16,\n"
             "                INTEGER32, REAL32, or REAL64... Exiting program.\n");
      exit(EXIT_FAILURE);
  }

  /* Scan to the beginning of the line.  */
  FSEEK64(file, sample_size*sample_count*line_number, SEEK_SET);

  temp_buffer = MALLOC( (size_t) sample_size * sample_count);
  
  samples_gotten = FREAD(temp_buffer, sample_size, sample_count, file);

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<sample_count; ii++ )
        dest[ii] = *( (uint8_t *) temp_buffer + ii);
      break;
    case INTEGER16:
      for ( ii=0; ii<sample_count; ii++ ) {
        big16(*((int16_t *)temp_buffer + ii));
        dest[ii] = *( (int16_t *) temp_buffer + ii);
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<sample_count; ii++ ) {
        big32(*((int32_t *)temp_buffer + ii));
        dest[ii] = *( (int32_t *) temp_buffer + ii);
      }
      break;
    case REAL32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ieee_big32(*((float*)temp_buffer + ii));
        dest[ii] = *( (float *) temp_buffer + ii);
      }
      break;
    case REAL64:
      for ( ii=0; ii<sample_count; ii++ ) {
        ieee_big64(*((double*)temp_buffer + ii));
        dest[ii] = *( (double *) temp_buffer + ii);
      }
  }

  FREE(temp_buffer);
  return samples_gotten;
}

/***********************************************************************************
 * Put a single line of data in floating point format, performing rounding, padding,
 * and endian conversion as needed.  The line_number argument is the zero-indexed
 * line number to get.  The dest argument must be a pointer to existing memory.   */
int put_float_line(FILE *file, meta_parameters *meta, int line_number, const float *source)
{
  int ii;               /* Sample index.                       */
  int samples_put;      /* Number of samples written           */
  size_t sample_size;   /* Sample size in bytes.               */
  void *out_buffer;     /* Buffer of converted data to write.  */
  int sample_count = meta->general->sample_count;
  int data_type    = meta->general->data_type;

  assert(line_number <= meta->general->line_count - 1);

  /* Determine sample size.  */
  switch (data_type) {
    case BYTE:      sample_size = 1; break;
    case INTEGER16: sample_size = 2; break;
    case INTEGER32: sample_size = 4; break;
    case REAL32:    sample_size = 4; break;
    case REAL64:    sample_size = 8; break;
    default:
      printf("put_float_line: Unrecognized data type. Value must be BYTE, INTEGER16,\n"
             "                INTEGER32, REAL32, or REAL64... Exiting program.\n");
      exit(EXIT_FAILURE);
  }

  out_buffer = MALLOC( (size_t) sample_size * sample_count);

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<sample_count; ii++ )
        ((unsigned char *)out_buffer)[ii] = source[ii];
      break;
    case INTEGER16:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((short*)out_buffer)[ii] = source[ii];
        big16( ((short*)out_buffer)[ii] );
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((int*)out_buffer)[ii] = source[ii];
        big32( ((int*)out_buffer)[ii] );
      }
      break;
    case REAL32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((float*)out_buffer)[ii] = source[ii];
        ieee_big32( ((float*)out_buffer)[ii] );
      }
    case REAL64:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((double*)out_buffer)[ii] = source[ii];
        ieee_big64( ((double*)out_buffer)[ii] );
      }
      break;
  }

  FSEEK64(file, sample_size*sample_count*line_number, SEEK_SET);
  samples_put = FWRITE(out_buffer, sample_size, sample_count, file);

  FREE(out_buffer);
  return samples_put;
}
