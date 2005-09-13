/*****************************************
io_util:
	Read & write files line by line.
*/

#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_complex.h"


/*******************************************************************************
 * Return the number of bytes that a data_type is made of, kill program on
 * failure to figure the size of the data type. */
int data_type2sample_size(int data_type)
{
  /* Determine sample size.  */
  switch (data_type) {
    case BYTE:      return sizeof(unsigned char);
    case INTEGER16: return sizeof(short int);
    case INTEGER32: return sizeof(int);
    case REAL32:    return sizeof(float);
    case REAL64:    return sizeof(double);
    case COMPLEX_BYTE:      return sizeof(complexByte);
    case COMPLEX_INTEGER16: return sizeof(complexShortInt);
    case COMPLEX_INTEGER32: return sizeof(complexInt);
    case COMPLEX_REAL32:    return sizeof(complexFloat);
    case COMPLEX_REAL64:    return sizeof(complexDouble);
    default:
      printf("\ndata_type2sample_size(): Unrecognized data type. Exiting program.\n");
      exit(EXIT_FAILURE);
  }
  return 0;
}


/*******************************************************************************
 * Get x number of lines of data (any data type) and fill a pre-allocated array
 * with it. The data is assumed to be in big endian format and will be converted
 * to the native machine's format. The line_number argument is the zero-indexed
 * line number to get. The dest argument must be a pointer to existing memory.
 * Returns the amount of samples successfully read & converted. */
int get_data_lines(FILE *file, meta_parameters *meta, int line_number,
                    int num_lines_to_get, void *dest, int dest_data_type)
{
  int ii;               /* Sample index.  */
  int samples_gotten;   /* Number of samples retrieved */
  size_t sample_size;   /* Sample size in bytes.  */
  void *temp_buffer;    /* Buffer for unconverted data.  */
  int sample_count = meta->general->sample_count;
  int data_type    = meta->general->data_type;
  int num_samples_to_get = num_lines_to_get * sample_count;

  if ((data_type>=COMPLEX_BYTE) && (dest_data_type<=REAL64)) {
    printf("\nget_data_lines: Cannot put complex data into a simple data buffer. Exiting.\n\n");
    exit(EXIT_FAILURE);
  }
  if ((data_type<=REAL64) && (dest_data_type>=COMPLEX_BYTE)) {
    printf("\nget_data_lines: Cannot put simple data into a complex data buffer. Exiting.\n\n");
    exit(EXIT_FAILURE);
  }

  /* Determine sample size.  */
  sample_size = data_type2sample_size(data_type);

  /* Make sure not to go past the end of file */
  if (line_number > meta->general->line_count) {
    printf("\nget_data_lines: Cannot read line %d in a file of %d lines. Exiting.\n",
           line_number, meta->general->line_count);
    exit(EXIT_FAILURE);
  }
  if ((line_number+num_lines_to_get) > meta->general->line_count) {
    num_samples_to_get = (meta->general->line_count - line_number)
                          * sample_count;
  }

  /* Scan to the beginning of the line.  */
  FSEEK64(file, (long long)sample_size*sample_count*line_number, SEEK_SET);

  temp_buffer = MALLOC( sample_size * num_samples_to_get );

  samples_gotten = FREAD(temp_buffer, sample_size, num_samples_to_get, file);

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<samples_gotten; ii++ ) {
        switch (dest_data_type) {
         case BYTE:((unsigned char*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case INTEGER16:((short int*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case INTEGER32:((int*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case REAL32:((float*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case REAL64:((double*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
        }
      }
     break;
    case INTEGER16:
      for ( ii=0; ii<samples_gotten; ii++ ) {
        big16(((short int *)temp_buffer)[ii]);
        switch (dest_data_type) {
         case BYTE:((unsigned char*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case INTEGER16:((short int*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case INTEGER32:((int*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case REAL32:((float*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case REAL64:((double*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
        }
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<samples_gotten; ii++ ) {
        big32(((int *)temp_buffer)[ii]);
        switch (dest_data_type) {
         case BYTE:((unsigned char*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case INTEGER16:((short int*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case INTEGER32:((int*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case REAL32:((float*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case REAL64:((double*)dest)[ii] = ((int*)temp_buffer)[ii];break;
        }
      }
      break;
    case REAL32:
      for ( ii=0; ii<samples_gotten; ii++ ) {
        ieee_big32(((float*)temp_buffer)[ii]);
        switch (dest_data_type) {
         case BYTE:((unsigned char*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case INTEGER16:((short int*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case INTEGER32:((int*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case REAL32:((float*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case REAL64:((double*)dest)[ii] = ((float*)temp_buffer)[ii];break;
        }
      }
      break;
    case REAL64:
      for ( ii=0; ii<samples_gotten; ii++ ) {
        ieee_big64(((double*)temp_buffer)[ii]);
        switch (dest_data_type) {
         case BYTE:((unsigned char*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case INTEGER16:((short int*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case INTEGER32:((int*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case REAL32:((float*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case REAL64:((double*)dest)[ii] = ((double*)temp_buffer)[ii];break;
        }
      }
    case COMPLEX_BYTE:
      for ( ii=0; ii<samples_gotten*2; ii++ ) {
        switch (dest_data_type) {
         case COMPLEX_BYTE:((unsigned char*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER16:((short int*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER32:((int*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case COMPLEX_REAL32:((float*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
         case COMPLEX_REAL64:((double*)dest)[ii] = ((unsigned char*)temp_buffer)[ii];break;
        }
      }
      break;
    case COMPLEX_INTEGER16:
      for ( ii=0; ii<samples_gotten*2; ii++ ) {
        big16(((short int *)temp_buffer)[ii]);
         switch (dest_data_type) {
         case COMPLEX_BYTE:((unsigned char*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER16:((short int*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER32:((int*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case COMPLEX_REAL32:((float*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
         case COMPLEX_REAL64:((double*)dest)[ii] = ((short int*)temp_buffer)[ii];break;
        }
      }
      break;
    case COMPLEX_INTEGER32:
      for ( ii=0; ii<samples_gotten*2; ii++ ) {
        big32(((int *)temp_buffer)[ii]);
        switch (dest_data_type) {
         case COMPLEX_BYTE:((unsigned char*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER16:((short int*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER32:((int*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case COMPLEX_REAL32:((float*)dest)[ii] = ((int*)temp_buffer)[ii];break;
         case COMPLEX_REAL64:((double*)dest)[ii] = ((int*)temp_buffer)[ii];break;
        }
      }
      break;
    case COMPLEX_REAL32:
      for ( ii=0; ii<samples_gotten*2; ii++ ) {
        ieee_big32(((float*)temp_buffer)[ii]);
        switch (dest_data_type) {
         case COMPLEX_BYTE:((unsigned char*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER16:((short int*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER32:((int*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case COMPLEX_REAL32:((float*)dest)[ii] = ((float*)temp_buffer)[ii];break;
         case COMPLEX_REAL64:((double*)dest)[ii] = ((float*)temp_buffer)[ii];break;
        }
      }
      break;
    case COMPLEX_REAL64:
      for ( ii=0; ii<samples_gotten*2; ii++ ) {
        ieee_big64(((double*)temp_buffer)[ii]);
        switch (dest_data_type) {
         case COMPLEX_BYTE:((unsigned char*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER16:((short int*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case COMPLEX_INTEGER32:((int*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case COMPLEX_REAL32:((float*)dest)[ii] = ((double*)temp_buffer)[ii];break;
         case COMPLEX_REAL64:((double*)dest)[ii] = ((double*)temp_buffer)[ii];break;
        }
      }
  }

  FREE(temp_buffer);
  return samples_gotten;
}

/*******************************************************************************
 * Get one line of any non-complex data type via get_data_lines and fill a float
 * buffer with it. Returns number of samples gotten                           */
int get_float_line(FILE *file, meta_parameters *meta, int line_number,
                   float *dest)
{
  return get_data_lines(file, meta, line_number, 1, dest, REAL32);
}

/*******************************************************************************
 * Get num_lines_to_get lines of any non-complex data type via get_data_lines
 * and fill a float buffer with it. Returns number of samples gotten          */
int get_float_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_get, float *dest)
{
  return get_data_lines(file,meta,line_number,num_lines_to_get,dest,REAL32);
}

/*******************************************************************************
 * Get a single line of any non-complex data in double floating point format,
 * performing rounding, padding, and endian conversion as needed.  The
 * line_number argument is the zero-indexed line number to get.  The dest
 * argument must be a pointer to existing memory. */
int get_double_line(FILE *file, meta_parameters *meta, int line_number,
                    double *dest)
{
  return get_data_lines(file, meta, line_number, 1, dest, REAL64);
}

/*******************************************************************************
 * Get num_lines_to_get lines of any non-complex data type via get_data_lines
 * and fill a double buffer with it. Returns number of samples gotten.        */
int get_double_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_get, double *dest)
{
  return get_data_lines(file,meta,line_number,num_lines_to_get,dest,REAL64);
}

/*******************************************************************************
 * Get one line of any complex data type via get_data_lines and fill a
 * complexFloat buffer with it. Returns number of samples gotten              */
int get_complexFloat_line(FILE *file, meta_parameters *meta, int line_number,
                    complexFloat *dest)
{
  return get_data_lines(file, meta, line_number, 1, dest, COMPLEX_REAL32);
}

/*******************************************************************************
 * Get num_lines_to_get lines of any complex data type via get_data_lines
 * and fill a complexFloat buffer with it. Returns number of samples gotten   */
int get_complexFloat_lines(FILE *file, meta_parameters *meta, int line_number,
                     int num_lines_to_get, complexFloat *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get, dest,
                        COMPLEX_REAL32);
}

/*******************************************************************************
 * Write x number of lines of any data type to file in the data format specified
 * by the meta structure. It is always written in big endian format. Returns the
 * amount of samples successfully converted & written. Will not write more lines
 * than specified in the supplied meta struct. */
int put_data_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_put, const void *source,
                   int source_data_type)
{
  int ii;               /* Sample index.                       */
  int samples_put;      /* Number of samples written           */
  size_t sample_size;   /* Sample size in bytes.               */
  void *out_buffer;     /* Buffer of converted data to write.  */
  int sample_count       = meta->general->sample_count;
  int data_type          = meta->general->data_type;
  int num_samples_to_put = num_lines_to_put * sample_count;

  if ((source_data_type>=COMPLEX_BYTE) && (data_type<=REAL64)) {
    printf("\nput_data_lines: Cannot put complex data into a simple data file. Exiting.\n\n");
    exit(EXIT_FAILURE);
  }
  if ((source_data_type<=REAL64) && (data_type>=COMPLEX_BYTE)) {
    printf("\nput_data_lines: Cannot put simple data into a complex data file. Exiting.\n\n");
    exit(EXIT_FAILURE);
  }

  /* Determine sample size.  */
  sample_size = data_type2sample_size(data_type);

  /* Make sure not to make file bigger than meta says it should be */
  if (line_number > meta->general->line_count) {
    printf("\nput_data_lines: Cannot write line %d in a file that should be %d lines. Exiting.\n",
           line_number, meta->general->line_count);
    exit(EXIT_FAILURE);
  }
  if ((line_number+num_lines_to_put) > meta->general->line_count) {
    num_samples_to_put = (meta->general->line_count - line_number)
                          * sample_count;
  }

  FSEEK64(file, (long long)sample_size*sample_count*line_number, SEEK_SET);
  out_buffer = MALLOC( sample_size * sample_count * num_lines_to_put );

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<num_samples_to_put; ii++ ) {
        switch (source_data_type) {
         case BYTE:((unsigned char*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case INTEGER16:((unsigned char*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case INTEGER32:((unsigned char*)out_buffer)[ii] = ((int*)source)[ii];break;
         case REAL32:((unsigned char*)out_buffer)[ii] = ((float*)source)[ii];break;
         case REAL64:((unsigned char*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
      }
      break;
    case INTEGER16:
      for ( ii=0; ii<num_samples_to_put; ii++ ) {
        switch (source_data_type) {
         case BYTE:((short int*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case INTEGER16:((short int*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case INTEGER32:((short int*)out_buffer)[ii] = ((int*)source)[ii];break;
         case REAL32:((short int*)out_buffer)[ii] = ((float*)source)[ii];break;
         case REAL64:((short int*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        big16( ((short int*)out_buffer)[ii] );
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<num_samples_to_put; ii++ ) {
        switch (source_data_type) {
         case BYTE:((int*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case INTEGER16:((int*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case INTEGER32:((int*)out_buffer)[ii] = ((int*)source)[ii];break;
         case REAL32:((int*)out_buffer)[ii] = ((float*)source)[ii];break;
         case REAL64:((int*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        big32( ((int*)out_buffer)[ii] );
      }
      break;
    case REAL32:
      for ( ii=0; ii<num_samples_to_put; ii++ ) {
        switch (source_data_type) {
         case BYTE:((float*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case INTEGER16:((float*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case INTEGER32:((float*)out_buffer)[ii] = ((int*)source)[ii];break;
         case REAL32:((float*)out_buffer)[ii] = ((float*)source)[ii];break;
         case REAL64:((float*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        ieee_big32( ((float*)out_buffer)[ii] );
      }
      break;
    case REAL64:
      for ( ii=0; ii<num_samples_to_put; ii++ ) {
        switch (source_data_type) {
         case BYTE:((double*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case INTEGER16:((double*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case INTEGER32:((double*)out_buffer)[ii] = ((int*)source)[ii];break;
         case REAL32:((double*)out_buffer)[ii] = ((float*)source)[ii];break;
         case REAL64:((double*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        ieee_big64( ((double*)out_buffer)[ii] );
      }
      break;
    case COMPLEX_BYTE:
      for ( ii=0; ii<num_samples_to_put*2; ii++ )
        switch (source_data_type) {
         case COMPLEX_BYTE:((unsigned char*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case COMPLEX_INTEGER16:((unsigned char*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case COMPLEX_INTEGER32:((unsigned char*)out_buffer)[ii] = ((int*)source)[ii];break;
         case COMPLEX_REAL32:((unsigned char*)out_buffer)[ii] = ((float*)source)[ii];break;
         case COMPLEX_REAL64:((unsigned char*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
     break;
    case COMPLEX_INTEGER16:
      for ( ii=0; ii<num_samples_to_put*2; ii++ ) {
        switch (source_data_type) {
         case COMPLEX_BYTE:((short int*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case COMPLEX_INTEGER16:((short int*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case COMPLEX_INTEGER32:((short int*)out_buffer)[ii] = ((int*)source)[ii];break;
         case COMPLEX_REAL32:((short int*)out_buffer)[ii] = ((float*)source)[ii];break;
         case COMPLEX_REAL64:((short int*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        big16( ((short int*)out_buffer)[ii] );
      }
      break;
    case COMPLEX_INTEGER32:
      for ( ii=0; ii<num_samples_to_put*2; ii++ ) {
        switch (source_data_type) {
         case COMPLEX_BYTE:((int*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case COMPLEX_INTEGER16:((int*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case COMPLEX_INTEGER32:((int*)out_buffer)[ii] = ((int*)source)[ii];break;
         case COMPLEX_REAL32:((int*)out_buffer)[ii] = ((float*)source)[ii];break;
         case COMPLEX_REAL64:((int*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        big32( ((int*)out_buffer)[ii] );
      }
      break;
    case COMPLEX_REAL32:
      for ( ii=0; ii<num_samples_to_put*2; ii++ ) {
        switch (source_data_type) {
         case COMPLEX_BYTE:((float*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case COMPLEX_INTEGER16:((float*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case COMPLEX_INTEGER32:((float*)out_buffer)[ii] = ((int*)source)[ii];break;
         case COMPLEX_REAL32:((float*)out_buffer)[ii] = ((float*)source)[ii];break;
         case COMPLEX_REAL64:((float*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        ieee_big32( ((float*)out_buffer)[ii] );
      }
      break;
    case COMPLEX_REAL64:
      for ( ii=0; ii<num_samples_to_put*2; ii++ ) {
        switch (source_data_type) {
         case COMPLEX_BYTE:((double*)out_buffer)[ii] = ((unsigned char*)source)[ii];break;
         case COMPLEX_INTEGER16:((double*)out_buffer)[ii] = ((short int*)source)[ii];break;
         case COMPLEX_INTEGER32:((double*)out_buffer)[ii] = ((int*)source)[ii];break;
         case COMPLEX_REAL32:((double*)out_buffer)[ii] = ((float*)source)[ii];break;
         case COMPLEX_REAL64:((double*)out_buffer)[ii] = ((double*)source)[ii];break;
        }
        ieee_big64( ((double*)out_buffer)[ii] );
      }
      break;
  }
  samples_put = FWRITE(out_buffer, sample_size, num_samples_to_put, file);
  FREE(out_buffer);

  if ( samples_put != num_samples_to_put ) {
    printf("put_data_lines: failed to write the correct number of samples\n");
  }

  return samples_put;
}

/*******************************************************************************
 * Write 1 line of data via put_data_lines from a floating point array to a file
 * in the data type specified in the meta struct. Return number of samples put*/
int put_float_line(FILE *file, meta_parameters *meta, int line_number,
                   const float *source)
{
  return put_data_lines(file, meta, line_number, 1, source, REAL32);
}

/*******************************************************************************
 * Write num_lines_to_put lines of data via put_data_lines from a floating point
 * array to a file in the data type specified in the meta struct. Returns the
 * number of samples successfully written */
int put_float_lines(FILE *file, meta_parameters *meta, int line_number,
                    int num_lines_to_put, const float *source)
{
  return put_data_lines(file,meta,line_number,num_lines_to_put,source,REAL32);
}

/*******************************************************************************
 * Write 1 line of data from a double floating point array to a file in the data
 * format specified by the meta structure. All rounding, padding, and endian
 * conversion will be done as needed. The line_number argument is the
 * zero-indexed line number to get. The source argument must be a pointer to
 * existing memory. */
int put_double_line(FILE *file, meta_parameters *meta, int line_number,
                    const double *source)
{
  return put_data_lines(file, meta, line_number, 1, source, REAL64);
}

/*******************************************************************************
 * Write num_lines_to_put lines of data from a double floating point array to a
 * file in the data format specified by the meta structure. All rounding,
 * padding, and endian conversion will be done as needed. The line_number
 * argument is the zero-indexed line number to get. The source argument must be
 * a pointer to existing memory. */
int put_double_lines(FILE *file, meta_parameters *meta, int line_number,
                     int num_lines_to_put, const double *source)
{
  return put_data_lines(file,meta,line_number,num_lines_to_put,source,REAL64);
}

/*******************************************************************************
 * Write 1 line of data via put_data_lines from a complexFloat array to a file
 * in the complex data type specified in the meta struct. Return number of
 * samples put */
int put_complexFloat_line(FILE *file, meta_parameters *meta, int line_number,
                    const complexFloat *source)
{
  return put_data_lines(file, meta, line_number, 1, source, COMPLEX_REAL32);
}

/*******************************************************************************
 * Write num_lines_to_put lines of data via put_data_lines from a floating point
 * array to a file in the data type specified in the meta struct. Returns the
 * number of samples successfully written */
int put_complexFloat_lines(FILE *file, meta_parameters *meta, int line_number,
                     int num_lines_to_put, const complexFloat *source)
{
  return put_data_lines(file,meta,line_number,num_lines_to_put,source,
                        COMPLEX_REAL32);
}
