/*****************************************
io_util:
	Read & write files line by line.
*/

#include <assert.h>
#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"

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
      printf("get_float_line: Unrecognized data type. Exiting program.\n");
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
      printf("put_float_line: Unrecognized data type. Exiting program.\n");
      exit(EXIT_FAILURE);
  }

  out_buffer = MALLOC( (size_t) sample_size * sample_count);

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<sample_count; ii++ )
        ((unsigned char *)out_buffer)[ii] = (unsigned char)source[ii];
      break;
    case INTEGER16:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((short*)out_buffer)[ii] = (short)source[ii];
        big16( ((short*)out_buffer)[ii] );
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((int*)out_buffer)[ii] = (int)source[ii];
        big32( ((int*)out_buffer)[ii] );
      }
      break;
    case REAL32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((float*)out_buffer)[ii] = (float)source[ii];
        ieee_big32( ((float*)out_buffer)[ii] );
      }
    case REAL64:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((double*)out_buffer)[ii] = (double)source[ii];
        ieee_big64( ((double*)out_buffer)[ii] );
      }
      break;
  }

  FSEEK64(file, sample_size*sample_count*line_number, SEEK_SET);
  samples_put = FWRITE(out_buffer, sample_size, sample_count, file);

  FREE(out_buffer);
  return samples_put;
}


/*******************************************************************************
 * Get a single line of data in double floating point format, performing
 * rounding, padding, and endian conversion as needed.  The line_number argument
 * is the zero-indexed line number to get.  The dest argument must be a pointer
 * to existing memory. */
int get_double_line(FILE *file, meta_parameters *meta, int line_number, double *dest)
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
      printf("get_float_line: Unrecognized data type. Exiting program.\n");
      exit(EXIT_FAILURE);
  }

  /* Scan to the beginning of the line.  */
  FSEEK64(file, sample_size*sample_count*line_number, SEEK_SET);

  temp_buffer = (void *)MALLOC( (size_t) sample_size * sample_count);
  
  samples_gotten = FREAD(temp_buffer, sample_size, sample_count, file);

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<sample_count; ii++ )
        dest[ii] = (double)*( (uint8_t *) temp_buffer + ii);
      break;
    case INTEGER16:
      for ( ii=0; ii<sample_count; ii++ ) {
        big16(*((int16_t *)temp_buffer + ii));
        dest[ii] = (double)*( (int16_t *) temp_buffer + ii);
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<sample_count; ii++ ) {
        big32(*((int32_t *)temp_buffer + ii));
        dest[ii] = (double)*( (int32_t *) temp_buffer + ii);
      }
      break;
    case REAL32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ieee_big32(*((float*)temp_buffer + ii));
        dest[ii] = (double)*( (float *) temp_buffer + ii);
      }
      break;
    case REAL64:
      for ( ii=0; ii<sample_count; ii++ ) {
        ieee_big64(*((double*)temp_buffer + ii));
        dest[ii] = (double)*( (double *) temp_buffer + ii);
      }
  }

  FREE(temp_buffer);
  return samples_gotten;
}

/*******************************************************************************
 * Put a single line of data in double floating point format, performing
 * rounding, padding, and endian conversion as needed.  The line_number argument
 * is the zero-indexed line number to get.  The dest argument must be a pointer
 * to existing memory. */
int put_double_line(FILE *file, meta_parameters *meta, int line_number, const double *source)
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
      printf("put_float_line: Unrecognized data type. Exiting program.\n");
      exit(EXIT_FAILURE);
  }

  out_buffer = (void *)MALLOC( (size_t) sample_size * sample_count);

  /* Fill in destination array.  */
  switch (data_type) {
    case BYTE:
      for ( ii=0; ii<sample_count; ii++ )
        ((unsigned char *)out_buffer)[ii] = (unsigned char)source[ii];
      break;
    case INTEGER16:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((short*)out_buffer)[ii] = (short)source[ii];
        big16( ((short*)out_buffer)[ii] );
      }
      break;
    case INTEGER32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((int*)out_buffer)[ii] = (int)source[ii];
        big32( ((int*)out_buffer)[ii] );
      }
      break;
    case REAL32:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((float*)out_buffer)[ii] = (float)source[ii];
        ieee_big32( ((float*)out_buffer)[ii] );
      }
    case REAL64:
      for ( ii=0; ii<sample_count; ii++ ) {
        ((double*)out_buffer)[ii] = (double)source[ii];
        ieee_big64( ((double*)out_buffer)[ii] );
      }
      break;
  }

  FSEEK64(file, sample_size*sample_count*line_number, SEEK_SET);
  samples_put = FWRITE(out_buffer, sample_size, sample_count, file);

  FREE(out_buffer);
  return samples_put;
}
