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
int get_data_lines(FILE *file, meta_parameters *meta, 
		   int line_number, int num_lines_to_get, 
		   int sample_number, int num_samples_to_get,
		   void *dest, int dest_data_type)
{
  int ii;               /* Sample index.  */
  int samples_gotten=0; /* Number of samples retrieved */
  int line_samples_gotten;
  size_t sample_size;   /* Sample size in bytes.  */
  void *temp_buffer;    /* Buffer for unconverted data.  */
  int sample_count = meta->general->sample_count;
  int line_count = meta->general->line_count;
  int band_count = meta->general->band_count;
  int data_type    = meta->general->data_type;
  int num_lines_left = line_count * band_count - line_number;
  int num_samples_left = sample_count - sample_number;
  long long offset;

  // Check whether data conversion is possible
  if ((data_type>=COMPLEX_BYTE) && (dest_data_type<=REAL64))
    asfPrintError("\nget_data_lines: Cannot put complex data"
		  " into a simple data buffer. Exiting.\n\n");
  if ((data_type<=REAL64) && (dest_data_type>=COMPLEX_BYTE))
    asfPrintError("\nget_data_lines: Cannot put simple data"
		  " into a complex data buffer. Exiting.\n\n");
  // Make sure not to go outside the image
  if (line_number > (line_count * band_count))
    asfPrintError("\nget_data_lines: Cannot read line %d "
		  "in a file of %d lines. Exiting.\n",
		  line_number, line_count*band_count);
  if (sample_number < 0 || sample_number > meta->general->sample_count)
    asfPrintError("\nget_data_lines: Cannot read sample %d "
		  "in a file of %d lines. Exiting.\n",
		  sample_number, sample_count);
  if (num_lines_to_get > num_lines_left)
    asfPrintError("\nget_data_lines: Cannot read %d lines. "
		  "Only %d lines left in file. Exiting.\n",
		  num_lines_to_get, num_lines_left);
  if (num_samples_to_get > num_samples_left)
    asfPrintError("\nget_data_lines: Cannot read %d samples. "
		  "Only %d samples left in file. Exiting.\n",
		  num_samples_to_get, num_samples_left);

  /* Determine sample size.  */
  sample_size = data_type2sample_size(data_type);

  temp_buffer = MALLOC( sample_size * num_lines_to_get * num_samples_to_get);


  // Scan to the beginning of the line sample.
  for (ii=0; ii<num_lines_to_get; ii++) {
    offset = sample_size * (sample_count * (line_number + ii) + sample_number);
    FSEEK64(file, offset, SEEK_SET);
    line_samples_gotten = FREAD(temp_buffer+ii*num_samples_to_get*sample_size, 
				sample_size, num_samples_to_get, file);
    samples_gotten += line_samples_gotten;
  }

  /* Fill in destination array.  */
  switch (data_type) {
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

int get_partial_byte_line(FILE *file, meta_parameters *meta, int line_number, 
			  int sample_number, int num_samples_to_get, 
			  unsigned char *dest)
{
  return get_data_lines(file, meta, line_number, 1, 
			sample_number, num_samples_to_get, dest, BYTE);
}

int get_partial_byte_lines(FILE *file, meta_parameters *meta, int line_number, 
			   int num_lines_to_get, int sample_number, 
			   int num_samples_to_get, unsigned char *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get, 
			sample_number, num_samples_to_get, dest, BYTE);
}


int get_byte_line(FILE *file, meta_parameters *meta, int line_number,
                  unsigned char *dest)
{
  return get_data_lines(file, meta, line_number, 1, 
			0, meta->general->sample_count, dest, BYTE);
}

int get_byte_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_get, unsigned char *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get,
			0, meta->general->sample_count, dest, BYTE);
}


/*******************************************************************************
 * Get one line of any non-complex data type via get_data_lines and fill a float
 * buffer with it. Returns number of samples gotten                           */
int get_float_line(FILE *file, meta_parameters *meta, int line_number,
                   float *dest)
{
  return get_data_lines(file, meta, line_number, 1, 
			0, meta->general->sample_count, dest, REAL32);
}

/*******************************************************************************
 * Get num_lines_to_get lines of any non-complex data type via get_data_lines
 * and fill a float buffer with it. Returns number of samples gotten          */
int get_float_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_get, float *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get,
			0, meta->general->sample_count, dest, REAL32);
}

int get_partial_float_line(FILE *file, meta_parameters *meta, int line_number, 
			   int sample_number, int num_samples_to_get,
			   float *dest)
{
  return get_data_lines(file, meta, line_number, 1, 
			sample_number, num_samples_to_get, dest, REAL32);
}

int get_partial_float_lines(FILE *file, meta_parameters *meta, 
			    int line_number, int num_lines_to_get,
			    int sample_number, int num_samples_to_get,
			    float *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get, 
			sample_number, num_samples_to_get, dest, REAL32);
}

/*******************************************************************************
 * Get a single line of any non-complex data in double floating point format,
 * performing rounding, padding, and endian conversion as needed.  The
 * line_number argument is the zero-indexed line number to get.  The dest
 * argument must be a pointer to existing memory. */
int get_double_line(FILE *file, meta_parameters *meta, int line_number,
                    double *dest)
{
  return get_data_lines(file, meta, line_number, 1, 
			0, meta->general->sample_count, dest, REAL64);
}

/*******************************************************************************
 * Get num_lines_to_get lines of any non-complex data type via get_data_lines
 * and fill a double buffer with it. Returns number of samples gotten.        */
int get_double_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_get, double *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get,
			0, meta->general->sample_count, dest, REAL64);
}

/*******************************************************************************
 * Get one line of any complex data type via get_data_lines and fill a
 * complexFloat buffer with it. Returns number of samples gotten              */
int get_complexFloat_line(FILE *file, meta_parameters *meta, int line_number,
                    complexFloat *dest)
{
  return get_data_lines(file, meta, line_number, 1, 
			0, meta->general->sample_count, dest, COMPLEX_REAL32);
}

/*******************************************************************************
 * Get num_lines_to_get lines of any complex data type via get_data_lines
 * and fill a complexFloat buffer with it. Returns number of samples gotten   */
int get_complexFloat_lines(FILE *file, meta_parameters *meta, int line_number,
                     int num_lines_to_get, complexFloat *dest)
{
  return get_data_lines(file, meta, line_number, num_lines_to_get, 
			0, meta->general->sample_count, dest, COMPLEX_REAL32);
}

/*******************************************************************************
 * Write x number of lines of any data type to file in the data format specified
 * by the meta structure. It is always written in big endian format. Returns the
 * amount of samples successfully converted & written. Will not write more lines
 * than specified in the supplied meta struct. */
static int put_data_lines(FILE *file, meta_parameters *meta, int band_number,
                          int line_number_in_band, int num_lines_to_put,
                          const void *source, int source_data_type)
{
  int ii;               /* Sample index.                       */
  int samples_put;      /* Number of samples written           */
  size_t sample_size;   /* Sample size in bytes.               */
  void *out_buffer;     /* Buffer of converted data to write.  */
  int sample_count       = meta->general->sample_count;
  int data_type          = meta->general->data_type;
  int num_samples_to_put = num_lines_to_put * sample_count;
  int line_number        = meta->general->line_count * band_number +
                               line_number_in_band;

  if ((source_data_type>=COMPLEX_BYTE) && (data_type<=REAL64)) {
    printf("\nput_data_lines: Cannot put complex data into a simple data file. Exiting.\n\n");
    exit(EXIT_FAILURE);
  }
  if ((source_data_type<=REAL64) && (data_type>=COMPLEX_BYTE)) {
    printf("\nput_data_lines: Cannot put simple data into a complex data file. Exiting.\n\n");
    exit(EXIT_FAILURE);
  }

  // Write out all optical data as byte image. 
  // They don't have a larger dynamic range than that.
  if (meta->optical)
    data_type = BYTE;

  /* Determine sample size.  */
  sample_size = data_type2sample_size(data_type);

  /* Make sure not to make file bigger than meta says it should be */
  if (line_number > meta->general->line_count * meta->general->band_count) {
    printf("\nput_data_lines: Cannot write line %d of band %d in a\n"
           "file that should be %d lines. Exiting.\n",
           line_number, band_number,
           meta->general->line_count * meta->general->band_count);
    exit(EXIT_FAILURE);
  }
  if ((line_number+num_lines_to_put) >
      meta->general->line_count * meta->general->band_count)
  {
    num_samples_to_put = (meta->general->line_count - line_number)
                          * sample_count;
  }

  FSEEK64(file, (long long)sample_size*sample_count*line_number, SEEK_SET);
  out_buffer = MALLOC( sample_size * sample_count * num_lines_to_put );

  /* Fill in destination array.  */
  switch (data_type) {
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
  return put_data_lines(file, meta, 0, line_number, 1, source, REAL32);
}

int put_band_float_line(FILE *file, meta_parameters *meta, int band_number,
                        int line_number, const float *source)
{
  return put_data_lines(file, meta, band_number, line_number, 1, source, REAL32);
}

/*******************************************************************************
 * Write num_lines_to_put lines of data via put_data_lines from a floating point
 * array to a file in the data type specified in the meta struct. Returns the
 * number of samples successfully written */
int put_float_lines(FILE *file, meta_parameters *meta, int line_number,
                    int num_lines_to_put, const float *source)
{
  return put_data_lines(file,meta,0,line_number,num_lines_to_put,source,REAL32);
}

int put_band_float_lines(FILE *file, meta_parameters *meta, int band_number,
                         int line_number, int num_lines_to_put,
                         const float *source)
{
  return put_data_lines(file,meta,band_number,line_number,
                        num_lines_to_put,source,REAL32);
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
  return put_data_lines(file, meta, 0, line_number, 1, source, REAL64);
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
  return put_data_lines(file,meta,0,line_number,num_lines_to_put,source,REAL64);
}

/*******************************************************************************
 * Write 1 line of data via put_data_lines from a complexFloat array to a file
 * in the complex data type specified in the meta struct. Return number of
 * samples put */
int put_complexFloat_line(FILE *file, meta_parameters *meta, int line_number,
                    const complexFloat *source)
{
  return put_data_lines(file, meta, 0, line_number, 1, source, COMPLEX_REAL32);
}

/*******************************************************************************
 * Write num_lines_to_put lines of data via put_data_lines from a floating point
 * array to a file in the data type specified in the meta struct. Returns the
 * number of samples successfully written */
int put_complexFloat_lines(FILE *file, meta_parameters *meta, int line_number,
                     int num_lines_to_put, const complexFloat *source)
{
  return put_data_lines(file,meta,0,line_number,num_lines_to_put,source,
                        COMPLEX_REAL32);
}
