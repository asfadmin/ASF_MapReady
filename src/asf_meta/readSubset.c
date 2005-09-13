/***********************************************************
readSubset:
  Reads a subset of an image. Regardless of the input data type
  (except complex data) the output will be converted to float.
*/

#include <assert.h>
#include "asf.h"
#include "asf_meta.h"


/* Function 'readSubset' returning a subset of image. The data type of the
   output image is float, regardless what the input data type is. It errors
   on complex data. The function expects to have the memory for the returning 
   image array already allocated. 
*/
void readSubset(char *fileName, int width, int height, int posX, int posY, 
		float *subset)
{
  FILE *fp;
  int ii, kk;
  float *buffer;
  meta_parameters *meta = meta_read(fileName);
  int lines = meta->general->line_count;
  int samples = meta->general->sample_count;

  /* Check whether input parameters are feasible */
  assert (width > 0);
  assert (height > 0);
  assert (width < lines);
  assert (height < samples);
  assert (posX > 0);
  assert (posY > 0);
  assert ((posX+width) < samples);
  assert ((posY+height) < lines);
  if (meta->general->data_type > 5)
    printErr("   ERROR: 'readSubset' does not work on complex data!\n");

  /* Allocate memory for buffers */
  buffer = (float *) MALLOC(height*samples*sizeof(float));

  /* Open image file */
  fp = FOPEN(fileName, "rb");

  /* Read the appropriate data chunk */
  get_float_lines(fp, meta, posY, height, buffer);

  /* Fill in the subset buffer */
  for (ii=0; ii<height; ii++) 
    for (kk=0; kk<width; kk++) 
      subset[ii*width+kk] = buffer[ii*samples+posX+kk];

  /* Clean up */
  FCLOSE(fp);
  FREE(buffer);
  meta_free(meta);
}
