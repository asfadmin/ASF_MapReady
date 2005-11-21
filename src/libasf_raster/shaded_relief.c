#include "asf.h"
#include "asf_raster.h"

#define VERSION 1.0
#define speckleLen 0x0fff
#define getSpeckle speckle[specklePtr=((specklePtr+7)&speckleLen)]

float unitRand(void)
{
  return (float)(0x0ffff&rand())/(0x0ffff);
}

float gaussRand(int num)
{
  int i;
  float ret=0;
  for (i=0;i<num;i++)
    ret+=unitRand();
  return 2*ret/num;
}

/*Note: it may seem stupid to add speckle to our simulated
  SAR images, but the speckle has an important statistical 
  contribution to the image.*/
float *createSpeckle(void)
{
  int i;
  float *speckle=(float *)MALLOC(sizeof(float)*(speckleLen+1));
  for (i=0;i<=speckleLen;i++)
    speckle[i]=gaussRand(4);
  return speckle;
}

/* The function was implemented using the documentation of the LAS tool picshade */
void shaded_relief(char *inFile, char *outFile, int addSpeckle)
{
  meta_parameters *meta;
  FILE *fpIn, *fpOut;
  int ii, kk, line_count, sample_count;
  float *dem, *relief;
  double a1, a2, a3, b1, b2, b3, dx, dy;
  double pixel_size;
  double input_scale = 1.0;
  double output_scale = 1.0;
  double sun_angle = 23.5 * D2R;
  double azimuth_angle = 90.0 * D2R;
  double exponent = 1.0;
  double ambient_light = 0.0;
  double alpha;
  static float *speckle=NULL;
  int specklePtr=rand();
  if (speckle==NULL)
    speckle=createSpeckle();
  
  // Read and write metadata
  meta = meta_read(inFile);
  line_count = meta->general->line_count;
  sample_count = meta->general->sample_count;
  pixel_size = meta->general->y_pixel_size;
  meta_write(meta, outFile);

  // Allocate memory for DEM and shaded relief
  dem = (float *) MALLOC(sizeof(double)*3*sample_count);
  relief = (float *) MALLOC(sizeof(double)*sample_count);

  // Open files
  fpIn = FOPEN(inFile, "rb");
  fpOut = FOPEN(outFile, "wb");
 
  // We can't calculate shaded relief values for the boundary pixels, since we
  // operate with 3x3 filter for calculating the gradients in x and y.
  // That means we need to set the boundary pixels to zero.

  // Take care of first line and last line of the shaded relief.
  for (ii=0; ii<sample_count; ii++)
    relief[ii] = 0.0;
  put_float_line(fpOut, meta, 0, relief);
  put_float_line(fpOut, meta, line_count-1, relief);

  // Go through DEM line by line
  for (ii=1; ii<line_count-2; ii++) {
    
    // Read in three lines of DEM at a time
    get_float_lines(fpIn, meta, ii-1, 3, dem);
    
    for (kk=1; kk<sample_count-1; kk++) {
      /* Calculate gradients in x and y
	 
	 DX = input_scale_factor / (2*pixel_size) | -1  0  1 |
       
                                                  |  1 |
         DY = input_scale_factor / (6*pixel_size) |  0 |
                                                  | -1 |
       
      */
    
      dx = input_scale/(2*pixel_size) * 
     	(-dem[kk+sample_count-1] + dem[kk+sample_count+1]);
      dy = input_scale/(2*pixel_size) * 
      	(-dem[kk] + dem[kk+2*sample_count]);
      
      // Calculate local surface normal using the gradient
      a1 = dx / sqrt(dx*dx + dy*dy + 1);
      a2 = dy / sqrt(dx*dx + dy*dy + 1);
      a3 = 1 / sqrt(dx*dx + dy*dy + 1);
      
      // Calculate sun direction vector
      b1 = sin(azimuth_angle) * cos(sun_angle);
      b2 = cos(azimuth_angle) * cos(sun_angle);
      b3 = sin(sun_angle);
      
      // Calculate cosine of the angle between sun direction and the surface normal
      alpha = a1*b1 + a2*b2 + a3*b3;
      if (alpha < 0.0)
	alpha = 0.0;
      
      // Calculate the shaded relief
      if (FLOAT_EQUIVALENT(dem[kk+sample_count], 0.0))
	relief[kk] = 0.0;
      else {
	relief[kk] = (float) (output_scale * pow(alpha,exponent) + ambient_light);
	if (addSpeckle)
	  relief[kk] *= getSpeckle;
      }
    }

    // Write out shaded relief
    put_float_line(fpOut, meta, ii, relief);
  }

  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(dem);
  FREE(relief);

}
