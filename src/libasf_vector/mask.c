#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include <string.h>
#include <stdio.h>

#define maxPoints 1024

// Check whether a point is within a polygon
// Code modified from algorithm by Randolph Franklin found at
// http://astronomy.swin.edu.au/~pbourke/geometry/insidepoly/
int pointInPolygon(int nVertices, double *pointX, double *pointY, 
		   double sample, double line)
{
  int i, j, inside=0;

  for (i=0, j=nVertices-1; i < nVertices; j=i++) {
    if ((((pointY[i] <= line) && (line < pointY[j])) ||
	 ((pointY[j] <= line) && (line < pointY[i]))) &&
	(sample < (pointX[j] - pointX[i]) * (line - pointY[i]) / 
	 (pointY[j] - pointY[i]) + pointX[i]))
      inside = !inside;
  }
  return inside;
}


// Generate a mask file from a point file that contains the area of interest
// (AOI) as a polygon. Set values within AOI to 1 and the background to 0.
// All file names are assumed to be basenames
int create_mask(char *imageFile, char *shapeFile, char *maskFile)
{
  meta_parameters *metaIn, *metaOut;
  FILE *fp;
  int i, k, nPoints=0, lines, samples, nShapes=0;
  int minLine, maxLine, minSample, maxSample;
  double *lat, *lon, *line, *sample;
  char pointFile[255], maskFileName[255], id[25], buf[1024];
  unsigned char *imageBuf;

  // Read metadata file
  metaIn = meta_read(imageFile);
  metaOut = meta_read(imageFile);
  metaOut->general->data_type = BYTE;
  lines = metaOut->general->line_count;
  samples = metaOut->general->sample_count;

  // Convert shapefile to point file
  sprintf(pointFile, "%s.txt", shapeFile);
  read_shapefile(shapeFile, pointFile);

  // Allocate memory and open point file
  imageBuf = (unsigned char *) MALLOC(sizeof(char)*lines*samples);
  sprintf(maskFileName, "%s.img", maskFile);
  fp = FOPEN(pointFile, "r");

  // Initialize the mask image with zeros
  for (i=0; i<lines; i++) 
    for (k=0; k<samples; k++)
      imageBuf[i*samples+k] = 0;

  // Determine the number of points in the shape object
  asfPrintStatus("   Calculating mask ...\n");
  while (fgets(buf, 1024, fp)) {
    if (strncmp(buf, "end", 3) != 0) {
      sscanf(buf, "%d", &nPoints);
      nShapes++;
      asfPrintStatus("   Part %d with %d vertices...\n", nShapes, nPoints);
      
      // Allocate memory for lat/lon and line/sample arrays
      lat = (double *) MALLOC(sizeof(double)*nPoints);
      lon = (double *) MALLOC(sizeof(double)*nPoints);
      line = (double *) MALLOC(sizeof(double)*nPoints);
      sample = (double *) MALLOC(sizeof(double)*nPoints);
      
      // Read lat/lon out of the point file
      for (i=0; i<nPoints; i++) {
	fgets(buf, 1024, fp);
	sscanf(buf, "%s\t%lf\t%lf", id, &lat[i], &lon[i]);
      }
      
      // Convert lat/lon into line/sample pairs and determine coverage
      minLine = lines;
      maxLine = 0;
      minSample = samples;
      maxSample = 0;
      for (i=0; i<nPoints; i++) {
	meta_get_lineSamp(metaIn, lat[i], lon[i], 0.0, &line[i], &sample[i]);
	if (line[i] < minLine) minLine = line[i];
	if (line[i] > maxLine) maxLine = line[i];
	if (sample[i] < minSample) minSample = sample[i];
	if (sample[i] > maxSample) maxSample = sample[i];
	//printf("ID: %d, lat: %.4lf, lon: %.4lf, line: %.4lf, sample: %.4lf\n",
	//       i, lat[i], lon[i], line[i], sample[i]);
      }
      
      // Make sure coverage limits are in the image
      if (minLine < 0) minLine = 0;
      if (maxLine > lines) maxLine = lines;
      if (minSample < 0) minSample = 0;
      if (maxSample > samples) maxSample = samples;
      printf("minLine: %d, maxLine: %d, minSample: %d, maxSample: %d\n",
	     minLine, maxLine, minSample, maxSample);

      // Walk through the image, check whether line/sample is within the predefined
      // polygon line by line
      for (i=minLine; i<maxLine; i++) {
	if (nPoints > 1000 && (i-minLine)%50 == 0)
	  printf("Checking line: %d\n", i);
	for (k=minSample; k<maxSample; k++) 
	  if (pointInPolygon(nPoints, line, sample, i, k))
	    imageBuf[i*samples+k] = 1;
      }

      // Free up allocated memory
      FREE(lat);
      FREE(lon);
      FREE(line);
      FREE(sample);
    }
  }
  FCLOSE(fp);
   
  // Write mask file and metadata file
  fp = FOPEN(maskFileName, "wb");
  FWRITE(imageBuf, lines*samples, 1, fp);
  meta_write(metaOut, maskFile);
  FCLOSE(fp);

  return(0);

}

// Function to invert a given mask
void invert_mask(char *maskInFile, char *maskOutFile)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta;
  int i, k, lines, samples;
  unsigned char *mask;

  // Do the prep work
  meta = meta_read(maskInFile);
  meta_write(meta, maskOutFile);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;
  mask = (unsigned char *) MALLOC(sizeof(char)*lines*samples);
  fpIn = FOPEN(maskInFile, "rb");
  fpOut = FOPEN(maskOutFile, "wb");
  
  // Read the mask data
  FREAD(mask, lines*samples, 1, fpIn);
  for (i=0; i<lines; i++)
    for (k=0; k<samples; k++)
      mask[i*samples+k] = !mask[i*samples+k]*255;
  FWRITE(mask, lines*samples, 1, fpOut);

  // Cleanup time
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(mask);
}

// Function to read a given mask
void read_mask(char *maskFile, unsigned char **p_mask, meta_parameters **p_meta)
{
  FILE *fp;
  int lines, samples;
  meta_parameters *meta = *p_meta;
  unsigned char *mask = *p_mask;

  // Read the metadata file and allocate appropriate memory
  meta = meta_read(maskFile);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;
  mask = (unsigned char *) MALLOC(sizeof(char)*lines*samples);

  // Read the mask
  fp = FOPEN(maskFile, "rb");
  FREAD(mask, lines*samples, 1, fp);
  FCLOSE(fp);

  // Set caller's pointers
  *p_meta = meta;
  *p_mask = mask;
}
