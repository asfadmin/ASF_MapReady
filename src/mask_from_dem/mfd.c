/* This fabulous program takes a dem 
and creates a mask based on the occurance of
a particular height range
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "asf.h"
#include "asf_meta.h"

float version=0.1;
#define BUF		256
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#define MASK_VALUE 	1

void usage();
char mask_expression(int op, float value1, float value2, float value3);
		
void usage()
{
  printf("\nUSAGE:\n");
  printf("   mfd in_dem_file out_mask_file  expression number[s]"
	".\n\n");         
  printf(" in_dem_file : the source file \n"
	 " out_mask_file : the output mask file name \n"
	 " expression one of the following \n"
	 " lt (<) less than,\n gt (>)  greater than,\n gte (>=) greater than or equal,\n"
	 " lte (<=) less than or equal,\n = equal,\n ... value range from [lower] to [higher] \n"
	 " number[s] are floating values to be used in the previous expression \n");
  printf("Version %.2f, ASF TOOLS\n\n",version);
  exit(1);
}


char mask_expression(int op, float value1, float lower, float higher)
{ // returns a suitably created byte mask depending on the value of op.
	
	char me;
	me = 0;
	
	switch (op)
	{
		case 0: // greater than or equal
			if (value1 >= lower) me = MASK_VALUE;
		break;
		case  1: // greater than
			if (value1 > lower) me = MASK_VALUE;
		break;
		case 2: // less than or equal
			if (value1 <= lower) me = MASK_VALUE;
		break;
		case 3: // less than
			if (value1 < lower) me = MASK_VALUE;
		break;
		case 4:		// equal
			if (value1 == lower) me = MASK_VALUE;
		break;
		case 5: // value range
			if (( value1 >= lower) && ( value1 <= higher)) me = MASK_VALUE;
		break;
		default: // unkown op code
		me = 0;
	}
	return (me);
}

int
main(int argc, char *argv[])
{

  int op;
  long point_size;
  long x_size, y_size, line,blockSize,xpos;
  FILE *in, *out;
  meta_parameters *inMeta, *outMeta;
  char infile[BUF];
  char outfile[BUF];
  char  *maskbuffer;
  float *floatbuffer;
  float arga,argb;
   if (argc<5) usage();
   
   point_size = 1;
  create_name (infile, argv[1], ".img");
  create_name (outfile,argv[2], ".img");
  inMeta = meta_read(argv[1]);
  outMeta = meta_read(argv[1]);
  x_size = inMeta->general->sample_count;
  y_size = inMeta->general->line_count;
//  printf(" x_size = %d y_size = %d \n \n ", x_size, y_size);
//  printf("target line is number %d \n",tline);
  
  floatbuffer = (float *) MALLOC (sizeof(float) * inMeta->general->sample_count  * 1 );
  maskbuffer = (char *) MALLOC (sizeof(char) * inMeta->general->sample_count * 1);
  
//  printf(" created memory buffer \n");
  in = fopenImage(infile, "rb");
  out = fopenImage(outfile, "wb");
  outMeta->general->data_type = BYTE;
  //TODO better argument conversion
  // now work out what operation we are doing
  arga = atof(argv[4]);
  argb = 0;
  if (strcmp(argv[3], "gt") >= 0)
  {
	  if (strchr(argv[3], 'e') != NULL)
  {
	   op = 0;
  }
  else 
  {
	   op = 1;
  }
  }
  
  if (strcmp(argv[3], "lt") >= 0)
  {
	  if (strchr(argv[3], 'e') != NULL)
  {
	  op = 2;
  }
  else 
  {
	   op = 3;
  }
  }	
			
  if (strchr(argv[3],'=') != NULL)
	op = 4;
			
  if (strcmp(argv[3],"...") == 0)
  {
	  argb = atof(argv[5]);
	  op = 5;
  }
  
  // now process the image
  
  for (line=0; line <= y_size ; line++) 
  	{
	   blockSize = get_float_line(in,inMeta,line,floatbuffer);
	   printf(".");
	   for (xpos=0; xpos < x_size; xpos++)
		   maskbuffer[xpos] = mask_expression(op, floatbuffer[xpos], arga, argb); // does the actual data validation
	   blockSize = put_data_lines(out, outMeta,  line, 1, maskbuffer, BYTE); 

	   // blockSize = put_float_line(out,outMeta,line,maskbuffer);
  	}
  
  meta_write(outMeta, outfile);
  meta_free(inMeta);
  meta_free(outMeta);
  
  FCLOSE(in);
  FCLOSE(out);
  FREE(floatbuffer);
  FREE(maskbuffer);
  return(0);
}
