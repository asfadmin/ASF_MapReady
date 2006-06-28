/* raw to point
takes a  gray scale image and makes a file x y z

*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "asf.h"
#include "asf_meta.h"

float version=0.1;
#define BUF		256

void usage();

void usage()
{
  printf("\nUSAGE:\n");
  printf("   profile_dump infile x|y Line/Sample  > your_outfile"
	".\n\n");         
  printf(" infile : the source file \n"
	 " x or y : type of profile you desire \n"
	 " Line/Sample : where to grab profile from \n"
	 " this is a very simple program....  \n"
	 " plot your data within gnuplot like this \n"
	 " profile_dump file y 5 > line_5_profile \n" 
	 " gnuplot \n"
	 "  plot \"line_5_profile\" with lines \n");
  printf("Version %.2f, ASF TOOLS\n\n",version);
  exit(1);
}

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

int
main(int argc, char *argv[])
{

  int i;
  long point_size;
  long x_size, y_size, line,tline,blockSize;
  FILE *in;
  meta_parameters *inMeta;
  char infile[BUF];
  float *buffer;

   if (argc!=4) usage();
   
   point_size = 1;
  create_name (infile, argv[1], ".img");
  inMeta = meta_read(argv[1]);
  x_size = inMeta->general->sample_count;
  y_size = inMeta->general->line_count;
//  printf(" x_size = %d y_size = %d \n \n ", x_size, y_size);
  tline = atol(argv[3]);
//  printf("target line is number %d \n",tline);
  
  buffer = (float *) MALLOC (sizeof(float) * inMeta->general->sample_count  * 1 );
//  printf(" created memory buffer \n");
  in = fopenImage(infile, "rb");
  if (argv[3]="x")
  {
	 
	  blockSize = get_float_line(in,inMeta,tline,buffer);
//	  printf(" read in line to memory \n");
	  for (i=0; i < inMeta->general->sample_count; i++)
	  {
		  printf("%f\n",buffer[i]);
	  }
	  
  } else // deal with the "y" profile
  {
  for (line=0; line<inMeta->general->line_count; line++) 
  	{
	   blockSize = get_float_line(in,inMeta,line,buffer);
	   printf(".");
	   tline = atoi(argv[4]);
	   printf("%f\n",buffer[tline]);   
  	}
  }
  FCLOSE(in);
  FREE(buffer);
  return(0);
}
