#include <stdlib.h>
#include <stdio.h>
#include "../util/util.h"
#include "../util/metadata.h"

#define X_SIZE (800)
#define Y_SIZE (800)

static void usage (void);
int main ( int argc, char **argv);

static void usage ()
{
   printf("image2pgm <DATA PLANE> <METADATA> <OUTPUT.pgm>\n");
   exit(0);
}

int main ( int argc, char **argv)
{
   FILE *out, *in, *metadata;
   unsigned int *buffer;
   //int *banger;
   double *counter;
   int fout, fin;
   //int x = 2000, y = 2000;
   int a, b;
   char bout[256];
   int number_of_pixels, number_of_lines;
   int out_number_of_pixels, out_number_of_lines;
   int reduction_pixels, reduction_lines;
   double sum,sum2, ave, min, max;
   double t;
   unsigned long long c = 0;
   unsigned char * biff;
   unsigned int value;
   double delta;
   
   if ( argc == 1)
      usage();
   
   in = fopen ( argv[1], "r");
   out = fopen ( argv[3], "w");
   metadata = fopen( argv[2], "r");
   fin = fileno(in);
   fout = fileno(out);
   
   file_check (fin, argv[1]);
   file_check (fout, argv[3]);
   
   read_metadata_int( metadata, NUMBER_OF_PIXELS, &number_of_pixels);
   read_metadata_int( metadata, NUMBER_OF_LINES, &number_of_lines);
   
   reduction_lines = (number_of_lines/Y_SIZE);
   reduction_pixels = (number_of_pixels/X_SIZE);
   out_number_of_pixels = number_of_pixels / reduction_pixels;
   out_number_of_lines = number_of_lines /  reduction_lines;
   buffer = (unsigned int * ) malloc ( 4 * number_of_pixels );
   counter = (double * ) malloc ( sizeof(double)*out_number_of_pixels );
   
   sum = sum2 = 0.0;
   c = 0;
   printf("Converting image:        ");
   /* First read image and find min, max, and stdev */
   for ( a = 0; a < number_of_lines ; a ++ )
   {
      read_integer ( in, buffer, a*number_of_pixels, (a+1)*number_of_pixels);
      for ( b = 0; b < number_of_pixels; b++)
      {
         if ( buffer[b] != 0)
         {
            t = (double) buffer[b];
            sum += t;
            sum2 += t*t;
            c++;
         }
      }
      if (a % 8 == 0)
      {
         printf ("\b\b\b\b\b\b\b%% %05.2f",
         50.0 *(double) a / (double) number_of_lines);
         fflush (stdout);
      }
   }
   ave = sum / (double)c ;
   
   min = ave - 2.0 * stdev(sum, sum2, (double)(c));
   if ( min < 0 )
      min = 0;
   max = ave + 2.0 * stdev(sum, sum2, (double)(c));
   if ( max > INT_MAX )
      max = INT_MAX;
   
   /*
   printf("Sum = %g, sum2 = %g\n", sum, sum2);
   printf("The average DN value is %g, the stddev = %g\n", ave , stdev(sum, sum2, c ));
   printf("The min value = %g max value = %g\n", min, max);
    */
   
   sprintf(bout, "P5\n %d %d\n %d\n", out_number_of_pixels, out_number_of_lines, 255);
   write(fout, bout, strlen(bout));
   
   biff= ( unsigned char * ) malloc ( sizeof(unsigned char) * out_number_of_pixels);
   
   delta = 256.0 / (max - min);
   
   for ( b = 0; b < number_of_pixels/reduction_pixels; b++)
      counter[b]= 0.0;
   
   for ( a = 0; a < number_of_lines ; a ++ )
   {
      read_integer ( in, buffer, a*number_of_pixels, (a+1)*number_of_pixels);
      for ( b = 0; b < number_of_pixels; b++)
         counter[b/reduction_pixels]+= (double)buffer[b];
      
      if ( a %  reduction_lines ==  reduction_lines -1)
      {
         for ( b = 0; b < out_number_of_pixels; b++ )
         {
            counter[b] = counter[b] / (reduction_pixels*reduction_lines);
            
            value = (unsigned int) (delta*((double)counter[b] - min));
            /* printf("value = %g, result = %d\n", counter[b], value); */
            /*value = (unsigned int) ( (double)(counter[b]) * delta);*/
            
            if (value < 0)
               value = 0;
            else if ( value > 255 )
               value = 255;
            
            biff[b] = (unsigned char )(value);
            counter[b] = 0;
         }
         write(fout, biff, out_number_of_pixels);
      }
      if (a % 8 == 0)
      {
         printf ("\b\b\b\b\b\b\b%% %05.2f",
         50.0 + 50.0 *(double) a / (double) number_of_lines);
         fflush (stdout);
      }
   }
   printf("\n\n");
   close(fin);
   close(fout);

   exit(1);
}
