//#include <stdlib.h>
//#include <stdio.h>
#include "util.h"
#include "metadata.h"

static void usage (void);
int main (int argc, char **argv);

static void usage ()
{
  printf ("image_mask <imagedata> <metadata> <mask> <outimage>\n");
  exit (-1);
}

int main (int argc, char **argv)
{
  FILE *out, *in, *metadata, *in_mask;
  unsigned int *pixels;
  unsigned char *mask;
  int start, stop;
  int mi;
  int foo;

  int fout, fin, fin_mask;
  int i, ii;

  int number_of_pixels, number_of_lines;

  if (argc == 1)
    usage ();

  in = fopen (argv[1], "r");
  in_mask = fopen (argv[3], "r");
  out = fopen (argv[4], "w");
  metadata = fopen (argv[2], "r");

  fin = fileno (in);
  fin_mask = fileno (in_mask);
  fout = fileno (out);

  file_check (fin, argv[1]);
  file_check (fout, argv[4]);
  file_check (fin_mask, argv[3]);

  read_metadata_int (metadata, NUMBER_OF_PIXELS, &number_of_pixels);
  read_metadata_int (metadata, NUMBER_OF_LINES, &number_of_lines);

  pixels = (unsigned int *) malloc (sizeof (unsigned int) * number_of_pixels);
  mask = (unsigned char *) malloc (sizeof (unsigned char) * number_of_pixels);

  printf ("\nCompleted:       ");
  for (i = 0; i < number_of_lines; i++)
    {
      start = i * number_of_pixels;
      stop = start + number_of_pixels;
      read_integer (in, pixels, start, stop);
      read_mask (in_mask, mask, start, stop, number_of_pixels);

      foo = 0;
      for (ii = 0; ii < number_of_pixels; ii++)
	{
	  mi = ii / 8;
	  /*
	     if (ii % 8 != 0)
	     mi++;
	   */
	  if (((mask[mi] << (ii % 8)) & 0x80) != 0)
	    pixels[ii] = 256;
	}
      write_integer (fout, pixels, number_of_pixels);

      if (i % 8 == 0)
	{
	  printf ("\b\b\b\b\b\b\b%% %05.2f",
		  100.0 * (double) i / (double) number_of_lines);
	  fflush (stdout);
	}


    }
  printf ("\nFinished.\n");

  fclose(in);
  fclose(in_mask);
  fclose(out);
  fclose(metadata);
  exit(1);
}
