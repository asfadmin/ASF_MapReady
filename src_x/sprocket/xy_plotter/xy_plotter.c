#include <stdlib.h>
#include <stdio.h>
#include "../util/util.h"
#include "../util/metadata.h"

#define COMPLEX 	(11)
#define DETECTED 	(13)
#define HEADER_PIECE "%12s "
#define EX_HEADER_PIECE "%12s,"
#define PRINT_SPACING (" ")

#define ROOT_NUMBER  	(1)
#define X_NUMBER 	(ROOT_NUMBER + 1)
#define Y_NUMBER 	(X_NUMBER + 1)
#define ON_NUMBER 	(Y_NUMBER + 1)
#define BIN_NUMBER 	(ON_NUMBER + 1)
#define MASK_NUMBER 	(BIN_NUMBER + 1)
#define OUTFILE_NUMBER 	(MASK_NUMBER+1)

static void print_headers (FILE * out, FILE * excel_out, char *name, char *mask);
static void usage (void);
int main (int argc, char **argv);

	/* Print the usage */
static void usage ()
{
  printf
    ("xy_plotter <input_root> <X> <Y> <on|off> <BIN> <MASK> <OUTFILE>\n");
  printf ("  <X>   = incidence|look|slant|ground|pixel\n");
  printf ("  <Y>   = dn|power|sigma0|gama0\n");
  printf ("  on	   = The anaysis is preformed on the selected area.\n");
  printf ("  off   = The anaysis is preformed on the areas not selected.\n");
  printf ("  \n");
  exit (0);
}

static double slt_rng_1st_pixel;
static double pixel_spacing_rng;
static double Re, H;
static int number_of_pixels, number_of_lines;

	/* Main program */
#undef CALLER
#define CALLER "xy_plotter:Main"
int main (int argc, char **argv)
{
  char metadata_name[PATH_MAX];
  char image_name[PATH_MAX];
  char sigma_name[PATH_MAX];
  char mask_name[PATH_MAX];
  char outfile_name[PATH_MAX];
  char look_name[PATH_MAX];
  char excel_outfile_name[PATH_MAX];
  //char image_format[256];
  int start, stop;
  int a;

  FILE *metadata, *sigma, *image, *look, *out, *in_mask, *excel_out;
  unsigned int *pixels_buff;
  unsigned char *mask;
  int i, ii;
  int mi;
  double t_power, t_stdpower, t_sigma0, t_look, t_sltrng, t_gama0;
  int ON;

  float *sigma_buff, *look_buff;
  double *sigma_counter, *dn_counter, *dn2_counter;
  unsigned int *counter;
  double t;

  {
    FILE *T;
    T = fopen ("/tmp/junk", "w");
    for (a = 0; a < argc; a++)
      fprintf (T, "argv[%d] = \"%s\"\n", a, argv[a]);
    fclose (T);
  }

  if (argc != 8)
    usage ();

  /* Open files */

  /* metadata file */
  strcpy (metadata_name, argv[ROOT_NUMBER]);
  strcat (metadata_name, METADATA_EXT);
  metadata = fopen (metadata_name, "r");
  FILE_check (metadata, metadata_name, CALLER);

  /* Image file */
  strcpy (image_name, argv[ROOT_NUMBER]);
  strcat (image_name, DATA_EXT);
  image = fopen (image_name, "r");
  FILE_check (image, image_name, CALLER);

  /* Sigma file */
  strcpy (sigma_name, argv[ROOT_NUMBER]);
  strcat (sigma_name, SIGMA_EXT);
  sigma = fopen (sigma_name, "r");
  FILE_check (sigma, sigma_name, CALLER);

  /* Mask file */
  strcpy (mask_name, argv[MASK_NUMBER]);
  in_mask = fopen (mask_name, "r");
  FILE_check (in_mask, mask_name, CALLER);

  /* Output file */
  strcpy (outfile_name, argv[OUTFILE_NUMBER]);
  out = fopen (outfile_name, "w");
  FILE_check (out, outfile_name, CALLER);

  /* Excel Output file */
  strcpy (excel_outfile_name, argv[OUTFILE_NUMBER]);
  strcat (excel_outfile_name, ".cdf");
  excel_out = fopen (excel_outfile_name, "w");
  FILE_check (excel_out, excel_outfile_name, CALLER);

  if (strcmp ("off", argv[ON_NUMBER]))
    ON = 0;
  else
    ON = 0x80;			/* This value depends on how the mask is interpeted - 
				   see the for loop bellow. It is 0x80 because the bit
				   repersenting the current pixel is shifted left and compared
				   with 0x80.  -JC
				 */
  /* look file */
  strcpy (look_name, argv[1]);
  strcat (look_name, LOOK_EXT);
  look = fopen (look_name, "r");
  FILE_check (look, look_name, CALLER);

  read_metadata_int (metadata, NUMBER_OF_PIXELS, &number_of_pixels);
  read_metadata_int (metadata, NUMBER_OF_LINES, &number_of_lines);
  /*read_metadata_string (metadata, IMAGE_FORMAT, image_format); */
  read_metadata_double (metadata, SLANT_RANGE_TO_FIRST_PIXEL,
			&slt_rng_1st_pixel);
  read_metadata_double (metadata, RNG_PIXEL_SPACING, &pixel_spacing_rng);
  read_metadata_double (metadata, EARTH_RADIUS_AT_IMAGE_NARIR, &Re);
  read_metadata_double (metadata, PLATFORM_ALITITUDE, &H);

  pixels_buff =
    (unsigned int *) malloc (sizeof (unsigned int) * number_of_pixels);
  mask = (unsigned char *) malloc (sizeof (unsigned char) * number_of_pixels);
  sigma_buff = (float *) malloc (sizeof (float) * number_of_pixels);
  look_buff = (float *) malloc (sizeof (float) * number_of_pixels);
  sigma_counter = (double *) malloc (sizeof (double) * number_of_pixels);
  dn_counter = (double *) malloc (sizeof (double) * number_of_pixels);
  dn2_counter = (double *) malloc (sizeof (double) * number_of_pixels);
  counter =
    (unsigned int *) malloc (sizeof (unsigned int) * number_of_pixels);

  start = 0;
  stop = number_of_pixels;
  read_float (look, look_buff, start, stop);

  for (ii = 0; ii < number_of_pixels; ii++)
    {
      dn_counter[ii] = 0;
      dn2_counter[ii] = 0;
      sigma_counter[ii] = 0;
      counter[ii] = 0;
    }

  printf ("\nCompleted:       ");
  for (i = 0; i < number_of_lines; i++)
    {

      start = i * number_of_pixels;
      stop = start + number_of_pixels;
      read_integer (image, pixels_buff, start, stop);
      read_float (sigma, sigma_buff, start, stop);
      read_mask (in_mask, mask, start, stop, number_of_pixels);
      for (ii = 0; ii < number_of_pixels; ii++)
	{

	  mi = ii / 8;
	  /*
	     if (ii % 8 != 0)
	     mi++;
	   */

	  /*
	     if (mask[mi] != 0 )
	     {
	     printf("Something should be found..\n");
	     printf("Mask = %x, offset = %d (%x), pixels_buff = %d (%x) -> %x\n", 
	     mask[mi], ii % 8, ii%8, pixels_buff[ii], ((mask[mi] << (ii % 8)) & 0x80));
	     }
	   */
	  if ((((mask[mi] << (ii % 8)) & 0x80) == ON)
	      && (pixels_buff[ii] != 0 && sigma_buff[ii] > 1E-9))
	    {
	      counter[ii]++;
	      sigma_counter[ii] += (double) sigma_buff[ii];
	      t = (double) pixels_buff[ii];
	      dn_counter[ii] += t * t;
	      dn2_counter[ii] += t * t * t * t;
	    }
	}

      if (i % 8 == 0)
	{
	  printf ("\b\b\b\b\b\b\b%% %05.2f",
		  100.0 * (double) i / (double) number_of_lines);
	  fflush (stdout);
	}

    }
  printf ("\nFinished.\n");

  print_headers (out, excel_out, argv[1], mask_name);

  for (ii = 0; ii < number_of_pixels; ii++)
    {
      if (counter[ii] > 3)
	{
	  t_power = dn_counter[ii] / (double) counter[ii];
	  t_stdpower =
	    stdev (dn_counter[ii], dn2_counter[ii], (double) counter[ii]);
	  t_sltrng = look2slant (look_buff[ii], Re, H);
	  /*t_sltrng = look2incidence (look_buff[ii], Re, H); */
	  t_look = look_buff[ii];
	  t_sigma0 = 10.0 * log10 (sigma_counter[ii] / (double) counter[ii]);
	  t_gama0 = sigma02gama0 (look2incidence (t_look, Re, H), t_sigma0);

	  fprintf (out, "%12d%s", ii, PRINT_SPACING);
	  fprintf (out, "%12d%s", (counter[ii]), PRINT_SPACING);
	  fprintf (out, "%12f%s", t_sltrng, PRINT_SPACING);
	  fprintf (out, "%12f%s", t_look, PRINT_SPACING);
	  fprintf (out, "%12e%s", dn_counter[ii], PRINT_SPACING);
	  fprintf (out, "%12e%s", dn2_counter[ii], PRINT_SPACING);
	  fprintf (out, "%12f%s", t_power, PRINT_SPACING);
	  fprintf (out, "%12f%s", t_stdpower, PRINT_SPACING);
	  fprintf (out, "%12f%s", t_sigma0, PRINT_SPACING);
	  fprintf (out, "%12f%s\n", t_gama0, PRINT_SPACING);

	  fprintf (excel_out, "%12d,", ii);
	  fprintf (excel_out, "%12d,", (counter[ii]));
	  fprintf (excel_out, "%12f,", t_sltrng);
	  fprintf (excel_out, "%12f,", t_look);
	  fprintf (excel_out, "%12e,", dn_counter[ii]);
	  fprintf (excel_out, "%12e,", dn2_counter[ii]);
	  fprintf (excel_out, "%12f,", t_power);
	  fprintf (excel_out, "%12f,", t_stdpower);
	  fprintf (excel_out, "%12f,", t_sigma0);
	  fprintf (excel_out, "%12f%s\n", t_gama0, PRINT_SPACING);
	}
    }

  /* files */
  fclose (image);
  fclose (in_mask);
  fclose (look);
  fclose (sigma);
  fclose (metadata);
  fclose (out);
  fclose (excel_out);

  return 0;
}

static void print_headers (FILE * out, FILE * excel_out, char *name, char *mask_name)
{

  /* Print metadata for excel data */
  fprintf (out, "\t\tDISTRIBUTED TARGET ANALYSIS\n");
  fprintf (out, "\t%-40s :\t \"%s\"\n", "Image File", name);
  fprintf (out, "\t%-40s :\t \"%s\"\n", "Mask File", mask_name);
  fprintf (out, "\t%-40s :\t %d\n", "Number of Pixels", number_of_pixels);
  fprintf (out, "\t%-40s :\t %d\n", "Number of Lines", number_of_lines);
  fprintf (out, "\t%-40s :\t %g\n", "Slant range to first pixel (km)",
	   slt_rng_1st_pixel);
  fprintf (out, "\t%-40s :\t %g\n", "Pixel Spacing in range (m)",
	   pixel_spacing_rng);
  fprintf (out, "\t%-40s :\t %g\n", "Radius of the earth at narir (km)", Re);
  fprintf (out, "\t%-40s :\t %g\n", "Platform Altitiude (km)", H);
  fprintf (out, "\n");

  /* Print metadata for formatted data */
  fprintf (excel_out, "\t\tDISTRIBUTED TARGET ANALYSIS\n");
  fprintf (excel_out, "\t%-40s , \"%s\"\n", "Image File", name);
  fprintf (excel_out, "\t%-40s , \"%s\"\n", "Mask File", mask_name);
  fprintf (excel_out, "\t%-40s , %d\n", "Number of Pixels", number_of_pixels);
  fprintf (excel_out, "\t%-40s , %d\n", "Number of Lines", number_of_lines);
  fprintf (excel_out, "\t%-40s , %g\n", "Slant Range to First Pixel (km)",
	   slt_rng_1st_pixel);
  fprintf (excel_out, "\t%-40s , %g\n", "Pixel Spacing Range (m)",
	   pixel_spacing_rng);
  fprintf (excel_out, "\t%-40s , %g\n",
	   "Radius of the earth at narir (km)", Re);
  fprintf (excel_out, "\t%-40s , %g\n", "Platform Altitiude (km)", H);
  fprintf (excel_out, "\n");

  /* Print header for text formatted data */
  fprintf (out, HEADER_PIECE, "Pixel #     ");
  fprintf (out, HEADER_PIECE, "# Of Values ");
  fprintf (out, HEADER_PIECE, "SltRng(km)  ");
  fprintf (out, HEADER_PIECE, "LookAg(D)   ");
  fprintf (out, HEADER_PIECE, "Sum of Pwr  ");
  fprintf (out, HEADER_PIECE, "Sum^2 Pwr   ");
  fprintf (out, HEADER_PIECE, "AvgPower      ");
  fprintf (out, HEADER_PIECE, "StDevPwr      ");
  fprintf (out, HEADER_PIECE, " Sigma0(db)  ");
  fprintf (out, HEADER_PIECE, " Gama0(db)   ");
  fprintf (out, "\n");

  /* Print header for excel formatted data */
  fprintf (excel_out, EX_HEADER_PIECE, "Pixel #     ");
  fprintf (excel_out, EX_HEADER_PIECE, "# Of Values ");
  fprintf (excel_out, EX_HEADER_PIECE, "SltRng(km)  ");
  fprintf (excel_out, EX_HEADER_PIECE, "LookAg(D)   ");
  fprintf (excel_out, EX_HEADER_PIECE, "Sum of Pwr  ");
  fprintf (excel_out, EX_HEADER_PIECE, "Sum^2 Pwr   ");
  fprintf (excel_out, EX_HEADER_PIECE, " AvgPower   ");
  fprintf (excel_out, EX_HEADER_PIECE, " StDevPwr   ");
  fprintf (excel_out, EX_HEADER_PIECE, " Sigma0(db) ");
  fprintf (excel_out, EX_HEADER_PIECE, " Gama0(db)  ");
  fprintf (excel_out, "\n");
}
