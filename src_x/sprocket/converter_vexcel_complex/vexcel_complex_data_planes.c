/**** RSI CEOS to Sprocket format converter for data_planes ****/

#include <math.h>
#include <errno.h>
#include "util.h"
#include "metadata.h"
#include "ceos_rd.h"

// globals defining image


static int left_pad, top_pad, right_pad, bottom_pad;
static int number_of_samples;
static int number_of_lines;
static double pixel_size_range;
// static double pixel_size_az;
static double slant_range_to_first_pixel;
static double platform_alt;
static double Re;
// int projection;
static double noise_array[512];
static double offset;
static double noise_scale;
static int direction;
static double noise_inc;

#define ASENDING (0)
#define DESENDING (1)

#define SIZE_OF_VEXCEL_HEADER (16252)
#define SIZE_OF_VEXCEL_IOF (192)
#define SIZE_OF_VEXCEL_COMPLEX ( 4 )
#define DEBUG ( getenv("DEBUG") != NULL)

/* Prototypes */
int main (int argc, char **argv);
static void usage ();
static int generate_data_planes (char *infile, char *base);
static void obtain_metadata (char *datain, char *metaout);
static float sigma_nought (float dn2, float j, float look_angle, float inc, float sl);


int main (int argc, char **argv)
{
  clock_t start, stop;
  if (argc == 1)
    usage ();

  start = times (NULL);

  obtain_metadata (argv[2], argv[3]);
  generate_data_planes (argv[1], argv[3]);

  stop = times (NULL);

  printf ("\n\nConversion took %f minites.\n",
	  (double) (stop - start) / (double) CLK_TCK / 60.0);

  return 0;
}


static void usage ()
{
  printf ("vexcel_complex_data_planes <DATA FILE> <METADATAFILE> <BASE>\n");
  exit (0);
}


#undef CALLER
#define CALLER "obtain_metadata"
/*+ Read metadata file +*/

static void obtain_metadata (char *data, /*+ Name of data file +*/
                             char *meta) /*+ Name of metadata file +*/
{
  char metafile_name[PATH_MAX];
  char str[256];		/* String used for error formatting. */
  FILE *metadata, *fF;
  int fd;
  int a;
  //int ret;

	/*** Values to be read ****
		int number_of_samples;
		int number_of_lines;
		int iof_size;
		double pixel_size_range, pixel_size_az;
		double slant_range_to_first_pixel;
		double platform_alt;
		double Re;
		double noise[512];
		int projection;
	********************************/


  strcpy (metafile_name, meta);
  strcat (metafile_name, METADATA_EXT);

  metadata = fopen (metafile_name, "r");
  FILE_check (metadata, metafile_name, CALLER);

  read_metadata_int (metadata, NUMBER_OF_PIXELS, &number_of_samples);
  read_metadata_int (metadata, NUMBER_OF_LINES, &number_of_lines);
  read_metadata_double (metadata, RNG_PIXEL_SPACING, &pixel_size_range);
  read_metadata_double (metadata, SLANT_RANGE_TO_FIRST_PIXEL,
			&slant_range_to_first_pixel);
  read_metadata_double (metadata, PLATFORM_ALITITUDE, &platform_alt);
  read_metadata_double (metadata, EARTH_RADIUS_AT_IMAGE_CENTER, &Re);

  Re = Re * 1000.0;
  platform_alt = platform_alt * 1000.0;
  slant_range_to_first_pixel = slant_range_to_first_pixel * 1000.0;

  fd = open (data, O_RDONLY);
  if (fd < 0)
    {
      char str[256];
      sprintf (str,
	       "Could not open the file \"%s\"(%d:'%s').\n",
	       data, errno, strerror (errno));
      ERROR (CALLER, str, EXIT);
    }

  fF = fdopen (fd, "r");
  FILE_check (fF, data, CALLER);

  if (fseek (fF, START_OF_RDR + 85, SEEK_SET) != 0)
    {
      sprintf (str,
	       "Could not open the RDR (%d:'%s').\n",
	       errno, strerror (errno));
      ERROR (CALLER, str, EXIT);
    }

  fscanf (fF, " %lg", &noise_inc);

  for (a = 0; a < 512; a++)
    fscanf (fF, " %lf ", &(noise_array[a]));

  fscanf (fF, " %lf ", &noise_scale);
  fscanf (fF, " %lf ", &offset);

  ceos_read_char (fd, START_OF_PPR + 534, 10, str);
  if (str[0] == 'A')
    direction = ASENDING;
  else
    direction = DESENDING;

  left_pad = top_pad = bottom_pad = right_pad = 0;
}

/*******************************************************************
*   sigma_
*    Computes sigma0 for a asending image.
*    Approach is from CSA's product description, section 5.3.2
*	dn2		power ( dn squared )
*	j		pixel number, across range
*	look_angle	look angle to platform, in deg
********************************************************************/
static float sigma_nought (float dn2, float j, float look_angle, float inc, float sl)
{
  float beta, sigma;

  if (dn2 == 0)
    return 1E-10;

  beta = 20.0 * log10 (dn2 / sl);
  sigma = pow (10.0, (beta + inc) / 10.0);

  /*

     if (DEBUG)
     {
     printf ("INFO:\t(%s) DN = %g\n", CALLER, dn2);
     printf ("INFO:\t(%s) beta = %g\n", CALLER, beta);
     printf ("INFO:\t(%s) sigma = %g\n", CALLER, beta);
     printf ("INFO:\t(%s) look = %g,inc = %g \n", CALLER, look_angle, inc);
     }

   */

  return sigma;
}

//#undef CALLER
//#define CALLER "sigma_nought_desending"
/*******************************************************************
*   sigma_nought_desending
*    Computes sigma0 for a desending image.
*    Approach is from CSA's product description, section 5.3.2
*       dn2             power ( dn squared )
*       j               pixel number, across range
*       look_angle      look angle to platform, in deg
********************************************************************/
/***
float sigma_nought_desending (float dn2, float j, float look_angle, float inc,
			      float sl)
{
  int Il, Iu;
  float A2;
  float beta, sigma;

  Il = (number_of_samples - j - 1.0) / noise_inc;

  if (Il >= 511)
    {
      A2 = noise_array[511] + (noise_array[511] -
			       noise_array[510]) *
	(((number_of_samples - 1 - j) / noise_inc - 511));
      if (DEBUG)
	printf ("INFO:\t(%s) noise = %g\n", CALLER, A2);

    }
  else
    {
      Iu = Il - 1;
      A2 =
	noise_array[Il] + (noise_array[Iu] -
			   noise_array[Il]) * ((j / noise_inc) - Il);
      if (DEBUG)
	printf ("INFO:\t(%s) noise = %g, noise2 = %g\n", CALLER,
		noise_array[Il], noise_array[Iu]);

    }

  if (dn2 == 0)
    return 0;

  beta = 20.0 * log10 (dn2 / (A2 * A2));
  sigma = pow (10.0, (beta + 10.0 * log10 (sin (inc * M_PI / 180.0))) / 10.0);
  if (DEBUG)
    {
      printf ("INFO:\t(%s) DN = %g\n", CALLER, dn2);
      printf ("INFO:\t(%s) beta = %g\n", CALLER, beta);
      printf ("INFO:\t(%s) sigma = %g\n", CALLER, sigma);
      printf ("INFO:\t(%s) look = %g,inc = %g \n", CALLER, look_angle, inc);
      printf ("INFO:\t(%s) pixel = %g, index = %d\n", CALLER, j, Il);
      printf ("INFO:\t(%s) number_of_samples = %d, noise_inc = %g\n", CALLER,
	      number_of_samples, noise_inc);
    }
  return sigma;
}
*/


#undef CALLER
#define CALLER "generate_data_planes"
static int generate_data_planes (char *infile, char *base)
{
  int f_in, f_data, f_look, f_sigma, idata, qdata;
  int mask = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  char datafile[PATH_MAX], lookfile[PATH_MAX], sigmafile[PATH_MAX];
  char qfile[PATH_MAX], ifile[PATH_MAX];
  int i, j;
  unsigned int I, Q;
  unsigned char *in;
  unsigned int *out_I, *out_Q, *detected;
  float *look, *sigma, *sl_range, *inc;
  double slant_range;
  double delta, initial_range;
  int sz;
  unsigned int ms_I, ls_I, ms_Q, ls_Q;
  unsigned int dn;
  double Ddn;
  int index;

  sz = (number_of_samples + right_pad + left_pad) * SIZE_OF_VEXCEL_COMPLEX +
    SIZE_OF_VEXCEL_IOF;

  in = (unsigned char *) malloc (sizeof (char) * (sz));
  out_I = (unsigned int *) malloc (sizeof (int) * number_of_samples);
  out_Q = (unsigned int *) malloc (sizeof (int) * number_of_samples);
  look = (float *) malloc (sizeof (float) * number_of_samples);
  inc = (float *) malloc (sizeof (float) * number_of_samples);
  sl_range = (float *) malloc (sizeof (float) * number_of_samples);

  sigma = (float *) malloc (sizeof (float) * number_of_samples);
  detected = (unsigned int *) malloc (sizeof (int) * number_of_samples);

  /* Create file names */
  strcpy (datafile, base);
  strcat (datafile, DATA_EXT);

  /* Detected file */
  strcpy (datafile, base);
  strcat (datafile, DATA_EXT);

  /* I file */
  strcpy (ifile, base);
  strcat (ifile, COMPLEX_I_PLANE);

  /* Q file */
  strcpy (qfile, base);
  strcat (qfile, COMPLEX_Q_PLANE);

  strcpy (lookfile, base);
  strcat (lookfile, LOOK_EXT);
  strcpy (sigmafile, base);
  strcat (sigmafile, SIGMA_EXT);

  /* Open files */
  f_data = open (datafile, O_WRONLY | O_CREAT | O_TRUNC, mask);
  file_check (f_data, datafile);

  idata = open (ifile, O_WRONLY | O_CREAT | O_TRUNC, mask);
  file_check (idata, ifile);

  qdata = open (qfile, O_WRONLY | O_CREAT | O_TRUNC, mask);
  file_check (qdata, qfile);

  f_look = open (lookfile, O_WRONLY | O_CREAT | O_TRUNC, mask);
  file_check (f_look, lookfile);

  f_sigma = open (sigmafile, O_WRONLY | O_CREAT | O_TRUNC, mask);
  file_check (f_sigma, sigmafile);

  f_in = open (infile, O_RDONLY);
  file_check (f_in, infile);

  printf ("Noise Vector\nIndex\t\tValue\n");
  lseek (f_in, SIZE_OF_VEXCEL_HEADER, SEEK_SET);


  /* 
     To compute slant range - compute range of near edge ( range pixel of 0) and 
     increment/decrent one pixel for each line.

     delta = size of one pixel.  Negitive if ASENDING, as the left hand side of a image
     is the farthest from the platform.  Positive if DECENDING, as the left hand is nearest
     the platform.
   */

  initial_range = slant_range_to_first_pixel;

  if (direction == ASENDING)
    delta = pixel_size_range;
  else
    delta = pixel_size_range * -1.0;


  /* Look angle is fixed across azmuth - compute before hand */
  for (j = 0, slant_range = initial_range; j < number_of_samples;
       j++, slant_range += delta)
    {
      double A2;
      int Il, Iu;
      look[j] = (float) slant2look (slant_range, Re, platform_alt);
      inc[j] =
	10.0 *
	log10 (sin
	       (look2incidence (look[j], Re, platform_alt) * M_PI / 180.0));
      if (direction == ASENDING)
	{
	  Il = j / noise_inc;
	  if (Il >= 511)
	    {
	      A2 = noise_array[511] + (noise_array[511] -
				       noise_array[510]) * ((j / noise_inc) -
							    511);
	    }
	  else
	    {
	      Iu = Il + 1;
	      A2 = noise_array[Il] + (noise_array[Iu] -
				      noise_array[Il]) * ((j / noise_inc) -
							  Il);
	    }
	}
      else
	{
	  Il = (number_of_samples - j - 1.0) / noise_inc;
	  if (Il >= 511)
	    {
	      A2 = noise_array[511] + (noise_array[511] -
				       noise_array[510]) *
		(((number_of_samples - 1 - j) / noise_inc - 511));

	    }
	  else
	    {
	      Iu = Il - 1;
	      A2 =
		noise_array[Il] + (noise_array[Iu] -
				   noise_array[Il]) * ((j / noise_inc) - Il);
	    }
	}

      sl_range[j] = A2;

      if (j % 512 == 0)
	printf ("%d \t\t%17.5f\n", j, A2);
    }


  printf ("Completed:        ");

  /* Loop through all the lines */
  for (i = 0; i < number_of_lines; i++)
    {
      /* read data */
      if (read (f_in, in, sz) != sz)
	ERROR (CALLER, "Error reading from original data file.", EXIT);

      /* Now, loop through all the pixels.  delta and inital_range are set above. */

      for (j = 0; j < number_of_samples; j++)
	{

	  index = SIZE_OF_VEXCEL_IOF + SIZE_OF_ASF_COMPLEX * (j + left_pad);

	  /* Pick appart the incomming data into the most sig (ms) and least sig (ls)
	     parts for the I and Q values */
	  ms_Q = in[index];
	  ls_Q = in[index + 1];
	  ms_I = in[index + 2];
	  ls_I = in[index + 3];

	  /* build the I and Q values from the ms and ls parts */
	  I = ms_I * 256 + ls_I;
	  Q = ms_Q * 256 + ls_Q;

	  /* 
	     If the ms piece is less than zero, then the whole value was ment to be
	     negitive.  Mask out the upper 16 bits to ffff to set the whole piece to
	     negitive.
	   */
	  if ((char) (ms_Q) < 0)
	    Q = Q ^ 0xffff0000;
	  if ((char) (ms_I) < 0)
	    I = I ^ 0xffff0000;
	  dn = I * I + Q * Q;

	  /* bit flip the I and Q values */
	  out_I[j] = (float) (htonl ((unsigned int) (I)));
	  out_Q[j] = (float) (htonl ((unsigned int) (Q)));

	  /* Compute the dn value from the I and Q values */
	  Ddn = sqrt ((double) (dn));
	  detected[j] = htonl ((unsigned int) Ddn);
	  sigma[j] =
	    (float) (sigma_nought
		     ((float) Ddn, (float) j, look[j], inc[j], sl_range[j]));
	}

      if (write (f_data, detected, number_of_samples * sizeof (int)) !=
	  number_of_samples * sizeof (int))
	ERROR (CALLER, "Error writing to output data file.", EXIT);

      if (write (qdata, out_Q, number_of_samples * sizeof (int)) !=
	  number_of_samples * sizeof (float))
	ERROR (CALLER, "Error writing to output data file.", EXIT);

      if (write (idata, out_I, number_of_samples * sizeof (int)) !=
	  number_of_samples * sizeof (float))
	ERROR (CALLER, "Error writing to output data file.", EXIT);

      if (write (f_look, look, number_of_samples * sizeof (float)) !=
	  number_of_samples * sizeof (float))
	ERROR (CALLER, "Error writing to output look angle file.", EXIT);

      if (write (f_sigma, sigma, number_of_samples * sizeof (float)) !=
	  number_of_samples * sizeof (float))
	ERROR (CALLER, "Error writing to output sigma0 data file.", EXIT);

      if (i % 16 == 0)
	{
	  printf ("\b\b\b\b\b\b\b%% %05.2f",
		  100.0 * (double) i / (double) number_of_lines);
	  fflush (stdout);
	}
    }

  /* Close the output files */
  close (f_data);
  close (f_look);
  close (f_sigma);
  exit(1);
}
