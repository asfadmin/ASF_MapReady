/**** RSI CEOS to Sprocket format converter for data_planes ****/

#include <math.h>
#include <errno.h>
#include "../util/util.h"
#include "../util/metadata.h"
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

#define ASCENDING (0)
#define DESCENDING (1)

#define SIZE_OF_VEXCEL_HEADER (16252)
#define SIZE_OF_VEXCEL_IOF (192)
#define SIZE_OF_VEXCEL_COMPLEX ( 4 )
#define DEBUG ( getenv("DEBUG") != NULL)

/* Prototypes */
int main (int argc, char **argv);
void usage (char *name);
static int generate_data_planes (char *infile, char *base);
static void obtain_metadata (char *datain, char *metaout);
static float sigma_nought (float dn2, float j, float look_angle, float inc, float sl);


int main (int argc, char **argv)
{
  clock_t start, stop;
  if (argc == 1)
    usage (argv[0]);

  start = times (NULL);

  obtain_metadata (argv[2], argv[3]);
  generate_data_planes (argv[1], argv[3]);

  stop = times (NULL);

  printf ("\n\nConversion took %f minites.\n",
	  (double) (stop - start) / (double) CLK_TCK / 60.0);

  return 0;
}


void usage (char *name)
{
  printf ("%s <DATA FILE> <METADATAFILE> <BASE>\n",name);
  exit (EXIT_FAILURE);
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
    direction = ASCENDING;
  else
    direction = DESCENDING;

  left_pad = top_pad = bottom_pad = right_pad = 0;
}

/*******************************************************************
*   sigma_
*    Computes sigma0 for a ASCENDING image.
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
//#define CALLER "sigma_nought_DESCENDING"
/*******************************************************************
*   sigma_nought_DESCENDING
*    Computes sigma0 for a DESCENDING image.
*    Approach is from CSA's product description, section 5.3.2
*       dn2             power ( dn squared )
*       j               pixel number, across range
*       look_angle      look angle to platform, in deg
********************************************************************/
/***
float sigma_nought_DESCENDING (float dn2, float j, float look_angle, float inc,
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

#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"

#define SQR(X) ((X)*(X))

#undef CALLER
#define CALLER "generate_data_planes"
static int generate_data_planes (char *inDataName, char *base)
{
   FILE *inDataFile;
   FILE *outDetectedFile, *outLookFile, *outSigmaFile, *outIFile, *outQFile;
   char detectedName[PATH_MAX], lookName[PATH_MAX], sigmaName[PATH_MAX];
   char Q_Name[PATH_MAX], I_Name[PATH_MAX];
   int ii, jj;
   unsigned char *inBuf;
   float *detectedBuf, *lookBuf, *sigmaBuf, *I_Buf, *Q_Buf;
   float *slantRangeBuf, *incidAngBuf;
   double slant_range;
   double delta;
   int inLineLen = (number_of_samples+right_pad+left_pad)*SIZE_OF_VEXCEL_COMPLEX
                   + SIZE_OF_VEXCEL_IOF;
   int inBufferSize = sizeof(char) * inLineLen;
   int outBufferSize = sizeof(float) * number_of_samples;
   unsigned int I_msb, I_lsb, Q_msb, Q_lsb;
   int I, Q;
   int index;

/* Create file names */
   strcat( strcpy(detectedName,base), DATA_EXT);
   strcat( strcpy(I_Name,base),       COMPLEX_I_PLANE);
   strcat( strcpy(Q_Name,base),       COMPLEX_Q_PLANE);
   strcat( strcpy(lookName,base),     LOOK_EXT);
   strcat( strcpy(sigmaName,base),    SIGMA_EXT);

/* Open files */
   inDataFile      = FOPEN(inDataName,   "r");
   outDetectedFile = FOPEN(detectedName, "w");
   outIFile        = FOPEN(I_Name,       "w");
   outQFile        = FOPEN(Q_Name,       "w");
   outLookFile     = FOPEN(lookName,     "w");
   outSigmaFile    = FOPEN(sigmaName,    "w");

/* Allocate in/out buffer memory */
   inBuf         = (unsigned char *) MALLOC (inBufferSize);
   detectedBuf   = (float *) MALLOC (outBufferSize);
   I_Buf         = (float *) MALLOC (outBufferSize);
   Q_Buf         = (float *) MALLOC (outBufferSize);
   lookBuf       = (float *) MALLOC (outBufferSize);
   incidAngBuf   = (float *) MALLOC (outBufferSize);
   slantRangeBuf = (float *) MALLOC (outBufferSize);
   sigmaBuf      = (float *) MALLOC (outBufferSize);

/* To compute slant range - compute range of near edge ( range pixel of 0) and
 * increment/decrent one pixel for each line.
 *
 * delta = size of one pixel.  Negitive if ASCENDING, as the left hand side of
 * a image is the farthest from the platform.  Positive if DESCENDING, as the
 * left hand is nearest the platform.
 */
   delta = (direction==ASCENDING) ? pixel_size_range : pixel_size_range*(-1.0);
   
   printf("Noise Vector\n");
   printf("Index\t\tValue\n");
   slant_range = slant_range_to_first_pixel;

/* Look angle is fixed across azmuth - compute before hand */
   for (jj = 0; jj < number_of_samples; jj++) {
      double A2;
      int Il, Iu;
      double incidence;

      lookBuf[jj]     = (float) slant2look (slant_range, Re, platform_alt);
      incidence       = look2incidence (lookBuf[jj], Re, platform_alt);
      incidAngBuf[jj] = 10.0 * log10 (sin (incidence * M_PI / 180.0));

      if (direction == ASCENDING) {
         Il = jj / noise_inc;
         if (Il >= 511) {
            A2 = noise_array[511]
                 + (noise_array[511]-noise_array[510]) * ((jj/noise_inc)-511);
         }
         else {
            Iu = Il + 1;
            A2 = noise_array[Il]
                 + (noise_array[Iu]-noise_array[Il]) * ((jj/noise_inc)-Il);
         }
      }
      else {
         Il = (number_of_samples-jj-1.0) / noise_inc;
         if (Il >= 511) {
            A2 = noise_array[511] + (noise_array[511]-noise_array[510])
                 * ((number_of_samples-1-jj) / noise_inc - 511);
         }
         else {
            Iu = Il - 1;
            A2 = noise_array[Il]
                 + (noise_array[Iu]-noise_array[Il]) * ((jj/noise_inc)-Il);
         }
      }
      slantRangeBuf[jj] = A2;
      slant_range += delta;
      if (jj%512 == 0)   printf ("% 6d       % 17.5f\n", jj, A2);
   }

   FSEEK64 (inDataFile, SIZE_OF_VEXCEL_HEADER, SEEK_SET);

/* Read, calculate, and write through all the lines */
   for (ii=0; ii<number_of_lines; ii++) {
      /* read data */
      FREAD(inBuf, sizeof(char), inLineLen, inDataFile);

   /* Now, loop through all the pixels. */
      for (jj=0; jj<number_of_samples; jj++) {
         index = SIZE_OF_VEXCEL_IOF + SIZE_OF_ASF_COMPLEX * (jj + left_pad);

      /* Pick apart the incoming data into the most significant byte (msb) and
       * least signficant byte (lsb) for the I and Q values */
         Q_msb = inBuf[index];
         Q_lsb = inBuf[index + 1];
         I_msb = inBuf[index + 2];
         I_lsb = inBuf[index + 3];

      /* Build the I and Q values from the msb and lsb parts */
         I = I_msb*256 + I_lsb;
         Q = Q_msb*256 + Q_lsb;

      /*If the ms piece is less than zero, then the whole value was ment to be
        negitive.  Mask out the upper 16 bits to ffff to set the whole piece to
        negitive.*/
         if ((char) (Q_msb) < 0)  Q = Q ^ 0xffff0000;
         if ((char) (I_msb) < 0)  I = I ^ 0xffff0000;

      /* Cast I & Q to floating point and fill the I & Q buffers */
         I_Buf[jj] = (float) ((unsigned int)I);
         Q_Buf[jj] = (float) ((unsigned int)Q);

      /* Compute the detected value from the I and Q values */
         detectedBuf[jj] = (float)sqrt((double)(SQR(Q)+SQR(I)));
         
      /* Figure the Sigma0 value and we're ready to write! */
         sigmaBuf[jj]    = (float) sigma_nought( (float)detectedBuf[jj],
                                                 (float)jj, lookBuf[jj],
                                                 incidAngBuf[jj],
                                                 slantRangeBuf[jj]);
      }

   /* Put data in big endian order (SProCKET & asf_tools read big endian) */
      for (jj = 0; jj<number_of_samples; jj++) {
         ieee_big32(detectedBuf[jj]);
         ieee_big32(Q_Buf[jj]);
         ieee_big32(I_Buf[jj]);
         ieee_big32(lookBuf[jj]);
         ieee_big32(sigmaBuf[jj]);
      }

   /* Write data to file */
      FWRITE(detectedBuf, sizeof(float), number_of_samples, outDetectedFile);
      FWRITE(Q_Buf,       sizeof(float), number_of_samples, outQFile);
      FWRITE(I_Buf,       sizeof(float), number_of_samples, outIFile);
      FWRITE(lookBuf,     sizeof(float), number_of_samples, outLookFile);
      FWRITE(sigmaBuf,    sizeof(float), number_of_samples, outSigmaFile);

      if (ii%100 == 0) {
         printf ("Completed: % 3d%%\r",(100*ii)/number_of_lines);
         fflush (stdout);
      }
   }
   printf ("Completed: % 3d\n%%",(100*ii)/number_of_lines);

/* Free malloc'd memory */
   FREE(inBuf);
   FREE(detectedBuf);
   FREE(I_Buf);
   FREE(Q_Buf);
   FREE(lookBuf);
   FREE(incidAngBuf);
   FREE(slantRangeBuf);
   FREE(sigmaBuf);

/* Close the output files */
   FCLOSE (outDetectedFile);
   FCLOSE (outQFile);
   FCLOSE (outIFile);
   FCLOSE (outLookFile);
   FCLOSE (outSigmaFile);
   exit(EXIT_SUCCESS);
}
