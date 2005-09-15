#include "../util/util.h"
#include "../util/metadata.h"
#include "ceos.h"
#include "errno.h"
#include "asf_endian.h"
/* #include <netinet/in.h>   // for htonl() (put data network byte order) */

#define INFO ( getenv("INFO") != NULL)
#define ASP (1)
#define PP (2)
#define LOW (3)
#define NOT_LOW (4)
#define SLANT (5)
#define NOT_SLANT (6)

   /* globals defining image */
int format;
int processor;
int number_of_samples;
int number_of_lines;
int left_pad;
int right_pad;
int top_pad;
int bottom_pad;
int iof_size;
double pixel_size_range, pixel_size_az;
double slant_range_to_first_pixel;
double platform_alt;
double Re;
double a1, a2, a3;
double noise[256];
int projection;


/* Proto types */
void generate_data_planes (char *infile, char *base);
void write_metadata (char *metafile, char *file);

void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
 	"   %s <dataFile> <metaFile> <outName>\n",name);
 printf("\n"
	"ARGUMENTS:\n"
	"   dataFile   ASF CEOS data file (*.D)\n"
	"   metaFile   ASF CEOS meta data file (*.L)\n"
	"   outName    Base name for the data planes to be output.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts the ASF CEOS data and metadata file set to the SProCKET\n"
	"   data and metadata file set.\n");
 printf("\n"
	"Version 1.00, ASF SAR Tools\n"
	"\n");
 exit (EXIT_FAILURE);
}

   /* globals defining conversion routines */
void write_metadata_item_string (FILE * f, char *item, char *value)
{
  char *str;
  str = (char *) malloc (sizeof (char) * strlen (value));
  strcpy (str, value);
  string_all_trim (str);

  fprintf (f, "%-30s = \"%s\"\n", item, str);
}

void write_metadata_item_double (FILE * f, char *item, double value)
{
  fprintf (f, "%-30s = \"%g\"\n", item, value);
}

void write_metadata_item_int (FILE * f, char *item, int value)
{
  fprintf (f, "%-30s = \"%d\"\n", item, value);
}

int main (int argc, char **argv)
{
  if (argc != 4)
    usage (argv[0]);

  write_metadata (argv[2], argv[3]);
  generate_data_planes (argv[1], argv[3]);
  return 0;
}

#undef CALLER
#define CALLER "generate_data_planes"
void generate_data_planes (char *infile, char *base)
{
   int f_in, f_data, f_look, f_sigma;
   int mask = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
   char datafile[PATH_MAX], lookfile[PATH_MAX], sigmafile[PATH_MAX];
   int ii, jj;
   unsigned char *in;
   float *out, *look, *sigma;
   unsigned int dn;
   double near_ground_range;

   in = (unsigned char *) malloc (sizeof (char) *
                         (number_of_samples + right_pad + left_pad + iof_size));
   out = (float *) malloc (sizeof (float) * number_of_samples);
   look = (float *) malloc (sizeof (float) * number_of_samples);
   sigma = (float *) malloc (sizeof (float) * number_of_samples);

   /* Create file names */
   strcpy (datafile, base);
   strcat (datafile, DATA_EXT);
   strcpy (lookfile, base);
   strcat (lookfile, LOOK_EXT);
   strcpy (sigmafile, base);
   strcat (sigmafile, SIGMA_EXT);

   /* Open files */
   f_data = open (datafile, O_WRONLY | O_CREAT | O_TRUNC, mask);
   file_check (f_data, datafile);

   f_look = open (lookfile, O_WRONLY | O_CREAT | O_TRUNC, mask);
   file_check (f_look, lookfile);

   f_sigma = open (sigmafile, O_WRONLY | O_CREAT | O_TRUNC, mask);
   file_check (f_sigma, sigmafile);

   f_in = open (infile, O_RDONLY);
   file_check (f_in, infile);


   /* Compute ground range to near edge of image */
   /* This is used to compute the slant range of each sample */
   near_ground_range = slant2ground (slant_range_to_first_pixel, Re,
                                     platform_alt);

   lseek (f_in, number_of_samples + right_pad + left_pad + iof_size, SEEK_SET);
   for (ii = 0; ii < number_of_lines; ii++) {
      if (read (f_in, in, number_of_samples + right_pad + left_pad + iof_size)
          != number_of_samples + right_pad + left_pad + iof_size)
         ERROR (CALLER, "Error reading from original data file.", EXIT);

      for (jj = 0; jj < number_of_samples; jj++) {
         dn = in[iof_size + left_pad + jj];

      /* Assign pixel value */
         out[jj] = (float) in[iof_size + left_pad + jj];

      /* Compute look angle */
         if (projection == SLANT)
            look[jj] = slant2look((double)jj * pixel_size_range
                                + slant_range_to_first_pixel, Re, platform_alt);
         else {
            double gr2sr = ground2slant_range((double)jj*pixel_size_range
                                              + near_ground_range,
                                              Re, platform_alt);
            look[jj] = slant2look(gr2sr, Re, platform_alt);
         }

      /* compute sigma0 */
         if (processor == PP) {
            if (format == LOW)
               sigma[jj] = (float)sigma0_low_PP((double)dn,jj,noise,a1,a2,a3);
            else
               sigma[jj] = (float)sigma0_PP((double)dn, jj, noise, a1, a2, a3);
         }
         else
            sigma[jj] = (float)sigma0_ASP((double) dn, jj, noise, a1, a2, a3,
                                         number_of_samples);
      }

   /* Put data in big endian order */
      for (jj = 0; jj<number_of_samples; jj++) {
         ieee_big32(out[jj]);
         ieee_big32(look[jj]);
         ieee_big32(sigma[jj]);
      }
      if (write (f_data, out, number_of_samples * sizeof (float))
          != number_of_samples * sizeof (float))
         ERROR (CALLER, "Error writing to output data file.", EXIT);

      if (write (f_look, look, number_of_samples * sizeof (float))
          != number_of_samples * sizeof (float))
         ERROR (CALLER, "Error writing to output look angle file.", EXIT);

      if (write (f_sigma, sigma, number_of_samples * sizeof (float))
          != number_of_samples * sizeof (float))
         ERROR (CALLER, "Error writing to output sigma0 data file.", EXIT);

      if (ii % 8 == 0) {
         printf ("Completed: %05.2f%%\r",100.0*(double)ii/(double)number_of_lines);
         fflush (stdout);
      }
   }
   printf ("Completed: %05.2f%%\n", 100.0*(double)ii/(double)number_of_lines);

   /* Close the output files */
   close (f_data);
   close (f_look);
   close (f_sigma);
}

void write_metadata (char *metafile, char *file)
{

  FILE *out;
  char output_file[1024];
  struct VFDRECV facdr;
  struct VRADDR dr;
  struct IOF_VFDR vfdr;
  struct dataset_sum_rec dssr;
  int a;

  /* Create the metadata file */
  strcpy (output_file, file);
  strcat (output_file, METADATA_EXT);

  /* Open it */
  out = FOPEN (output_file, "w");

  /* Read faclity data record */
  get_asf_facdr (metafile, &facdr);

  /* Read data set summury record */
  get_dssr (metafile, &dssr);

  /* Read the iof record */
  get_ifiledr (metafile, &vfdr);

  /* Read the radiometric data record */
  get_raddr (metafile, &dr);


  /*-----------------------*
   * WRITE THIS STUFF OUT! *
   *-----------------------*/

  /* VERSION INFO */
  write_metadata_item_string (out, CONVERTER_VERSION,
                              "ASF Standard Detected Converter");

  /* PROCESSING_INFO */
  {
    char buff[256];

    /* Remove spaces */
    string_all_trim (dssr.fac_id);
    string_all_trim (dssr.sys_id);
    string_all_trim (dssr.ver_id);

    sprintf (buff, "%s/%s/%s", dssr.fac_id, dssr.sys_id, dssr.ver_id);
    write_metadata_item_string (out, PROCESSING_INFO, buff);
  }

  /* PROCESSING_DATE */
  {
    char buff[256];
    char stripped[256];

    strcpy (stripped, facdr.coryear);
    string_all_trim (stripped);
    strcpy (buff, strcat (stripped, "/"));

    strcpy (stripped, facdr.cortime);
    string_all_trim (stripped);
    strcat (buff, stripped);

    write_metadata_item_string (out, PROCESSING_DATE, buff);
  }

  /* PLATFORM */
  write_metadata_item_string (out, PLATFORM, dssr.mission_id);

  /* BEAM_MODE */
  if (strncmp(dssr.sensor_id,"ERS",3)==0 || strncmp(dssr.sensor_id,"JERS",4)==0)
     write_metadata_item_string (out, BEAM_MODE, "STD");
  else
     write_metadata_item_string (out, BEAM_MODE, "");

  /* FREQUENCY */
  {
    char str[256];
    /* Just take out the FREQUENCY part */
    str[0] = dssr.sensor_id[7];
    str[1] = 0;
    write_metadata_item_string (out, FREQUENCY, str);
  }

  /* POLARIZATION */
  {
    char str[256];
    strcpy (str, &(dssr.sensor_id[15]));
    write_metadata_item_string (out, POLARIZATION, str);
  }

  /* TRACK_ANGLE */
  write_metadata_item_double (out, TRACK_ANGLE, facdr.trackang);

  /* CLOCK_ANGLE */
  write_metadata_item_double (out, CLOCK_ANGLE, dssr.clock_ang);

  /* PROJECTION */
  write_metadata_item_string (out, PROJECTION, facdr.grndslnt);

  /* NUMBER_OF_PIXELS */
  write_metadata_item_int (out, NUMBER_OF_PIXELS, facdr.apixels);

  /* NUMBER_OF_LINES */
  write_metadata_item_int (out, NUMBER_OF_LINES, facdr.alines);

  /* RNG_PIXEL_SPACING */
  write_metadata_item_double (out, RNG_PIXEL_SPACING, dssr.line_spacing);

  /* AZ_PIXEL_SPACING */
  write_metadata_item_double (out, AZ_PIXEL_SPACING, dssr.pixel_spacing);

  /* CENTER_GMT  */
  {
    char buff[256];
    strcpy (buff, facdr.imgyear);
    buff[4] = '-';
    strcat (buff, facdr.imgtime);
    buff[8] = 'T';
    write_metadata_item_string (out, CENTER_GMT, buff);
  }

  /* SLANT_RANGE_TO_FIRST_PIXEL */
  write_metadata_item_double (out, SLANT_RANGE_TO_FIRST_PIXEL,
               facdr.sltrngfp);

  /* EARTH_RADIUS_AT_IMAGE_CENTER */
  write_metadata_item_double (out, EARTH_RADIUS_AT_IMAGE_CENTER,
               facdr.eradcntr);

  /* EARTH_RADIUS_AT_IMAGE_NARIR */
  write_metadata_item_double (out, EARTH_RADIUS_AT_IMAGE_NARIR,
               facdr.eradnadr);

  /* PLATFORM_ALITITUDE */
  write_metadata_item_double (out, PLATFORM_ALITITUDE, facdr.scalt);

  /* IMAGE_FORMAT */
  write_metadata_item_string (out, IMAGE_FORMAT, STANDARD_FORMAT);

  /* UPPER RIGHT CORNER LOCATION */
  write_metadata_item_double (out, TOP_RIGHT_CORNER_LAT, facdr.farslat);
  write_metadata_item_double (out, TOP_RIGHT_CORNER_LONG, facdr.farslon);

  /* UPPER LEFT CORNER LOCATION */
  write_metadata_item_double (out, TOP_LEFT_CORNER_LAT, facdr.nearslat);
  write_metadata_item_double (out, TOP_LEFT_CORNER_LONG, facdr.nearslon);

  /* LOWER RIGHT CORNER LOCATION */
  write_metadata_item_double (out, BOTTOM_RIGHT_CORNER_LAT, facdr.farelat);
  write_metadata_item_double (out, BOTTOM_RIGHT_CORNER_LONG, facdr.farelon);

  /* LOWER LEFT CORNER LOCATION */
  write_metadata_item_double (out, BOTTOM_LEFT_CORNER_LAT, facdr.nearelat);
  write_metadata_item_double (out, BOTTOM_LEFT_CORNER_LONG, facdr.nearelon);

  /* ELLIPS_MAJ_AXIS */
  write_metadata_item_double (out, ELLIPS_MAJ_AXIS, 6378.144);

  /* ELLIPS_MIN_AXIS */
  write_metadata_item_double (out, ELLIPS_MIN_AXIS, 6356.755);

  /* REVOLUTION */
  write_metadata_item_int (out, REVOLUTION, atoi(dssr.revolution));

  /* FLIGHT_DIRECTION */
  write_metadata_item_string (out, FLIGHT_DIRECTION, dssr.asc_des);

  /* PRF */
  write_metadata_item_double (out, PRF, dssr.prf);

  /* DOPPLER_POLY_A0 */
  write_metadata_item_double (out, DOPPLER_POLY_A0, dssr.crt_dopcen[0]);

  /* DOPPLER_POLY_A1 */
  write_metadata_item_double (out, DOPPLER_POLY_A1, dssr.crt_dopcen[1]);

  /* DOPPLER_POLY_A2 */
  write_metadata_item_double (out, DOPPLER_POLY_A2, dssr.crt_dopcen[2]);

  /* CLOSE THE METADATA FILE */
  fclose (out);


  /*------------------------------------------------*
   * SAVE PARAMETERS NECESSARY FOR CONVERTING IMAGE *
   *------------------------------------------------*/

  /* Image size numbers */
  number_of_samples = facdr.apixels;
  number_of_lines = facdr.alines;

/*  left_pad = vfdr.lbrdrpxl;
 *  right_pad = vfdr.rbrdrpxl;
 *  top_pad = vfdr.topbrdr;
 *  bottom_pad = vfdr.botbrdr;
 */

  /* Pixel size information */
  pixel_size_range = dssr.line_spacing;
  pixel_size_az = dssr.pixel_spacing;
  slant_range_to_first_pixel = facdr.sltrngfp * 1000.0;
  platform_alt = facdr.scalt * 1000.0;
  Re = facdr.eradcntr * 1000.0;
  iof_size = vfdr.predata;

  /* Calibration coefs */
  a1 = dr.a[0];
  a2 = dr.a[1];
  a3 = dr.a[2];

  /* Store noise vector */
  for (a = 0; a < 256; a++)
    noise[a] = dr.noise[a];

  /* Determine processor */
  if (strstr (dssr.sys_id, "ASP") != NULL)
    processor = ASP;
  else
    processor = PP;

  /* Determine format */
  if (strstr (dssr.product_type, "LOW") != NULL)
    format = LOW;
  else
    format = NOT_LOW;

  /* Determine if ground range or slant */
  if (strstr (facdr.grndslnt, "GROUND") != NULL)
    projection = NOT_SLANT;
  else
    projection = SLANT;


  /*-----------------------------------------------------------*
   * BLATHER AT USER IF THEIR ENVIRONMENT SAYS THEY WANT US TO *
   *-----------------------------------------------------------*/
  if (INFO) {
    write_metadata_item_int (stdout, "number_of_samples", number_of_samples);
    write_metadata_item_int (stdout, "number_of_lines", number_of_lines);
    write_metadata_item_int (stdout, "left_pad", left_pad);
    write_metadata_item_int (stdout, "right_pad", right_pad);
    write_metadata_item_int (stdout, "top_pad", top_pad);
    write_metadata_item_int (stdout, "bottom_pad", bottom_pad);
    write_metadata_item_double (stdout, "pixel_size_range",pixel_size_range);
    write_metadata_item_double (stdout, "pixel_size_az", pixel_size_az);
    write_metadata_item_double (stdout, "slant_range_to_first_pixel",
                                slant_range_to_first_pixel);
    write_metadata_item_double (stdout, "platform_alt", platform_alt);
    write_metadata_item_double (stdout, "Re", Re);
    write_metadata_item_int (stdout, "Size of iof", iof_size);
  }
}
