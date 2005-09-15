/*******************************************************************************
File Name: vexcel_complex_metadata.c

Usage:
  convert_complex <DATA FILE> <METADATAFILE> <VOLFILE> <BASE>

Purpose:
  RSI CEOS to Sprocket format converter for metadata.

Revision History:
  Author     Date      Description
  --------   --------  -----------------------------------------------------
  Jay Cable  ??/??/??  Initial creation
  mmoore     07/11/02  Added comments and renamed some variables for clarity
  P. Denny   ??/??/??  Changed headers to fit with asf_tools
*******************************************************************************/

#include "../util/util.h"
#include "../util/metadata.h"
#include "ceos_rd.h"

#define INFO ( getenv("INFO") != NULL)
#define ASP (1)
#define PP (2)
#define LOW (3)
#define NOT_LOW (4)
#define SLANT (5)
#define NOT_SLANT (6)


/* prototypes */
int main (int argc, char **argv);

static void write_metadata (char *metafile, char *datafile, char *volfile, char *file);
static void write_metadata_item_double (FILE * f, char *item, double value);
static void write_metadata_item_int (FILE * f, char *item, int value);
static void write_metadata_item_string (FILE * f, char *item, char *value);

static void processor_id_string (char *str, int fd);

static double platform_height (int fd, int vol_fd);
static double radius_of_earth (int fd);

static void frequency_string (char *str, int fd);
static void polarization_string (char *str, int fd);

static char *center_time (char *str, int fd);

static int polint (double *xa, double *ya, int n, double x, double *y, double *dy);

static void usage ();


/*+++ globals defining conversion routines +++*/

static void write_metadata_item_string (FILE * f, char *item, char *value)
{
   char *str;
   str = (char *) malloc (sizeof (char) * strlen (value) + 2);
   strcpy (str, value);
   string_all_trim (str);

   fprintf (f, "%-30s = \"%s\"\n", item, str);

   free (str);
   fflush (f);
}

static void write_metadata_item_double (FILE * f, char *item, double value)
{
   fprintf (f, "%-30s = \"%20g\"\n", item, value);
   fflush (f);
}

static void write_metadata_item_int (FILE * f, char *item, int value)
{
   fprintf (f, "%-30s = \"%d\"\n", item, value);
   fflush (f);
}

static void usage ()
{
   printf ("convert_complex <DATA FILE> <METADATAFILE> <VOLFILE> <BASE>\n");
   exit (EXIT_FAILURE);
}

int main (int argc, char **argv)
{
   if (argc == 1)
      usage ();
   write_metadata (argv[2], argv[1], argv[3], argv[4]);
   return 0;
}

#undef CALLER
#define CALLER "write_metadata"
static void write_metadata (char *metafile, char *datafile, char *volfile,
                            char *file)
{

   FILE *out;
   /*char buff1[1024];*/
   char output_file[1024];
   char tmp_value[256];
   int fd, data_fd, vol_fd;
   /*int a;*/
   double Re;
   /*double h;*/
   /*int npixels, nlines;*/
   char buff[512];

   /* Create the metadata file */
   strcpy (output_file, file);
   strcat (output_file, METADATA_EXT);

   /* Open it */
   out = fopen (output_file, "w");
   if (out == NULL)
   {
      char str[256];
      sprintf (str,
         "Could not open the file \"%s\" for writing(%d:'%s').\n",
      file, errno, strerror (errno));

      ERROR (CALLER, str, EXIT);
   }

   fd = open (metafile, O_RDONLY);
   if (fd < 0)
   {
      char str[256];
      sprintf (str,
         "Could not open the file \"%s\"(%d:'%s').\n",
      metafile, errno, strerror (errno));
      ERROR (CALLER, str, EXIT);
   }


   data_fd = open (datafile, O_RDONLY);
   if (data_fd < 0)
   {
      char str[256];
      sprintf (str,
         "Could not open the file \"%s\"(%d:'%s').\n",
      datafile, errno, strerror (errno));
      ERROR (CALLER, str, EXIT);
   }

   vol_fd = open (volfile, O_RDONLY);
   if (vol_fd < 0)
   {
      char str[256];
      sprintf (str,
         "Could not open the file \"%s\"(%d:'%s').\n",
      datafile, errno, strerror (errno));
      ERROR (CALLER, str, EXIT);
   }


   /* Compute Radius of geoid at image center */
   Re = radius_of_earth (fd);

   /* VERSION INFO */
   write_metadata_item_string (out, CONVERTER_VERSION,
                               "ASF Vexcel Complex Converter");

   /* PROCESSING_INFO */
   processor_id_string (buff, fd);
   write_metadata_item_string (out, PROCESSING_INFO, buff);

   /* PROCESSING_DATE */
   write_metadata_item_string (out, PROCESSING_DATE, "");

   /* PLATFORM */
   ceos_read_char (fd, START_OF_PPR + 7274, 10, tmp_value);
   write_metadata_item_string (out, PLATFORM, tmp_value);

   /* BEAM_MODE */
   ceos_read_char (fd, START_OF_PPR + 932, 3, tmp_value);
   write_metadata_item_string (out, BEAM_MODE, tmp_value);

   /* FREQUENCY */
   frequency_string (buff, fd);
   write_metadata_item_string (out, FREQUENCY, buff);

   /* POLARIZATION */
   polarization_string (buff, fd);
   write_metadata_item_string (out, POLARIZATION, buff);

   /* TRACK_ANGLE */
   write_metadata_item_double (out, TRACK_ANGLE,
                               ceos_read_double (fd, START_OF_DSSR + 469, 8));

   /* CLOCK_ANGLE */
   write_metadata_item_double (out, CLOCK_ANGLE,
                               ceos_read_double (fd, START_OF_DSSR + 477, 8));

   /* PROJECTION */
   write_metadata_item_string (out, PROJECTION, "GROUND");

   /* NUMBER_OF_PIXELS */
   write_metadata_item_int (out, NUMBER_OF_PIXELS,
                            ceos_read_binary (data_fd, START_OF_PDR + 25, 4));

   /* NUMBER_OF_LINES */
   write_metadata_item_int (out, NUMBER_OF_LINES,
                            ceos_read_int (data_fd, START_OF_IOF + 181, 6));

   /* RNG_PIXEL_SPACING */
   write_metadata_item_double (out, RNG_PIXEL_SPACING,
                               ceos_read_double (fd, START_OF_DSSR + 1703, 16));

   /* AZ_PIXEL_SPACING */
   write_metadata_item_double (out, AZ_PIXEL_SPACING,
                               ceos_read_double (fd, START_OF_DSSR + 1687, 16));

   /* CENTER_GMT */
   write_metadata_item_string (out, CENTER_GMT, center_time (buff, vol_fd));

   /* SLANT_RANGE_TO_FIRST_PIXEL */
   write_metadata_item_double (out, SLANT_RANGE_TO_FIRST_PIXEL,
            ((double)ceos_read_binary(data_fd, START_OF_PDR + 65, 4)) / 1000.0);

   /* EARTH_RADIUS_AT_IMAGE_CENTER */
   write_metadata_item_double (out, EARTH_RADIUS_AT_IMAGE_CENTER, Re / 1000.0);

   /* EARTH_RADIUS_AT_IMAGE_NARIR */
   write_metadata_item_double (out, EARTH_RADIUS_AT_IMAGE_NARIR, Re / 1000.0);

   /* PLATFORM_ALTITUDE (Detemined) */
   write_metadata_item_double (out, PLATFORM_ALITITUDE,
                               platform_height (fd, vol_fd));

   /* IMAGE_FORMAT */
   write_metadata_item_string (out, IMAGE_FORMAT, COMPLEX_FORMAT);

   /* GET THE IMAGE CORNERS */
   {
      int number_of_lines;
      int number_of_samples;
      double ONE_MILL = 1000000;

      number_of_samples = ceos_read_binary (data_fd, START_OF_PDR + 9, 4);
      number_of_lines = ceos_read_int (data_fd, START_OF_IOF + 181, 6);

      /* TOP_RIGHT_CORNER_LAT/LONG */
      write_metadata_item_double (out, TOP_RIGHT_CORNER_LAT,
         ceos_read_binary (data_fd, START_OF_PDR + 141, 4) / ONE_MILL);
      write_metadata_item_double (out, TOP_RIGHT_CORNER_LONG,
         ceos_read_binary (data_fd, START_OF_PDR + 153, 4) / ONE_MILL);
      /* TOP_LEFT_CORNER_LAT/LONG */
      write_metadata_item_double (out, TOP_LEFT_CORNER_LAT,
         ceos_read_binary (data_fd, START_OF_PDR + 133, 4) / ONE_MILL);
      write_metadata_item_double (out, TOP_LEFT_CORNER_LONG,
         ceos_read_binary (data_fd, START_OF_PDR + 145, 4) / ONE_MILL);
      /* BOTTOM_RIGHT_CORNER_LAT/LONG */
      write_metadata_item_double (out, BOTTOM_RIGHT_CORNER_LAT,
         ceos_read_binary (data_fd,
            START_OF_PDR + number_of_samples * (number_of_lines - 1) + 141, 4)
         / ONE_MILL);
      write_metadata_item_double (out, BOTTOM_RIGHT_CORNER_LONG,
         ceos_read_binary (data_fd,
            START_OF_PDR + number_of_samples * (number_of_lines - 1) + 153, 4)
         / ONE_MILL);
      /* BOTTOM_LEFT_CORNER_LAT/LONG */
      write_metadata_item_double (out, BOTTOM_LEFT_CORNER_LAT,
         ceos_read_binary (data_fd,
            16252 + number_of_samples * (number_of_lines - 2) + 133, 4)
         / ONE_MILL);
      write_metadata_item_double (out, BOTTOM_LEFT_CORNER_LONG,
         ceos_read_binary (data_fd,
            START_OF_PDR + number_of_samples * (number_of_lines - 2) + 145, 4)
         / ONE_MILL);
   }

   /* ELLIPS_MAJ_AXIS (geoid stuff) */
   write_metadata_item_double (out, ELLIPS_MAJ_AXIS,
                                ceos_read_double (fd, START_OF_DSSR + 181, 16));

   /* ELLIPS_MIN_AXIS (geoid stuff) */
   write_metadata_item_double (out, ELLIPS_MIN_AXIS,
                                ceos_read_double (fd, START_OF_DSSR + 197, 16));

   /* REVOLUTION */
   write_metadata_item_int (out, REVOLUTION,
                            ceos_read_int (fd, START_OF_DSSR + 445, 8));

   /* FLIGHT_DIRECTION */
   ceos_read_char (fd, START_OF_DSSR + 101, 16, tmp_value);
   write_metadata_item_string (out, FLIGHT_DIRECTION, tmp_value);

   /* PRF */
   write_metadata_item_double (out, PRF,
                               ceos_read_double (fd, START_OF_DSSR + 935, 16));

   /* DOPPLER POLY_A0 */
   write_metadata_item_double (out, DOPPLER_POLY_A0,
                               ceos_read_double (fd, START_OF_DSSR + 1479, 16));

   /* DOPPLER POLY_A1 */
   write_metadata_item_double (out, DOPPLER_POLY_A1,
                          ceos_read_double (fd, START_OF_DSSR + 1479 + 16, 16));

   /* DOPPLER POLY_A2 */
   write_metadata_item_double (out, DOPPLER_POLY_A2,
                          ceos_read_double (fd, START_OF_DSSR + 1479 + 32, 16));


   /*========== Vexcel Unique stuff ==============*/

   /* What file is being used for the antenna pattern */
   ceos_read_char (fd, START_OF_DSSR + 2020, 256, buff);
   write_metadata_item_string (out, VEXCEL_ANTENNA_PATTERN, buff);

   /* If the image is generated in Sigma or Beta nought */
   ceos_read_char (fd, START_OF_DSSR + 2275, 32, buff);
   write_metadata_item_string (out, VEXCEL_BETA_OR_SIGMA, buff);

   fclose (out);
}

static double platform_height (int fd, int vol_fd)
{
   double height[16];
   double times[16];
   double derror;
   double x, y, z;
   char buff[512];
   int month, day, year, jday, hour, min;
   double gmt_second;
   double data_inc;
   char date[256];
   time_t root;
   struct tm dt;

   int address;
   int a;


   /* Read the state vectors */
   address = START_OF_PPDR + 387;
   for (a = 0; a < 15; a++, address += 22 * 6)
   {
      x = ceos_read_double (fd, address, 22);
      y = ceos_read_double (fd, address + 22, 22);
      z = ceos_read_double (fd, address + 44, 22);
      height[a + 1] = sqrt (x * x + y * y + z * z);
   }

   /*  Read the date of the first state vector  */
   year = ceos_read_int (fd, START_OF_PPDR + 145, 4);
   month = ceos_read_int (fd, START_OF_PPDR + 149, 4);
   day = ceos_read_int (fd, START_OF_PPDR + 153, 4);
   jday = ceos_read_int (fd, START_OF_PPDR + 157, 4);
   gmt_second = ceos_read_double (fd, START_OF_PPDR + 161, 22);


   /* Convert date to seconds from 1970 */
   dt.tm_mon = month - 1;
   dt.tm_mday = day;
   dt.tm_year = year - 1900;
   dt.tm_hour = 0;
   dt.tm_min = 0;
   dt.tm_sec = 0;
   dt.tm_isdst = -1;
   dt.tm_sec = gmt_second;
   root = mktime (&dt);

   /* Read the time inc from one state vector to another */
   data_inc = ceos_read_double (fd, START_OF_PPDR + 183, 22);

   /* Compute the time for each SV */
   for (a = 0; a < 15; a++)
      times[a + 1] = ((double) root) + (double) (a) * data_inc;

   /* Read the  center time */
   ceos_read_char (vol_fd, 0x648 + 4, 21, date);
   string_all_trim (date);

   /* Parse the date pieces */

   /* Year */
   strcpy (buff, &(date[1]));
   buff[4] = 0;
   year = atoi (buff);

   /* Month */
   strcpy (buff, &(date[5]));
   buff[2] = 0;
   month = atoi (buff);

   /* Day */
   strcpy (buff, &(date[7]));
   buff[2] = 0;
   day = atoi (buff);

   /* Hour */
   strcpy (buff, &(date[11]));
   buff[2] = 0;
   hour = atoi (buff);

   /* Min */
   strcpy (buff, &(date[13]));
   buff[2] = 0;
   min = atoi (buff);

   /* Second */
   strcpy (buff, &(date[15]));
   buff[2] = 0;
   gmt_second = (double) atoi (buff);

   strcpy (buff, &(date[17]));
   buff[3] = 0;
   gmt_second += 1.0 / (double) atoi (buff);

   dt.tm_mday = day;
   dt.tm_year = year - 1900;
   dt.tm_mon = month - 1;
   dt.tm_hour = hour;
   dt.tm_min = min;
   dt.tm_sec = 0;
   dt.tm_yday = 0;
   dt.tm_isdst = -1;

   /* Compute time from 1970 */
   root = mktime (&dt);
   z = root + gmt_second;

   /* Figure out hieght for center time using 15 SV */


   polint (times, height, 15, z, &y, &derror);
   printf("Estimated platform altitude %g (km)\n",
          (y-radius_of_earth(fd))/1000.0);
   printf ("Estimated error in platform altitude = %g (km)\n", derror/1000.0);

   if (getenv ("DEBUG") != NULL)
   {
      printf("\n");
      for (a = 1; a < 16; a++)
         printf ("SV # %2d \t %16f \t %16f\n", a, times[a], height[a]);
      printf("\n");
   }


   /* Return hieght in Km */
   return ((y - radius_of_earth (fd)) / 1000.0);

}

/* Computes the radius of the earth at image center (nadir) */
/* uses formula from tom logan's write up, not from RSI specs */

static double radius_of_earth (int fd)
{
   double ellip_major, ellip_minor, plat_lat, pix_spacing;
   double eph_orb_data;
   /*double t, n_data_pixel, srgr_coef;*/
   double theta;
   double e;

   ellip_major = ceos_read_double (fd, START_OF_DSSR + 181, 16);
   ellip_minor = ceos_read_double (fd, START_OF_DSSR + 197, 16);
   plat_lat = ceos_read_double (fd, START_OF_DSSR + 453, 8);
   pix_spacing = ceos_read_double (fd, START_OF_DSSR + 1703, 16);
   eph_orb_data = ceos_read_double (fd, START_OF_PPR + 4649, 16);

   /* E is the excentricity of the earth */
   e =
   sqrt (ellip_major * ellip_major -
   ellip_minor * ellip_minor) / ellip_major;

   /* Theta is the earth centric latitude */
   theta = atan (tan (plat_lat * M_PI / 180.0) * (1.0 - e * e));

   return 1000.0 * (ellip_major * ellip_minor) /
   sqrt (pow (ellip_minor * cos (theta), 2.0) +
   pow (ellip_major * sin (theta), 2.0));


   /* Compute Radius of the earth at image center */
   /* Formula from RSI specs - not accurate!
   t = tan (plat_lat * 180.0 / M_PI);
    t = t * t; */

   /*
   return (ellip_minor * 1000.0 * sqrt (1 + t)
   / sqrt ((ellip_minor * ellip_minor) / (ellip_major * ellip_major) +
   t));
    */
}

static void processor_id_string (char *str, int fd)
{
   char processor_id[60], file_id[60];

   /* Read processor id */
   ceos_read_char (fd, 33, 11, processor_id);

   /* Read file id */
   ceos_read_char (fd, 49, 16, file_id);

   /* Remove spaces */
   string_all_trim (processor_id);
   string_all_trim (file_id);

   sprintf (str, "%s/%s", processor_id, file_id);
}


static void frequency_string (char *str, int fd)
{
   ceos_read_char (fd, START_OF_DSSR + 413, 32, str);
   /* Just take out the FREQUENCY part */
   str[0] = str[7];
   str[1] = 0;
}

static void polarization_string (char *str, int fd)
{
   char str2[256];
   ceos_read_char (fd, START_OF_DSSR + 413, 32, str2);

   strcpy (str, &(str2[15]));
}



static char *center_time (char *str, int vol_fd)
{
   char buff[50];
   char date[50];
   int year, month, day, hour, min;
   double second;
   struct tm dt;

   /* PROCESSING_DATE */
   ceos_read_char (vol_fd, 0x648 + 4, 21, date);
   string_all_trim (date);

   /* Year */
   strcpy (buff, &(date[1]));
   buff[4] = 0;
   year = atoi (buff);

   /* Month */
   strcpy (buff, &(date[5]));
   buff[2] = 0;
   month = atoi (buff);

   /* Day */
   strcpy (buff, &(date[7]));
   buff[2] = 0;
   day = atoi (buff);

   /* Hour */
   strcpy (buff, &(date[11]));
   buff[2] = 0;
   hour = atoi (buff);

   /* Min */
   strcpy (buff, &(date[13]));
   buff[2] = 0;
   min = atoi (buff);

   /* Second */
   strcpy (buff, &(date[15]));
   buff[2] = 0;
   second = (double) atoi (buff);

   strcpy (buff, &(date[17]));
   buff[3] = 0;
   second += 1.0 / (double) atoi (buff);


   dt.tm_mday = day;
   dt.tm_year = year - 1900;
   dt.tm_mon = month - 1;
   dt.tm_hour = hour;
   dt.tm_min = min;
   dt.tm_sec = 0;
   dt.tm_yday = 0;
   dt.tm_isdst = -1;

   mktime (&dt);

   /* Hack for improper handling of zero padding for %f arguements */
   if (second > 9.0)
   sprintf (str, "%04d-%03dT%02d:%02d:%02.3f", year, dt.tm_yday + 1, hour,
   min, second);
   else
   sprintf (str, "%04d-%03dT%02d:%02d:0%2.3f", year, dt.tm_yday + 1, hour,
   min, second);
   return str;
}


/* Linear interpolation function from Num Rec. in C pg 109-110 */
static int polint (double *xa, double *ya, int n, double x, double *y, double *dy)
{
   int i, m, ns = 1;
   float den, dif, dift, ho, hp, w;
   float *c, *d;

   dif = fabs (x - xa[1]);
   /*
   if( (c=vector(1,n)) == NULL){ return(-1); }
   if( (d=vector(1,n)) == NULL){ return(-1); }
       */
   c = (float *) malloc (sizeof (float) * (n + 3));
   d = (float *) malloc (sizeof (float) * (n + 3));
   for (i = 1; i <= n; i++)
   {
      if ((dift = fabs (x - xa[i])) < dif)
      {
         ns = i;
         dif = dift;
      }
      c[i] = ya[i];
      d[i] = ya[i];
   }
   *y = ya[ns--];
   for (m = 1; m < n; m++)
   {
      for (i = 1; i <= n - m; i++)
      {
         ho = xa[i] - x;
         hp = xa[i + m] - x;
         w = c[i + 1] - d[i];
         if ((den = ho - hp) == 0.0)
         {
            fprintf (stderr, "Error in routine POLINT");
            return (-1);
         }
         den = w / den;
         d[i] = hp * den;
         c[i] = ho * den;
      }
      *y += (*dy = (2 * ns < (n - m) ? c[ns + 1] : d[ns--]));
   }
   /*
   free_vector(d,1,n);
   free_vector(c,1,n);
    */
   return (0);
}
