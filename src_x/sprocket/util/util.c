/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
File Name: util.c

Sub Functions:

Purpose:
  Global functions used by other Sprocket modules.

Inputs
  Parameters:

  Globals:

  Files:

Outputs
  Parameters:

  Globals:

  Files:

Return Values:

Calls:

Called By:

Author:  Jay Cable

Creation Date:  ???

Revision History:
Initials   Date      Description
--------   --------  -----------------------------------------------------
mmoore     07/11/02  Added comments and renamed some variables for clarity
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

//#include <stdio.h>
//#include <stdlib.h>
//#include <string.h>
//#include <sys/times.h>
//#include <time.h>
#include <ctype.h>
#include "util.h"
#include "metadata.h"

#define INFO (0)

/* Local Prototypes */
static int read_metadata_item (FILE * metadata, char *token, char *value);
static double toDeg (double theta);
static double toRad (double theta);

void string_trim (char *str);
void read_metadata_string (FILE * metadata, char *item, char *value);
double incidence2look (double inc, double Re, double H);
double sigma0_ASP (double dn, double range, double *noise_array, double a1,
                   double a2, double a3, int number_of_pixels);
double sigma0_PP (double dn, double range, double *noise_array, double a1,
                  double a2, double a3);
double sigma0_low_PP (double dn, double range, double *noise_array, double a1,
                      double a2, double a3);
double compute_ASF_sigma0 (double dn, double range, double *noise_array,
                           double a1, double a2, double a3,
                           int interval_size);
double ground2slant_range (double r /*** ground range ***/ ,
                           double Re /*** height above platform ***/ ,
                           double H /*** Radius of Earth ***/ );
double slant2ground (double sl, double Re, double H);


/* not used
void string_trim (char *str)
{
while (*str != 0 && isspace (*str))
   str++;
 *str = 0;
}
 */


/*+ Trim all spaces from end of a string +*/

void string_all_trim (char *str)
{
   char *ss;
   int start = 0, end = 0;
   /*printf ("String_all_trim (\"%s\")\n", str); */
   
   ss = (char *) malloc (sizeof (char) * strlen (str) + 2);
   
   while ((str[start] != 0) && (isspace (str[start]) != 0))
      start++;
   
   if (str[start] == 0)		/* No spaces found at all */
   {
      free (ss);
      return;
   }
   
   end = strlen (str) - 1;	/* Spaces in the begining found */
   
   while (end > start && isspace (str[end]) != 0)
      end--;
   
   if (end == start)
   {
      if (str[end] == 0)
      {
         /*printf ("Sring_all_trim:Nothing but space.\n", start, end); */
         str[0] = 0;
         free (ss);
         return;
      }
      return;
   }
   else
   {
      /*printf ("Sring_all_trim:strpping from %d to %d\n", start, end); */
      strcpy (ss, &(str[start]));
      ss[end - start + 1] = 0;
      strcpy (str, ss);
      free (ss);
      return;
   }
}


/*+++++++++++++
Report error information to stderr and exit with status -1

int ERROR      Returns -1 if status != EXIT

char *caller   Name of function where error occurred

char *str      Error string defined by caller

int status     Exit with -1 if status == EXIT
 +++++++++++++*/

int ERROR (char *caller, char *str, int status)
{
   fprintf (stderr, "ERROR (%s):\t%s (%d:'%s')\n", caller, str, errno,
   strerror (errno));
   fflush (NULL);
   if (status == EXIT)
      exit (-1);
   
   return -1;
}


/*+++++++++++++
Check if we have a valid file descriptor from open()

void file_check   IF the fd < 1 then ERROR() is called with status EXIT

int fd            File descriptor from open()

char *name        Name of file to check
 +++++++++++++*/

void file_check (int fd, char *name)
{
   char str[256];
   if (fd > 0)
      return;
   sprintf (str, "Could not open \"%s\"", name);
   ERROR ("file_check", str, EXIT);
}


/*+++++++++++++
Check if we have a valid file pointer from fopen()

void FILE_check   IF the fp == NULL then ERROR() is called with status EXIT

FILE * fp         File pointer from fopen()

char *name        Name of file to check

char *caller      Name of function where error occurred
 +++++++++++++*/

void FILE_check (FILE * fp, char *name, char *caller)
{
   char str[256];
   if (fp != NULL)
      return;
   sprintf (str, "(%s) Could not open \"%s\"", caller, name);
   ERROR ("file_check", str, EXIT);
}



#undef CALLER
#define CALLER "read_metadata_int"

/*+ Read an integer record of Metadata +*/

void read_metadata_int (FILE * metadata, char *item, int *value)
{
   char token[1024], v[1024];
   char message[256];
   rewind (metadata);
   
   if (metadata == NULL)
   {
      ERROR (CALLER, "Could not open metadata file.", EXIT);
   }
   while (feof (metadata) == 0)
   {
      if (read_metadata_item (metadata, token, v) == OK)
      if (strcmp (token, item) == 0)
      {
         /* Strip off ending '"' */
         v[strlen (v) - 1] = 0;
         sscanf (&(v[1]), "%d", value);
         return;
      }
   }
   
   sprintf (message, "Could not read metadata item \"%s\".", item);
   ERROR (CALLER, message, EXIT);
}

/* not used
#undef CALLER
#define CALLER "read_metadata_string"
void read_metadata_string (FILE * metadata, char *item, char *value)
{
char token[1024], v[1024];
char message[256];
rewind (metadata);

if (metadata == NULL)
   {
ERROR (CALLER, "Could not open metadata file.", EXIT);
}
while (feof (metadata) == 0)
   {
if (read_metadata_item (metadata, token, v) == OK)
if (strcmp (token, item) == 0)
   {
 *** Strip off ending '"' ***
v[strlen (v) - 1] = 0;
strcpy (value, &(v[1]));
return;
}
}

sprintf (message, "Could not read metadata item \"%s\".", item);
ERROR (CALLER, message, EXIT);
}
 */

#undef CALLER
#define CALLER "read_metadata_double"

/*+ Read an double record of Metadata +*/

void read_metadata_double (FILE * metadata, char *item, double *value)
{
   char token[1024], v[1024];
   char message[256];
   rewind (metadata);
   
   if (metadata == NULL)
   {
      ERROR (CALLER, "Could not open metadata file.", EXIT);
   }
   while (feof (metadata) == 0)
   {
      if (read_metadata_item (metadata, token, v) == OK)
      if (strcmp (token, item) == 0)
      {
         /* Strip off ending '"' */
         v[strlen (v) - 1] = 0;
         sscanf (&(v[1]), "%lg", value);
         return;
      }
   }
   
   sprintf (message, "Could not read metadata item \"%s\".", item);
   ERROR (CALLER, message, EXIT);
}


#undef CALLER
#define CALLER "read_metadata_item"

/*+ Read a string record of Metadata as a token value pair +*/

static int read_metadata_item (FILE * metadata, char *token, char *value)
{
   char buffer[1024];
   int a = 0, b;
   int len;
   
   if (fgets (buffer, 1023, metadata) == NULL)
   ERROR (CALLER, "A error was encountered while reading the metadata.",
   EXIT);
   
   /* Save string length */
   len = strlen (buffer);
   
   while (isspace (buffer[a]))
      a++;
   
   if (buffer[a] == '#')
      return COMMENT_METADATA;
   
   for (b = 0; a < len && buffer[a] != '='; a++, b++)
      token[b] = buffer[a];
   token[b] = 0;
   
   /* Eat up '=' */
   a++;
   for (b = 0; a < len; a++, b++)
      value[b] = buffer[a];
   
   string_all_trim (value);
   string_all_trim (token);
   /*printf ("Read \"%s\" = \"%s\"\n", token, value); */
   return OK;
}

#undef CALLER
#define CALLER "read_integer"
void read_integer (FILE * in, unsigned int *buffer, int start, int end)
{
   int ret;
   char msg[256];
   int fn;
   int i;
   
   fn = fileno (in);
   ret = lseek (fn, start * 4, SEEK_SET);
   if (ret != start * 4)
   {
      sprintf (msg,
         "Error reading input file, lseek returned %d, expected %d.",
      ret, start * 4);
      ERROR (CALLER, msg, EXIT);
   }
   
   ret = read (fn, (void *) buffer, (end - start) * 4);
   if (ret != (end - start) * 4)
   {
      sprintf (msg,
         "Error reading input file, read returned %d, expected %d.",
      ret, (end - start) * 4);
      ERROR (CALLER, msg, EXIT);
   }
   
   for (i = 0; i < end - start; i++)
      buffer[i] = (unsigned int) ntohl (buffer[i]);
}

#undef CALLER
#define CALLER "read_float"
void read_float (FILE * in, float *buffer, int start, int end)
{
   int ret;
   char msg[256];
   int fn;
   
   fn = fileno (in);
   ret = lseek (fn, start * sizeof (float), SEEK_SET);
   if (ret != start * sizeof (float))
   {
      sprintf (msg,
         "Error reading input file, lseek returned %d, expected %d.",
      ret, start * sizeof (float));
      ERROR (CALLER, msg, EXIT);
   }
   
   ret = read (fn, buffer, (end - start) * sizeof (float));
   if (ret != (end - start) * sizeof (float))
   {
      sprintf (msg,
         "Error reading input file, read returned %d, expected %d.",
      ret, (end - start) * sizeof (float));
      ERROR (CALLER, msg, EXIT);
   }
   /***
   for (i = 0; i < end - start; i++)
   if (buffer[i] > .5 || buffer[i] < .05)
      printf ("{sigma0[%d] = %g}\n", i, (double) (buffer[i]));
    ***/
}

#undef CALLER
#define CALLER "write_integer"
void write_integer (int out, unsigned int *buffer, int number)
{
   int ret;
   char msg[256];
   int i;
   
   for (i = 0; i < number; i++)
      buffer[i] = htonl (buffer[i]);
   
   ret = write (out, (void *) buffer, number * sizeof (int));
   if (ret != number * sizeof (int))
   {
      sprintf (msg,
         "Error reading writing output file, read returned %d, expected %d.",
      ret, number * sizeof (int));
      ERROR (CALLER, msg, EXIT);
   }
}

#undef CALLER
#define CALLER "read_mask"
void read_mask (FILE * in, unsigned char *buffer, int start, int end, int ns)
{
   int ret;
   char msg[256];
   int bstart;
   int sz;
   int fn;
   int ln_count;
   
   ln_count = ns / 8;
   if (ns % 8 != 0)
      ln_count++;
   
   bstart = (start / ns) * ln_count + start % ns;
   sz = (end - start) / 8;
   
   if (((end - start) % 8) != 0)
      sz++;
   
   fn = fileno (in);
   ret = lseek (fn, bstart, SEEK_SET);
   if (ret != bstart)
   {
      sprintf (msg,
         "Error reading mask file, lseek returned %d, expected %d.",
      ret, bstart);
      ERROR (CALLER, msg, EXIT);
   }
   
   ret = read (fn, (void *) buffer, sz);
   if (ret != sz)
   {
      sprintf (msg,
         "Error reading mask file, read returned %d, expected %d.",
      ret, sz);
      ERROR (CALLER, msg, EXIT);
   }
}


double stdev (double sum, double sum2, double n)
{
   return sqrt ((sum2 - (sum * sum / n)) / (n - 1.0));
}


double sigma0_low_PP (double dn, double range, double *noise_array, double a1,
                      double a2, double a3)
{
    return compute_ASF_sigma0 (dn, range, noise_array, a1, a2, a3, 4.0);
}

double sigma0_PP (double dn, double range, double *noise_array, double a1,
                  double a2, double a3)
{
    return compute_ASF_sigma0 (dn, range, noise_array, a1, a2, a3, 32);
}

double sigma0_ASP (double dn, double range, double *noise_array, double a1,
                   double a2, double a3, int number_of_pixels)
{
 return compute_ASF_sigma0 (dn, range, noise_array, a1, a2, a3,
                            number_of_pixels / 256);
}

double compute_ASF_sigma0 (double dn, double range, double *noise_array,
                           double a1, double a2, double a3, int interval_size)
{
    int lowend;
    double npower;
    double ret;


    lowend = (int) (range / interval_size);

     /*** This part is not intuitive. Since we are
    using lowend, and lowend + 1, lowend must be less
    than 255, as there are only 255 elements. If all
    well, this code should never be executed. ***/
    if (lowend >= 255) lowend = 254;

    npower = noise_array[lowend] * (1 - (range / interval_size - lowend))
             + noise_array[lowend + 1] * (range / interval_size - lowend);

     /*** Equation: 10 * log{a2 * [d^2 - (a1 * n(r))] + a3} ***/

     /***
    printf ("{ noise = %g ", npower);
    printf ("power = %g ", dn * dn);
    printf ("a = (%g, %g, %g)", a1, a2, a3);
    printf ("index = (%d,%d)", lowend, lowend + 1);
    printf ("sigma = %g } \n", (a2 * (dn * dn - a1 * npower) + a3));
     ***/

    ret = ((a2 * (dn * dn - a1 * npower) + a3));


     /***
    if (ret > .5 || ret < .005)
       {
    printf ("{ noise = %g ", npower);
    printf ("power = %g ", dn * dn);
    printf ("a = (%g, %g, %g)", a1, a2, a3);
    printf ("index = (%d,%d)", lowend, lowend + 1);
    printf ("sigma = %g } \n", (a2 * (dn * dn - a1 * npower) + a3));
    }

     ***/

    return ret;
}

double ground2slant_range (double r /*** ground range ***/ ,
			   double Re /*** height above platform ***/ ,
			   double H /*** Radius of Earth ***/ )
{
    double p;
    p = r / Re;
    if (INFO)
       printf ("INFO:\tground2slant_range(r=%f, Re=%f, H=%f)\n", r, Re, H);
    return (sqrt (2.0 * Re * (Re + H) * (1.0 - cos (p)) + H * H));
}

/*+ Calculate the look angle from the slant range, height and Earth Radius +*/

double slant2look (double R, /* slant range */
   double Re, /* height */
double H)	/* Radus of earth at nadir */
{
   
   /* (R2 + 2HRe + H2) / {2R(Re + H)} */
   
   return toDeg (acos ((R * R + 2.0 * H * Re + H * H) / (2.0 * R * (Re + H))));
}

double look2slant (double look, double Re, double H)
{
   /*** R^2 = 2Re(Re + H)(1 - cos P ) + H2  ***/
   double P, R2;
   P = look2incidence (look, Re, H) - look;
   R2 = 2.0 * Re * (Re + H) * (1.0 - cos (toRad (P))) + H * H;
   return (sqrt (R2));
   
}


double sigma02gama0 (double inc, double sigma0)
{
   
   return (sigma0 - 10.0 * log10 (cos (toRad (inc))));
   
}


/*+ Convert Radians to Degrees +*/

static double toDeg (double theta)
{
   return ((theta / M_PI) * 180.0);
}

/*+ Convert Degrees to Radians +*/

static double toRad (double theta)
{
   return ((theta / 180.0) * M_PI);
}

/*+ Convert Look angle to Incidence angle +*/

double look2incidence (double look, double Re, double H)
{
   return toDeg ((asin ((Re + H) * sin (toRad (look)) / Re)));
}

double incidence2look (double inc, double Re, double H)
{
    return toDeg (asin ((sin (toRad (inc)) / (Re + H)) * Re));
}

/* not used 
double P_2_slant( double P)
{
    double R2;
    R2 = 2.0*Re*(Re+H)*(1-cosd(P))+H*H;
    return sqrt(R2);
}
*/

/* not used 
double look_2_slant ( double look)
{
     double P;
     if (state == 0) geo_error(ERROR_STATE);
     P = look_2_P ( look);
     return P_2_slant(P);
}
*/

/* not used 
double incidence_2_slant (double incidence )
{
    double P, R2;
    if (state == 0) geo_error(ERROR_STATE);
    P = incidence_2_P (incidence );
    return P_2_slant(P);
}
*/

/* not used 
double look_2_P ( double look)
{
    return look_2_incidence(look) - look;
}
*/
/* not used 
double incidence_2_P (double incidence )
{
    return incidence-incidence_2_look(incidence);
}
*/

double slant2ground (double sl, double Re, double H)
{
    double ReH;
    if (INFO)
       printf ("INFO:\t slant2ground(sl=%f, Re=%f, H=%f)\n", sl, Re, H);
    ReH = Re + H;
    return ((Re * acos ((Re * Re + ReH * ReH - sl * sl) / (2.0 * Re * ReH))));
}
