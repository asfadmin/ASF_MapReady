#include <stdlib.h>
#include <stdio.h>
#include "../util/util.h"
#include "../util/metadata.h"
#include "asf.h"
#include "asf_endian.h"

#define IMAGE_NUMBER  	(currArg)
#define ON_NUMBER 	(IMAGE_NUMBER + 1)
#define MASK_NUMBER 	(ON_NUMBER + 1)
#define OUTFILE_NUMBER 	(MASK_NUMBER + 1)

#define PRINT_SPACING (" ")

#undef VERSION
#define VERSION 0.5

/* <in>.metadata feilds */
static double slt_rng_1st_pixel;
static double pixel_spacing_rng;
static double Re, H;
static int number_of_pixels, number_of_lines;

/* Function prototypes */
static void print_headers (FILE * out, FILE * excel_out, char *name, char *mask);
void usage (char *name);

#undef CALLER
#define CALLER "xy_plotter:Main"

int main (int argc, char **argv)
{
   char metadataName[PATH_MAX];
   char imageName[PATH_MAX], lookName[PATH_MAX], sigmaName[PATH_MAX];
   char maskName[PATH_MAX];
   char outfileName[PATH_MAX], excelOutfileName[PATH_MAX];
   FILE *metadataFile, *sigmaFile, *imageFile, *lookFile;
   FILE *outFile, *inMaskFile, *excelOutFile;
   float *imageBuf, *sigmaBuf, *lookBuf;
   unsigned char *maskBuf;
   double *sigma_counter, *dn_counter, *dn2_counter;
   unsigned int *counter;
   int line, pixel;
   int start, stop;
   int mi;
   int ON;
   double t;

/* Parse commandline */
   while (currArg < (argc-4)) {
      char *key = argv[currArg++];
      if (strmatch(key,"-bin")) {
      /* CHECK_ARG(1);
         num_bins = atod(GET_ARG(1));
      */
         printf("The -bin option has not yet been implemented");
	 usage(argv[0]);
      }
      else if (strmatch(key,"-x")) {
         printf("The -x option has not yet been implemented");
	 usage(argv[0]);
      }
      else if (strmatch(key,"-y")) {
         printf("The -y option has not yet been implemented");
	 usage(argv[0]);
      }
      else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
   }
   if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}

/* Formulate file names */
   create_name (metadataName,     argv[IMAGE_NUMBER],   METADATA_EXT);
   create_name (imageName,        argv[IMAGE_NUMBER],   DATA_EXT);
   create_name (lookName,         argv[IMAGE_NUMBER],   LOOK_EXT);
   create_name (sigmaName,        argv[IMAGE_NUMBER],   SIGMA_EXT);
   create_name (excelOutfileName, argv[OUTFILE_NUMBER], ".cdf");
   strcpy (maskName,    argv[MASK_NUMBER]);
   strcpy (outfileName, argv[OUTFILE_NUMBER]);

/* Open files */
   metadataFile = FOPEN (metadataName, "r");
   imageFile    = FOPEN (imageName,    "r");
   lookFile     = FOPEN (lookName,     "r");
   sigmaFile    = FOPEN (sigmaName,    "r");
   inMaskFile   = FOPEN (maskName,     "r");
   outFile      = FOPEN (outfileName,      "w");
   excelOutFile = FOPEN (excelOutfileName, "w");

/* This value depends on how the mask is interpeted - 
 * see the for loop bellow. It is 0x80 because the bit
 * representing the current pixel is shifted left and 
 * compared with 0x80.  -JC
 */
   if (strcmp ("off", argv[ON_NUMBER]))
      ON = 0;
   else
      ON = 0x80;

/* Get necessary metadata values */              
   read_metadata_int (metadataFile, NUMBER_OF_PIXELS, &number_of_pixels);
   read_metadata_int (metadataFile, NUMBER_OF_LINES, &number_of_lines);
   read_metadata_double (metadataFile, SLANT_RANGE_TO_FIRST_PIXEL,
                         &slt_rng_1st_pixel);
   read_metadata_double (metadataFile, RNG_PIXEL_SPACING, &pixel_spacing_rng);
   read_metadata_double (metadataFile, EARTH_RADIUS_AT_IMAGE_NARIR, &Re);
   read_metadata_double (metadataFile, PLATFORM_ALITITUDE, &H);

/* Allocate buffers */
   imageBuf = (float *)MALLOC(sizeof (float) * number_of_pixels);
   lookBuf  = (float *)MALLOC(sizeof (float) * number_of_pixels);
   sigmaBuf = (float *)MALLOC(sizeof (float) * number_of_pixels);
   maskBuf = (unsigned char *)MALLOC(sizeof (unsigned char) * number_of_pixels);

   sigma_counter = (double *)MALLOC(sizeof (double) * number_of_pixels);
   dn_counter    = (double *)MALLOC(sizeof (double) * number_of_pixels);
   dn2_counter   = (double *)MALLOC(sizeof (double) * number_of_pixels);
   counter = (unsigned int *) MALLOC(sizeof (unsigned int) * number_of_pixels);

   start = 0;
   stop = number_of_pixels;

   read_float (lookFile, lookBuf, start, stop);
   for (pixel=0; pixel<number_of_pixels; pixel++) {
      ieee_big32(lookBuf[pixel]); /* Set bytes in correct order */
   }

   for (pixel=0; pixel<number_of_pixels; pixel++) {
      dn_counter[pixel] = 0;
      dn2_counter[pixel] = 0;
      sigma_counter[pixel] = 0;
      counter[pixel] = 0;
   }

   printf ("\nPercent completed:    ");
   for (line=0; line<number_of_lines; line++) {

      start = line * number_of_pixels;
      stop  = start + number_of_pixels;

      read_float (imageFile, imageBuf, start, stop);
      read_float (sigmaFile, sigmaBuf, start, stop);
      read_mask (inMaskFile, maskBuf, start, stop, number_of_pixels);

      for (pixel=0; pixel<number_of_pixels; pixel++) {

         /* Set data bytes in correct order */
         ieee_big32(imageBuf[pixel]);
         ieee_big32(sigmaBuf[pixel]);

	 mi = pixel / 8;

/*         if (pixel % 8 != 0)  { mi++; }
 */

/*         if (maskBuf[mi] != 0 ) {
 *            printf("Something should be found..\n");
 *            printf("Mask = %x, offset = %d (%x), imageBuf = %d (%x) -> %x\n", 
 *                    maskBuf[mi],
 *                    pixel%8,
 *                    pixel%8,
 *                    imageBuf[pixel],
 *                    ((maskBuf[mi] << (pixel % 8)) & 0x80));
 *         }
 */
	 if ((((maskBuf[mi] << (pixel % 8)) & 0x80) == ON)
	      && (imageBuf[pixel] != 0 && sigmaBuf[pixel] > 1E-9)) {
	    counter[pixel]++;
	    sigma_counter[pixel] += (double) sigmaBuf[pixel];
	    t = (double) imageBuf[pixel];
	    dn_counter[pixel] += t * t;
	    dn2_counter[pixel] += t * t * t * t;
	 }
      }

      if (line % 8 == 0) {
	 printf ("\b\b\b%3d", (100*line)/number_of_lines);
	 fflush (stdout);
      }
   }
/* Free memory we're finished with */
   FREE(imageBuf);
   FREE(lookBuf);
   FREE(sigmaBuf);
   FREE(maskBuf);

   printf ("\rPercent completed: 100\n");

/* Write the output */
   print_headers (outFile, excelOutFile, argv[1], maskName);
   for (pixel=0; pixel<number_of_pixels; pixel++) {
      double t_power, t_stdpower, t_sigma0, t_look, t_sltrng, t_gama0;

      if (counter[pixel] > 3) {
	 t_power    = dn_counter[pixel] / (double) counter[pixel];
	 t_stdpower = stdev (dn_counter[pixel], dn2_counter[pixel],
	                     (double) counter[pixel]);
	 t_sltrng = look2slant (lookBuf[pixel], Re, H);
	 t_look   = lookBuf[pixel];
	 t_sigma0 = 10.0 * log10 (sigma_counter[pixel]/(double)counter[pixel]);
	 t_gama0  = sigma02gama0 (look2incidence (t_look, Re, H), t_sigma0);

	 fprintf (outFile, "%12d%s", pixel, PRINT_SPACING);
	 fprintf (outFile, "%12d%s", (counter[pixel]), PRINT_SPACING);
	 fprintf (outFile, "%12f%s", t_sltrng, PRINT_SPACING);
	 fprintf (outFile, "%12f%s", t_look, PRINT_SPACING);
	 fprintf (outFile, "%12e%s", dn_counter[pixel], PRINT_SPACING);
	 fprintf (outFile, "%12e%s", dn2_counter[pixel], PRINT_SPACING);
	 fprintf (outFile, "%12e%s", t_power, PRINT_SPACING);
	 fprintf (outFile, "%12e%s", t_stdpower, PRINT_SPACING);
	 fprintf (outFile, "%12f%s", t_sigma0, PRINT_SPACING);
	 fprintf (outFile, "%12f%s\n", t_gama0, PRINT_SPACING);

	 fprintf (excelOutFile, "%12d,", pixel);
	 fprintf (excelOutFile, "%12d,", (counter[pixel]));
	 fprintf (excelOutFile, "%12f,", t_sltrng);
	 fprintf (excelOutFile, "%12f,", t_look);
	 fprintf (excelOutFile, "%12e,", dn_counter[pixel]);
	 fprintf (excelOutFile, "%12e,", dn2_counter[pixel]);
	 fprintf (excelOutFile, "%12e,", t_power);
	 fprintf (excelOutFile, "%12e,", t_stdpower);
	 fprintf (excelOutFile, "%12f,", t_sigma0);
	 fprintf (excelOutFile, "%12f%s\n", t_gama0, PRINT_SPACING);
      }
   }

/* Free the rest of the buffers */
   FREE(sigma_counter);
   FREE(dn_counter);
   FREE(dn2_counter);
   FREE(counter);

/* Close all the files */
   FCLOSE (imageFile);
   FCLOSE (inMaskFile);
   FCLOSE (lookFile);
   FCLOSE (sigmaFile);
   FCLOSE (metadataFile);
   FCLOSE (outFile);
   FCLOSE (excelOutFile);

   StopWatch();

   return 0;
}


/******************************************************************************/
#define HEADER_PIECE "%12s "
#define EX_HEADER_PIECE "%12s,"
static void print_headers(FILE *out,FILE *excel_out,char *name,char *maskName)
{

  /* Print metadata for excel data */
  fprintf (out, "\t\tDISTRIBUTED TARGET ANALYSIS\n");
  fprintf (out, "\t%-40s :\t \"%s\"\n", "Image File", name);
  fprintf (out, "\t%-40s :\t \"%s\"\n", "Mask File", maskName);
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
  fprintf (excel_out, "\t%-40s , \"%s\"\n", "Mask File", maskName);
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
  fprintf (out, HEADER_PIECE, "       Pixel");
  fprintf (out, HEADER_PIECE, " # Of Values");
  fprintf (out, HEADER_PIECE, "  SltRng(km)");
  fprintf (out, HEADER_PIECE, "   LookAg(D)");
  fprintf (out, HEADER_PIECE, "  Sum of Pwr");
  fprintf (out, HEADER_PIECE, "   Sum^2 Pwr");
  fprintf (out, HEADER_PIECE, "    AvgPower");
  fprintf (out, HEADER_PIECE, "    StDevPwr");
  fprintf (out, HEADER_PIECE, "  Sigma0(db)");
  fprintf (out, HEADER_PIECE, "  Gamma0(db)");
  fprintf (out, "\n");

  /* Print header for excel formatted data */
  fprintf (excel_out, EX_HEADER_PIECE, "       Pixel");
  fprintf (excel_out, EX_HEADER_PIECE, " # Of Values");
  fprintf (excel_out, EX_HEADER_PIECE, "  SltRng(km)");
  fprintf (excel_out, EX_HEADER_PIECE, "   LookAg(D)");
  fprintf (excel_out, EX_HEADER_PIECE, "  Sum of Pwr");
  fprintf (excel_out, EX_HEADER_PIECE, "   Sum^2 Pwr");
  fprintf (excel_out, EX_HEADER_PIECE, "    AvgPower");
  fprintf (excel_out, EX_HEADER_PIECE, "    StDevPwr");
  fprintf (excel_out, EX_HEADER_PIECE, "  Sigma0(db)");
  fprintf (excel_out, EX_HEADER_PIECE, "  Gamma0(db)");
  fprintf (excel_out, "\n");
}


/* Print the usage ************************************************************/
void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
/*
	"   %s [-x <X>] [-y <Y>] [-bin <num_bins>]\n"
	"              <imageFile> <on|off> <maskFile> <outFile>\n", name);
*/
	"   %s <imageFile> <on|off> <maskFile> <outFile>\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   imageFile  Name of the image file.\n"
	"   on|off     If 'off', the analysis is preformed on the areas not masked,\n"
	"                otherwise the analysis is preformed on the masked areas.\n"
	"   maskFile   Name of the file containing the mask information.\n"
	"   outFile    Analysis file to be written.\n");
/*
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -x    <X> can be: incidence, look, slant, ground, or pixel.\n"
	"            Defaults to incidence.
	"   -y    <Y> can be: dn, power, sigma0, or gamma0. Defaults to dn.\n"
	"   -bin  <bin> is the number of bins to use in the histogram.\n"
*/
 printf("\n"
	"DESCRIPTION:\n"
	"   This program generates the radiometric backscatter information necessary to\n"
	"   calibrate the processor's antenna pattern.  The image lines are averaged to an\n"
	"   array, as a function of range, that includes: pixel number, number of values,\n"
	"   slant range (km), look angle (degrees), sum of power, sum^2 of power, average\n"
	"   power, standard deviation of the power, Sigma0(db), and Gamma0(db).\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit (EXIT_FAILURE);
}

