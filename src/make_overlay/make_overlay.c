#define ASF_NAME_STRING "make_overlay"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <input file> <output file>\n"

#define ASF_DESCRIPTION_STRING \
"     This program creates an overlay image.  It is intended for internal\n"\
"     use by ASF during production processing.\n"\
"\n"\
"     The input image should have a band named AMP, and another named\n"\
"     INTERFEROGRAM_PHASE.  Output is a geotiff file with the AMP\n"\
"     overlayed on the interferogram, which has the interferogram look\n"\
"     up table applied.\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

// Print minimalistic usage info & exit
static void make_overlay_usage(const char *name)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  exit(EXIT_FAILURE);
}

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_SUCCESS);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

static unsigned char map255(float val, float orig_val)
{
  if (orig_val == 0)
    return 0;
  else if (val < 0)
    return 0;
  else if (val >= 254.5)
    return 255;
  else
    return (unsigned char) (val+.5); 
}

static void get_band_stats(FILE *inFp, meta_parameters *meta, int nl, int ns,
                           int band_num, float *buf, float *min, float *max)
{
  int ii, jj;
  for (ii=0; ii<nl; ++ii) 
    get_band_float_line(inFp, meta, band_num, ii, buf+ii*ns);

  float avg=0;
  for (jj=0; jj<ns*nl; ++jj)
    avg += buf[jj];
  avg /= (float)(nl*ns);

  float stddev=0;
  for (jj=0; jj<ns*nl; ++jj)
    stddev += (buf[jj] - avg) * (buf[jj] - avg);
  stddev = sqrt(stddev / (float)(nl*ns));

  *min = avg - 2*stddev;
  *max = avg + 2*stddev;
}

// work around a hard-coded function call in CHECK_ARG macro
#define usage make_overlay_usage

// Main program body.
int
main (int argc, char *argv[])
{
  int currArg = 1;
  int NUM_ARGS = 2;

  // FIXME: turn these into command line options
  char *greyscale_band_name = "AMP";
  char *rgb_band_name = "INTERFEROGRAM_PHASE";
  char *lut_name = "interferogram.lut";

  char **band_names = MALLOC(sizeof(char*)*2);
  band_names[0] = "OVERLAY";
  band_names[1] = NULL;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<3)
    make_overlay_usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
    print_help();

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else {
      --currArg;
      break;
    }
  }

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.  Expected %d, got %d.\n",
           NUM_ARGS, argc-currArg);
    make_overlay_usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    make_overlay_usage(argv[0]);
  }

  char *inputImg = appendExt(argv[currArg], ".img");
  char *inputMeta = appendExt(argv[currArg], ".meta");
  char *outputImg = appendExt(argv[currArg+1], ".img");
  char *outputMeta = appendExt(argv[currArg+1], ".meta");

  if (!fileExists(inputImg))
    asfPrintError("Not found: %s\n", inputImg);
  if (!fileExists(inputMeta))
    asfPrintError("Not found: %s\n", inputMeta);

  meta_parameters *meta = meta_read(inputMeta);
  if (!meta) asfPrintError("Failed to read metadata: %s\n", inputMeta);

  meta_parameters *outMeta = meta_read(inputMeta);
  strcpy(outMeta->general->bands, "r,g,b");
  outMeta->general->band_count = 3;
  outMeta->general->image_data_type = IMAGE;
  //FREE(outMeta->insar);
  //outMeta->insar = NULL;

  char *band_str = meta->general->bands;
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  int nb = meta->general->band_count;
  int ii, jj;

  int amp_band_num = get_band_number(band_str, nb, greyscale_band_name);
  int phase_band_num = get_band_number(band_str, nb, rgb_band_name);

  if (amp_band_num<0)
    asfPrintError("Band '%s' not found.", greyscale_band_name);
  if (phase_band_num<0)
    asfPrintError("Band '%s' not found.", rgb_band_name);

  FILE *inFp = FOPEN(inputImg, "rb");
  float *buf = MALLOC(sizeof(float)*nl*ns);

  asfPrintStatus("Calculating stats...\n");
  float amp_min, amp_max, rgb_min, rgb_max;
  get_band_stats(inFp, meta, nl, ns, phase_band_num, buf, &rgb_min, &rgb_max);
  get_band_stats(inFp, meta, nl, ns, amp_band_num, buf, &amp_min, &amp_max);
  FREE(buf);

  float amp_range = amp_max - amp_min;
  float rgb_range = rgb_max - rgb_min;

  float *amp_buf = MALLOC(sizeof(float) * ns);
  float *phase_buf = MALLOC(sizeof(float) * ns);
  unsigned char *gs_buf = MALLOC(sizeof(unsigned char) * ns);
  unsigned char *rgb_buf = MALLOC(sizeof(unsigned char) * ns*3);
  float *red_buf = MALLOC(sizeof(float) * ns);
  float *grn_buf = MALLOC(sizeof(float) * ns);
  float *blu_buf = MALLOC(sizeof(float) * ns);

  asfPrintStatus("Writing output...\n");

  FILE *outFp = FOPEN(outputImg, "wb");

  //double K2 = 1.0 / sqrt(2.0);
  //double K3 = 1.0 / sqrt(3.0);
  //double K6 = 1.0 / sqrt(6.0);
  //double r2d = 180.0 / 3.141592654;
  //double d2r = 3.141592654 / 180.0;
  double wt = .4;

  for (ii=0; ii<nl; ++ii) {
    get_band_float_line(inFp, meta, amp_band_num, ii, amp_buf);
    get_band_float_line(inFp, meta, phase_band_num, ii, phase_buf);
    for (jj=0; jj<ns; ++jj)
      gs_buf[jj] = map255(255*(phase_buf[jj]-rgb_min)/rgb_range, phase_buf[jj]);
    apply_look_up_table_byte(lut_name, gs_buf, ns, rgb_buf);
    for (jj=0; jj<ns; ++jj)
      gs_buf[jj] = map255(255*(amp_buf[jj]-amp_min)/amp_range, amp_buf[jj]);

    // now for the overlay...
    for (jj=0; jj<ns; ++jj) {
      int kk = jj*3;

      double gs = gs_buf[jj];
      //gs = 255*sqr(gs/255.0);
      gs = gs*(1.0-wt);
      red_buf[jj] = rgb_buf[kk+0]*wt + gs;
      grn_buf[jj] = rgb_buf[kk+1]*wt + gs;
      blu_buf[jj] = rgb_buf[kk+2]*wt + gs;

/* ihs -> rgb and back calculation that we aren't using
      double R = rgb_buf[kk];
      double G = rgb_buf[kk+1];
      double B = rgb_buf[kk+2];
      if (R==0 && G==0 && B==0) {
        red_buf[jj] = 0;
        grn_buf[jj] = 0;
        blu_buf[jj] = 0;
      }
      else {
        double B1 = K6 * (2*B - R - G);
        double X1 = K2 * (G - R);
        double I = K3 * (R + G + B);
        double H,S;

        if (B1 == 0) {
          H = G<R ? 270 : 90;
        }
        else {
          H = r2d*atan(X1/B1);
          if (G>R && H<0) H += 180;
          if (G<R && H>0) H += 180;
          if (G==R && R>B) H=180;
        }

        if (H<0) H+=360;
        if (H >= 360) H-=360;

        I *= (255.0/442.0);
        H *= (255.0/360.0);
        S *= (255.0/208.2066); 

        I = gs_buf[jj];
        S = 255;

        double I1 = I*(442.0 / 255.0);
        double H1 = H*(360.0 / 255.0);
        double S1 = S*(208.2066 / 255.0);

        B1 = S1*cos(d2r*H1);
        X1 = S1*sin(d2r*H1);

        double Rn = (K3*I1) - (K6*B1) - (K2*X1);
        double Gn = (K3*I1) - (K6*B1) + (K2*X1);
        double Bn = (K3*I1) + (2*K6*B1);

        red_buf[jj] = map255(Rn,1);
        grn_buf[jj] = map255(Gn,1);
        blu_buf[jj] = map255(Bn,1);
      }
*/
    }
    put_band_float_line(outFp, outMeta, 0, ii, red_buf);
    put_band_float_line(outFp, outMeta, 1, ii, grn_buf);
    put_band_float_line(outFp, outMeta, 2, ii, blu_buf);

    asfLineMeter(ii,nl);
  }

  meta_write(outMeta, outputMeta);

  meta_free(outMeta);
  meta_free(meta);

  FCLOSE(outFp);
  FCLOSE(inFp);
  FREE(inputImg);
  FREE(inputMeta);
  FREE(outputImg);
  FREE(outputMeta);

  FREE(amp_buf);
  FREE(phase_buf);
  FREE(gs_buf);
  FREE(rgb_buf);
  FREE(red_buf);
  FREE(grn_buf);
  FREE(blu_buf);

  asfPrintStatus("Done.\n");
  return EXIT_SUCCESS;
}
