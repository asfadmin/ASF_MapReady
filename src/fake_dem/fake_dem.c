#define ASF_NAME_STRING "fake_dem"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <type> <size> <in_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program creates an image with the same geometry as the input\n"\
"     but with fake data as specified.\n"\
"\n"\
"     Valid types are:\n"\
"           sinusoid-horizontal\n"\
"           sinusoid-vertical\n"\
"           sinusoid-diagonal\n"\
"           triangle-horizontal\n"\
"           triangle-vertical\n"\
"           triangle-diagonal\n"\
"\n"\
"     The size parameter specifies the period, in pixels, of the sinusoid or\n"\
"     triangle wave.\n"\
"\n"\
"     For example:\n"\
"        fake_dem sinusoid-horizontal 200 delta_fixed my_fake_dem\n"\
"\n"


#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <math.h>

// Print minimalistic usage info & exit
static void usage(const char *name)
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
      "Contact:\n" ASF_CONTACT_STRING "\n"
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

static void fake_it(const char *inFile, const char *outFile,
                    const char *type, double size);

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *outFile;
  int currArg = 1;
  int NUM_ARGS = 4;
  double size;
  char *type;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
      usage(ASF_NAME_STRING);
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
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  type = argv[currArg];
  size = atof(argv[currArg+1]);
  inFile = argv[currArg+2];
  outFile = argv[currArg+3];
  
  char *in_base = get_basename(inFile);
  char *out_base = get_basename(outFile);

  fake_it(inFile, outFile, type, size);
  asfPrintStatus("Done.\n");

  free(in_base);
  free(out_base);

  return EXIT_SUCCESS;
}

static double triangle(double t, double a)
{
  return fabs(2.*(t/a - floor(t/a+.5)));
}

static double sin2(double t, double a)
{
  double d = sin(t*PI/a);
  return d*d;
}

static float calc_val(int ii, int nl, int jj, int ns, const char *type,
                      double size)
{
  if (strcmp_case(type, "sinusoid-horizontal")==0) {
    return sin2(jj,size);
  }
  else if (strcmp_case(type, "sinusoid-vertical")==0) {
    return sin2(ii,size);
  }
  else if (strcmp_case(type, "sinusoid-diagonal")==0) {
    return sin2(.707*(ii+jj),size);
  }
  else if (strcmp_case(type, "triangle-horizontal")==0) {
    return triangle(jj,size);
  }
  else if (strcmp_case(type, "triangle-vertical")==0) {
    return triangle(ii,size);
  }
  else if (strcmp_case(type, "triangle-diagonal")==0) {
    return triangle(.707*(ii+jj),size);
  }
  asfPrintStatus("Valid types:\n"
                 "  sinusoid-horizontal\n"
                 "  sinusoid-vertical\n"
                 "  sinusoid-diagonal\n"
                 "  triangle-horizontal\n"
                 "  triangle-vertical\n"
                 "  triangle-diagonal\n");
  asfPrintError("Impossible: %s\n", type);
  return 0;
}

static void fake_it(const char *inFile, const char *outFile,
                    const char *type, double size)
{
  char *metaFile = appendExt(inFile, ".meta");
  meta_parameters *meta = meta_read(inFile);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int ii, jj;

  char *outImg = appendExt(outFile, ".img");
  char *outMeta = appendExt(outFile, ".meta");
  float *buf = MALLOC(sizeof(float)*ns);

  FILE *fp = FOPEN(outImg, "wb");

  for (ii=0; ii<nl; ++ii) {
    for (jj=0; jj<ns; ++jj) {
      buf[jj] = 500 + 300*calc_val(ii, nl, jj, ns, type, size); 
    }
    put_float_line(fp, meta, ii, buf);
    asfLineMeter(ii,nl);
  }
  FCLOSE(fp);

  meta_write(meta, outMeta);

  FREE(buf);
  FREE(metaFile);
  meta_free(meta);

  FREE(outImg);
  FREE(outMeta);
}

