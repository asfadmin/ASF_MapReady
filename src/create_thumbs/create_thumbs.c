#define ASF_NAME_STRING "create_thumbs"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-verbose] [-size <size>]\n"\
"                 [-recursive] [-out-dir <dir>] <files>\n"

#define ASF_DESCRIPTION_STRING \
"     This program takes any number of CEOS files and generates\n"\
"     thumbnails.  If a directory is specified, all files in that\n"\
"     directory are processed.  If -R is specified, any subdirectories\n"\
"     are also processed, recursively.\n\n"\
"     The generated thumbnails have the same basename as the input\n"\
"     CEOS file, with '_thumb.jpg' added.\n"

#define ASF_INPUT_STRING \
"     At least one input file or directory is required.\n"  

#define ASF_OUTPUT_STRING \
"     The program will produce one thumbnail for each input file found.\n"

#define ASF_OPTIONS_STRING \
"     -size <size>\n"\
"          Generate thumbnails of the given size.  The default is %d.\n"\
"          If the input image isn't square, the longer side will be\n"\
"          scaled to the given size, the other dimension will be\n"\
"          determined so as to keep the same aspect ratio.\n"\
"\n"\
"     -recursive (-R, -r)\n"\
"          Recurse into subdirectories, looking for additional CEOS files\n"\
"          to generate thumbnails for.\n"\
"\n"\
"     -out-dir (-output-dir, -o)\n"\
"          Specify a directory where all thumbnails are placed.  Without\n"\
"          this option, all thumbnails are placed in the same directory as\n"\
"          the CEOS file.\n"\
"\n"\
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -quiet (-q)\n"\
"          Supresses all non-essential output.\n"\
"\n"\
"     -verbose (-v)\n"\
"          Prints out files that were ignored (i.e., not CEOS files).\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version (-v)\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"     Generate thumbnails for all files in the current directory:\n"\
"     > "ASF_NAME_STRING" .\n\n"\
"     Generate thumbnails for files in the directory n60s:\n"\
"     > "ASF_NAME_STRING" n60s\n\n"\
"     Generate thumbnails for all files in the current direcotry,\n"\
"     and all subdirectories.\n"\
"     > "ASF_NAME_STRING" -r .\n\n"\
"     Generate a large thumbnail for the single file file1.D:\n"\
"     > "ASF_NAME_STRING" -size 1024 file.D\n\n"

#define ASF_LIMITATIONS_STRING \
"     The output file naming convention is not user-customizable.\n"

#define ASF_SEE_ALSO_STRING \
"     asf_convert_gui\n"

#include <ceos.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

#ifdef linux
#include <unistd.h>
#endif

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "asf.h"
#include "get_ceos_names.h"
#include "asf_endian.h"
#include "asf_import.h"
#include "float_image.h"
#include "asf_nan.h"

int verbose = 0;
const int default_thumbnail_size = 256;
char *out_dir = NULL;

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
      "Input:\n" ASF_INPUT_STRING "\n"
      "Output:\n"ASF_OUTPUT_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n",
      default_thumbnail_size);
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

char *spaces(int n)
{
    int i;
    static char buf[256];
    for (i=0; i<256; ++i)
        buf[i] = i<n*3 ? ' ' : '\0';
    return buf;
}

int has_prepension(const char * data_file_name)
{
    /* at the moment, the only prepension we allow is IMG- (ALOS) */
    char *basename = get_basename(data_file_name);
    int ret = strncmp(basename, "LED-", 4) == 0;
    free(basename);
    return ret ? 4 : 0;
}

char *meta_file_name(const char * data_file_name)
{
    char *basename = get_basename(data_file_name);
    int is_alos = strncmp(basename, "IMG-", 4) == 0;
    free(basename);

    if (is_alos) {
        char *meta_name = MALLOC(sizeof(char) * (strlen(data_file_name) + 5));
        char *dir = get_dirname(data_file_name);
        strcpy(meta_name, dir);
        strcat(meta_name, "LED-");
        char *file = get_filename(data_file_name);
        char *p=strchr(file, '-');
        if (p) p = strchr(p+1, '-');
        if (p) strcat(meta_name, p+1);
        free(dir);
        free(file);
        return meta_name;
    }
    else {
        return appendExt(data_file_name, ".L");
    }
}

void process(const char *what, int top, int recursive, int size);

void process_dir(const char *dir, int top, int recursive, int size)
{
  char name[1024];
  struct dirent *dp;
  DIR *dfd;

  if ((dfd = opendir(dir)) == NULL) {
    asfPrintStatus("cannot open %s\n",dir);
    return; // error
  }
  while ((dp = readdir(dfd)) != NULL) {
    if (strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0) {
      continue;
    }
    if (strlen(dir)+strlen(dp->d_name)+2 > sizeof(name)) {
      asfPrintWarning("dirwalk: name %s/%s exceeds buffersize.\n",
                      dir, dp->d_name);
      return; // error
    }
    else {
      sprintf(name, "%s%c%s", dir, DIR_SEPARATOR, dp->d_name);
      process(name, top, recursive, size);
    }
  }
  closedir(dfd);
}

meta_parameters * silent_meta_create(const char *filename)
{
    report_level_t prev = g_report_level;

    g_report_level = NOREPORT;
    meta_parameters *ret = meta_create(filename);

    g_report_level = prev;
    return ret;
}

int generate_ceos_thumbnail(const char *input_data, int size)
{
    char *input_metadata = meta_file_name(input_data);

    /* This can happen if we don't get around to drawing the thumbnail
       until the file has already been processes & cleaned up, don't want
       to crash in that case. */
    if (!fileExists(input_metadata)) {
        asfPrintStatus("Metadata file not found: %s\n", input_metadata);
        return FALSE;
    }

    // Input metadata
    meta_parameters *imd;
    char *data_name, *met;

    int pre = has_prepension(input_metadata);
    if (pre > 0)
    {
        int ii, nBands;
        char **dataName = MALLOC(sizeof(char*)*MAX_BANDS);
        for (ii=0; ii<MAX_BANDS; ++ii)
            dataName[ii] = MALLOC(sizeof(char)*255);
        char filename[255], dirname[255];
        split_dir_and_file(input_metadata, dirname, filename);
        met = MALLOC(sizeof(char)*(strlen(input_metadata)+1));
        sprintf(met, "%s%s", dirname, filename + pre);

        get_ceos_data_name(met, dataName, &nBands);

        data_name = STRDUP(dataName[0]);
        imd = silent_meta_create(met);

        for (ii=0; ii<MAX_BANDS; ++ii)
            FREE(dataName[ii]);
        FREE(dataName);
    }
    else
    {
        imd = silent_meta_create (input_metadata);
        data_name = STRDUP(input_data);
        met = STRDUP(input_metadata);
    }

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16) 
// Turning off support for these guys for now.
//        imd->general->data_type != INTEGER32 &&
//        imd->general->data_type != REAL32 &&
//        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        asfPrintStatus("Unknown data type: %d\n", imd->general->data_type);
        return FALSE;
    }

    FILE *fpIn = fopen(data_name, "rb");
    if (!fpIn)
    {
        // failed for some reason, quit without thumbnailing
        meta_free(imd);
        asfPrintStatus("Failed to open: %s\n", data_name);
        return FALSE;
    }

    struct IOF_VFDR image_fdr;                /* CEOS File Descriptor Record */
    get_ifiledr(met, &image_fdr);
    int leftFill = image_fdr.lbrdrpxl;
    int rightFill = image_fdr.rbrdrpxl;
    int headerBytes = firstRecordLen(data_name) +
        (image_fdr.reclen - (imd->general->sample_count + leftFill + rightFill)
         * image_fdr.bytgroup);

    // use a larger dimension at first, for our crude scaling.  We will
    // use a better scaling method later, from GdbPixbuf
    int larger_dim = 1024;

    // Vertical and horizontal scale factors required to meet the
    // max_thumbnail_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Thumbnail image sizes.
    size_t tsx = imd->general->sample_count / sf;
    size_t tsy = imd->general->line_count / sf;

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    size_t ii, jj;
    unsigned short *line =
        MALLOC (sizeof(unsigned short) * imd->general->sample_count);
    unsigned char *bytes =
        MALLOC (sizeof(unsigned char) * imd->general->sample_count);

    // Here's where we're putting all this data
    FloatImage *img = float_image_new(tsx, tsy);

    // Read in data line-by-line
    for ( ii = 0 ; ii < tsy ; ii++ ) {

        long long offset =
            (long long)headerBytes+ii*sf*(long long)image_fdr.reclen;

        FSEEK64(fpIn, offset, SEEK_SET);
        if (imd->general->data_type == INTEGER16)
        {
            FREAD(line, sizeof(unsigned short), imd->general->sample_count,
                  fpIn);

            for (jj = 0; jj < imd->general->sample_count; ++jj)
                big16(line[jj]);
        }
        else if (imd->general->data_type == BYTE)
        {
            FREAD(bytes, sizeof(unsigned char), imd->general->sample_count,
                  fpIn);

            for (jj = 0; jj < imd->general->sample_count; ++jj)
                line[jj] = (unsigned short)bytes[jj];
        }

        for ( jj = 0 ; jj < tsx ; jj++ ) {
            // Current sampled value.
            double csv;		

            // We will average a couple pixels together.
            if ( jj * sf < imd->general->line_count - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }

            float_image_set_pixel(img, jj, ii, csv);
        }
    }
    FREE (line);
    FREE (bytes);
    fclose(fpIn);

    char *out_file;
    char *thumb_file = appendToBasename(input_data, "_thumb");

    if (out_dir && strlen(out_dir) > 0) {
        char *basename = get_basename(thumb_file);
        out_file = MALLOC((strlen(out_dir)+strlen(basename)+10)*sizeof(char));
        sprintf(out_file, "%s/%s.jpg", out_dir, basename);
    } else {
        out_file = appendExt(thumb_file, ".jpg");
    }

    // Create the jpeg
    float_image_export_as_jpeg(img, out_file, size, NAN);

    meta_free(imd);
    FREE(data_name);
    FREE(thumb_file);
    FREE(met);
    FREE(out_file);
    FREE(input_metadata);

    return TRUE;
}

void process_file(const char *file, int level, int size)
{
    char *base = get_filename(file);
    char *ext = findExt(base);
    if ((ext && strcmp_case(ext, ".D") == 0) ||
        (strncmp(base, "IMG-", 4) == 0))
    {
        asfPrintStatus("%s%s\n", spaces(level), base);
        generate_ceos_thumbnail(file, size);
    }
    else {
        if (verbose)
            asfPrintStatus("%s%s (ignored)\n", spaces(level), base);
    }
    FREE(base);
}

void process(const char *what, int level, int recursive, int size)
{
  struct stat stbuf;

  if (stat(what, &stbuf) == -1) {
    asfPrintStatus("Cannot access: %s\n", what);
    return;
  }

  char *base = get_filename(what);

  if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
      if (level==0 || recursive) {
          asfPrintStatus("%s%s/\n", spaces(level), base);
          process_dir(what, level+1, recursive, size);
      }
      else {
          if (verbose)
              asfPrintStatus("%s%s (skipped)\n", spaces(level), base);
      }
  }
  else {
      process_file(what, level, size);
  }

  FREE(base);
}

int main(int argc, char *argv[])
{
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  int recursive = FALSE;
  int size = default_thumbnail_size;

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();

  do {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
        CHECK_ARG(1);
        strcpy(logFile,GET_ARG(1));
        fLog = FOPEN(logFile, "a");
        logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
        quietflag = TRUE;
    }
    else if (strmatches(key,"-verbose","--verbose","-v",NULL)) {
        verbose = TRUE;
    }
    else if (strmatches(key,"-recursive","--recursive","-r","-R",NULL)) {
        recursive = TRUE;
    }
    else if (strmatches(key,"-out-dir","--out-dir","--output-dir",
                            "-output-dir","-o",NULL)) {
        CHECK_ARG(1);
        char tmp[1024];
        strcpy(tmp,GET_ARG(1));
        out_dir = MALLOC(sizeof(char)*(strlen(tmp)+1));
        strcpy(out_dir, tmp);
    }
    else if (strmatches(key,"--size","-size","-s",NULL)) {
        CHECK_ARG(1);
        size = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"--",NULL)) {
        break;
    }
    else if (key[0] == '-') {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(ASF_NAME_STRING);
    }
    else {
        // this was a file/dir to process -- back up
        --currArg;
        break;
    }
  } while (currArg < argc);

  if (!quietflag)
      asfSplashScreen(argc, argv);

  if (out_dir) asfPrintStatus("Output directory is: %s\n", out_dir);

  int i;
  for (i=currArg; i<argc; ++i)
      process(argv[i], 0, recursive, size);

  if (fLog) fclose(fLog);
  if (out_dir) free(out_dir);

  exit(EXIT_SUCCESS);
}
