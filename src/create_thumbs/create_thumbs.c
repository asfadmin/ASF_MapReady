#include "asf_export.h"
#include "asf_sar.h"
#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_import.h"
#include "ardop_defs.h"
#include "asf_endian.h"
#include "float_image.h"
#include "asf_nan.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "lzFetch.h"
#include "asf_license.h"
#include "create_thumbs_help.h"

#ifdef linux
#include <unistd.h>
#endif

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#define MIN_ARGS (1)
#define MAX_ARGS (30)

typedef enum {
    not_L0=0,
    stf,
    ceos,
    jaxa_l0
} level_0_flag;

int checkForOption(char* key, int argc, char* argv[]); // in help.c
char *spaces(int n);
int strmatches(const char *key, ...);
void process(const char *what, int top, int recursive, int size, int verbose,
             level_0_flag L0Flag, float scale_factor, int browseFlag,
             int saveMetadataFlag, int nPatchesFlag, int nPatches,
             output_format_t output_format, char *out_dir);
void process_dir(const char *dir, int top, int recursive, int size, int verbose,
                 level_0_flag L0Flag, float scale_factor, int browseFlag,
                 int saveMetadataFlag, int nPatchesFlag, int nPatches,
                 output_format_t output_format, char *out_dir);
void process_file(const char *file, int level, int size, int verbose,
                  level_0_flag L0Flag, float scale_factor, int browseFlag,
                  int saveMetadataFlag, int nPatchesFlag, int nPatches,
                  output_format_t output_format, char *out_dir);
meta_parameters * silent_meta_create(const char *filename);
int generate_ceos_thumbnail(const char *input_data, int size,
                            output_format_t output_format, char *out_dir,
                            int saveMetadataFlag, double scale_factor, int browse_flag);
void generate_level0_thumbnail(const char *file, int size, int verbose, level_0_flag L0Flag,
                               double scale_factor, int browseFlag, int saveMetadataFlag,
                               int nPatchesFlag, int nPatches,
                               output_format_t output_format, char *out_dir);
int is_stf_level0(const char *file);
int is_ceos_level0(const char *file);
void flip_to_north_up(const char *in_file, const char *out_file);
int is_JL0_basename(const char *what);

int main(int argc, char *argv[])
{
  output_format_t output_format=JPEG;
  level_0_flag L0Flag=not_L0;
  int verbose = 0;
  const int default_thumbnail_size = 256;
  char *out_dir = NULL;
  int sizeFlag=0;
  int scaleFlag=0;
  int saveMetadataFlag=0;
  int out_dir_Specified=0;
  int nPatches, nPatchesFlag=0; // Secret command line parameter for limiting num patches processed for Level 0
  float scale_factor=-1.0;
  int browseFlag=0;

  quietflag = (checkForOption("-quiet", argc, argv)  ||
               checkForOption("--quiet", argc, argv) ||
               checkForOption("-q", argc, argv));
  if (argc > 1 && !quietflag) {
      check_for_help(argc, argv);
      handle_license_and_version_args(argc, argv, TOOL_NAME);
  }
  if (argc <= MIN_ARGS || argc > MAX_ARGS) {
      if (!quietflag) usage();
      exit(1);
  }

  int recursive = FALSE;
  int size = default_thumbnail_size;

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
        out_dir_Specified = 1;
        CHECK_ARG(1);
        char tmp[1024];
        strcpy(tmp,GET_ARG(1));
        out_dir = MALLOC(sizeof(char)*(strlen(tmp)+1));
        strcpy(out_dir, tmp);
    }
    else if (strmatches(key,"--size","-size","-s",NULL)) {
        CHECK_ARG(1);
        sizeFlag=TRUE;
        size = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"-L0","-LO","-Lo","-l0","-lO","-lo",NULL)) {
        CHECK_ARG(1);
        char tmp[1024];
        strcpy(tmp,GET_ARG(1));
        if (strncmp(uc(tmp),"STF",3) == 0) {
            L0Flag=stf;
        }
        else if (strncmp(uc(tmp),"CEOS",4) == 0) {
            L0Flag=ceos;
        }
        else if (strncmp(uc(tmp),"JAXA_L0",7) == 0) {
            L0Flag=jaxa_l0;
        }
        else {
            L0Flag=not_L0;
        }
    }
    else if (strmatches(key,"-output-format","--output-format",
             "-out-format", "--out-format", NULL)) {
        CHECK_ARG(1);
        char tmp[1024];
        strcpy(tmp,GET_ARG(1));
        output_format=JPEG; // Default
        if (strncmp(uc(tmp),"TIF",3) == 0) {
            output_format=TIF;
        }
        else if (strncmp(uc(tmp),"JPG",3) == 0 ||
                 strncmp(uc(tmp),"JPEG",4) == 0) {
            output_format=JPEG;
        }
        else {
            fprintf(stderr,"\n**Invalid output format type \"%s\".  Expected tiff or jpeg.\n", tmp);
            if (!quietflag) usage();
            exit(1);
        }
    }
    else if (strmatches(key,"--scale","-scale",NULL)) {
        CHECK_ARG(1);
        scaleFlag=TRUE;
        scale_factor = atof(GET_ARG(1));
        if (scale_factor < 1.0) {
            fprintf(stderr,"\n**Invalid scale factor for -scale option."
                    "  Scale factor must be 1.0 or greater.\n");
            if (!quietflag) usage();
            exit(1);
        }
    }
    else if (strmatches(key,"--browse","-browse","-b",NULL)) {
        browseFlag=TRUE;
    }
    else if (strmatches(key,"--save-metadata","-save-metadata","-sm",NULL)) {
        saveMetadataFlag=TRUE;
    }
    else if (strmatches(key,"--patches","-patches","-p",NULL)) {
        CHECK_ARG(1);
        nPatchesFlag=TRUE;
        nPatches = atoi(GET_ARG(1));
        if (nPatches < 1.0) {
            fprintf(stderr,"\n**Invalid number of patches for -patches option."
                    "  Number of patches must be 1.0 or greater.\n");
            if (!quietflag) usage();
            exit(1);
        }
    }
    else if (strmatches(key,"--",NULL)) {
        break;
    }
    else if (key[0] == '-') {
      fprintf(stderr,"\n**Invalid option:  %s\n",argv[currArg-1]);
      if (!quietflag) usage();
      exit(1);
    }
    else {
        // this was a file/dir to process -- back up
        --currArg;
        break;
    }
  } while (currArg < argc);

  // Check for conflicting options
  if (sizeFlag && scaleFlag) {
      fprintf(stderr, "**Invalid combination of options.  You cannot use the -size and -scale options\n"
              "at the same time.\n");
      if (!quietflag) usage();
      exit(1);
  }
  if (scaleFlag) size = 0;
  if (sizeFlag)  scale_factor = -1.0;
  if (L0Flag == not_L0 && nPatchesFlag) {
      fprintf(stderr, "**Invalid option.  You cannot use the -patches flag without also using\n"
              "the -L0 flag\n");
      if (!quietflag) usage();
      exit(1);
  }

  if (!quietflag) {
      asfSplashScreen(argc, argv);
  }

  if (!out_dir || !out_dir_Specified) {
      out_dir = (char *)MALLOC(2 * sizeof(char));
      strcpy(out_dir, ".");
  }
  if (out_dir && !quietflag) {
      asfPrintStatus("Output directory is: %s\n",
                     (out_dir_Specified) ? out_dir : "(Current directory)");
  }

  int i;
  if (currArg >= argc) {
      fprintf(stderr,"\n**Wrong number of options.\n");
      if (!quietflag) usage();
      exit(1);
  }
  for (i=currArg; i<argc; ++i) {
      process(argv[i], 0, recursive, size, verbose,
              L0Flag, scale_factor, browseFlag, saveMetadataFlag,
              nPatchesFlag, nPatches,
              output_format, out_dir);
  }

  if (fLog) fclose(fLog);
  FREE(out_dir);

  exit(EXIT_SUCCESS);
}

char *spaces(int n)
{
    int i;
    static char buf[256];
    for (i=0; i<256; ++i)
        buf[i] = i<n*3 ? ' ' : '\0';
    return buf;
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

void process_dir(const char *dir, int top, int recursive, int size, int verbose,
                 level_0_flag L0Flag, float scale_factor, int browseFlag,
                 int saveMetadataFlag, int nPatchesFlag, int nPatches,
                 output_format_t output_format, char *out_dir)
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
            process(name, top, recursive, size, verbose,
                    L0Flag, scale_factor, browseFlag,
                    saveMetadataFlag, nPatchesFlag, nPatches,
                    output_format, out_dir);
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

int generate_ceos_thumbnail(const char *input_data, int size,
                            output_format_t output_format, char *out_dir,
                            int saveMetadataFlag, double scale_factor, int browseFlag)
{
    FloatImage *img;
    char **inBandName = NULL, **inMetaName = NULL;
    char baseName[512];
    int nBands, trailer;
    ceos_file_pairs_t ceos_pair = NO_CEOS_FILE_PAIR;

    //Check that input data is available
    ceos_pair = get_ceos_names(input_data, baseName,
                               &inBandName, &inMetaName,
                               &nBands, &trailer);
    if (ceos_pair == NO_CEOS_FILE_PAIR) {
        return FALSE;
    }

    // Input metadata
    meta_parameters *imd = silent_meta_create(inMetaName[0]);

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16)
// Turning off support for these guys for now.
//        imd->general->data_type != INTEGER32 &&
//        imd->general->data_type != REAL32 &&
//        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        asfPrintStatus("Unknown or unsupported data type: %d\n", imd->general->data_type);
        return FALSE;
    }

    FILE *fpIn;
    int ll;
    nBands = 1;
    for (ll=0; ll<nBands; ll++) {

      if (ll == 0) {
    fpIn = fopen(inBandName[ll], "rb");
    if (!fpIn)
      {
        // failed for some reason, quit without thumbnailing
        meta_free(imd);
        asfPrintStatus("Failed to open:\n    %s\n", inBandName[ll]);
        return FALSE;
      }
      }

      struct IOF_VFDR image_fdr;                /* CEOS File Descriptor Record */
      get_ifiledr(inBandName[0], &image_fdr);
      int leftFill = image_fdr.lbrdrpxl;
      int rightFill = image_fdr.rbrdrpxl;
      int headerBytes = firstRecordLen(inBandName[0]) +
              (image_fdr.reclen - (imd->general->sample_count + leftFill + rightFill)
              * image_fdr.bytgroup);

      // use a larger dimension at first, for our crude scaling.  We will
      // use a better scaling method later, from GdbPixbuf
      if ((size <= 0 && scale_factor <= 0.0) ||
      (size > 0 && scale_factor > 0.0))
    {
      // Should never reach here unless the initial option checking gets
      // mucked up (see far above in main())
        asfPrintError("generate_ceos_thumbnail(): Invalid combination of -scale and -size\n"
              "options.  Either they are not initialized or both were used at the same\n"
              "time.  Cannot utilize a pixel size and scale_factor\n"
              "option simultaneously:\n\n"
              "    -size        : %d\n"
              "    -scale-factor: %f\n", size, scale_factor);
    }
      if (size < 0 && scale_factor > 0.0) size = 0;
      if (scale_factor < 0 && size > 0) scale_factor = 0.0;
      int sf;
      if (size > 1024)
    {
      sf = 1; // read in the whole thing
    }
      else if (size > 0) // if size == 0 then a scale factor is being used
    {
      int larger_dim = size*4;
      if (larger_dim < 1024) larger_dim = 1024;

      // Vertical and horizontal scale factors required to meet the
      // max_thumbnail_dimension part of the interface contract.
      int vsf = ceil (imd->general->line_count / larger_dim);
      int hsf = ceil (imd->general->sample_count / larger_dim);
      // Overall scale factor to use is the greater of vsf and hsf.
      sf = (hsf > vsf ? hsf : vsf);
    }
      else if (scale_factor > 0.0) {
        // Round the passed-in scale factor to nearest integer
        sf = (int)(scale_factor + 0.5);
      }
      else {
        // Shouldn't need to trap an error here...
        asfPrintError("generate_ceos_thumbnail(): A pixel size or scale factor must\n"
              "be specified for the output thumbnail (or browse image)\n");
      }

      // Thumbnail image sizes.
      size_t tsx = imd->general->sample_count / sf;
      size_t tsy = imd->general->line_count / sf;

      // Form the thumbnail image by grabbing individual pixels.  FIXME:
      // Might be better to do some averaging or interpolating.
      size_t ii, jj;
      unsigned short *line = MALLOC (sizeof(unsigned short) * imd->general->sample_count);
      unsigned char *bytes = MALLOC (sizeof(unsigned char) * imd->general->sample_count);

      // Here's where we're putting all this data
      img = float_image_new(tsx, tsy);

      // Read in data line-by-line
      for ( ii = 0 ; ii < tsy ; ii++ ) {
        long long offset = (long long)headerBytes+ii*sf*(long long)image_fdr.reclen;

        FSEEK64(fpIn, offset, SEEK_SET);
        if (imd->general->data_type == INTEGER16)
      {
            FREAD(line, sizeof(unsigned short), imd->general->sample_count, fpIn);

            for (jj = 0; jj < imd->general->sample_count; ++jj) {
          big16(line[jj]);
            }
      }
        else if (imd->general->data_type == BYTE)
      {
            FREAD(bytes, sizeof(unsigned char), imd->general->sample_count, fpIn);

            for (jj = 0; jj < imd->general->sample_count; ++jj) {
          line[jj] = (unsigned short)bytes[jj];
            }
      }

        for ( jj = 0 ; jj < tsx ; jj++ ) {
      // Current sampled value.
      double csv;

      if (sf == 1) {
        csv = line[jj];
      } else {
        // We will average a couple pixels together.
        if ( jj * sf < imd->general->line_count - 1 ) {
          csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
        }
        else {
          csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
        }
      }

      float_image_set_pixel(img, jj, ii, csv);
        }
      }
      FREE (line);
      FREE (bytes);
      fclose(fpIn);
    }

    char *out_file;
    char *thumb_file;
    char base_ext[32];
    if (browseFlag) {
        strcpy(base_ext, "");
    }
    else {
        strcpy(base_ext, "_thumb");
    }
    thumb_file = appendToBasename(input_data, base_ext);


    // Create the output file
    switch(output_format) {
        case TIF:
            if (out_dir && strlen(out_dir) > 0) {
                char *basename = get_basename(thumb_file);
                out_file = MALLOC((strlen(out_dir)+strlen(basename)+10)*sizeof(char));
                sprintf(out_file, "%s/%s.tif", out_dir, basename);
            } else {
                out_file = appendExt(thumb_file, ".tif");
            }
            float_image_export_as_tiff(img, out_file, size, NAN);
            break;
        case JPEG:
        default:
            if (out_dir && strlen(out_dir) > 0) {
                if (!is_dir(out_dir)) {
                    mkdir(out_dir, S_IRWXU | S_IRWXG | S_IRWXO);
                }
                char *basename = get_basename(thumb_file);
                out_file = MALLOC((strlen(out_dir)+strlen(basename)+10)*sizeof(char));
                sprintf(out_file, "%s/%s.jpg", out_dir, basename);
            } else {
                out_file = appendExt(thumb_file, ".jpg");
            }
            float_image_export_as_jpeg(img, out_file, size, NAN);
            break;
    }
    if (saveMetadataFlag) {
        // Copy metadata file to output directory
        char tmp[1024], *outMetaBase, *outMeta, tmp_folder[1024];
        time_t t;
        char t_stamp[32];
        char cmd[1024];

        t = time(NULL);
        strftime(t_stamp, 22, "%d%b%Y-%Hh_%Mm_%Ss", localtime(&t));
        outMetaBase = get_basename(inMetaName[0]);
        outMeta = appendExt(outMetaBase, ".meta");
        sprintf(tmp_folder, "./create_thumbs_tmp_dir_%s_%s", outMetaBase, t_stamp);
        if (!is_dir(tmp_folder)) {
            mkdir(tmp_folder, S_IRWXU | S_IRWXG | S_IRWXO);
        }
        else {
        // Should never reach here
            asfPrintError("Temporary folder already exists:\n    %s\n",
                          tmp_folder);
        }
        sprintf(tmp,"%s%c%s", tmp_folder, DIR_SEPARATOR, outMeta);
        FREE(outMeta);
        FREE(outMetaBase);
        meta_write(imd, tmp);
        sprintf(cmd, "cp -f %s %s", tmp, out_dir);
        asfSystem(cmd);
        sprintf(cmd, "rm -rf %s", tmp_folder);
        asfSystem(cmd);
    }

    meta_free(imd);
    FREE(thumb_file);
    FREE(out_file);
    free_ceos_names(inBandName, inMetaName);

    return TRUE;
}

void process_file(const char *file, int level, int size, int verbose,
                  level_0_flag L0Flag, float scale_factor, int browseFlag,
                  int saveMetadataFlag, int nPatchesFlag, int nPatches,
                  output_format_t output_format, char *out_dir)
{
    char *base = get_filename(file);
    char *inDataName = NULL;
    char filename[256], dir[1024];

    split_dir_and_file(file, dir, filename);
    if (L0Flag == stf && is_stf_level0(file)) {
        if (get_stf_data_name(file, &inDataName)) {
            if (strcmp(file, inDataName) == 0) {
                asfPrintStatus("%s%s\n", spaces(level), base);
                generate_level0_thumbnail(inDataName, size, verbose, L0Flag, scale_factor, browseFlag,
                                          saveMetadataFlag, nPatchesFlag, nPatches,
                                          output_format, out_dir);
            }
        }
        else {
            if (verbose) {
                asfPrintStatus("%s%s (ignored)\n", spaces(level), base);
            }
        }
    }
    else if (L0Flag == ceos && is_ceos_level0(file)) {
        char **dataName = NULL, *baseName = (char *)MALLOC(sizeof(char) * 256);
        int nBands;
        ceos_data_ext_t data_ext = get_ceos_data_name(file, baseName, &dataName, &nBands);
        FREE(baseName);
        if (data_ext == CEOS_RAW || data_ext == CEOS_raw) {
            asfPrintStatus("%s%s\n", spaces(level), base);
            generate_level0_thumbnail(*dataName, size, verbose, L0Flag, scale_factor, browseFlag,
                                      saveMetadataFlag, nPatchesFlag, nPatches,
                                      output_format, out_dir);
        }
    }
    else if (L0Flag == jaxa_l0) {
        if (is_JL0_basename(file)) {
            generate_level0_thumbnail(file, size, verbose, L0Flag, scale_factor, browseFlag,
                                      saveMetadataFlag, nPatchesFlag, nPatches,
                                      output_format, out_dir);
        }
        else {
            if (verbose) {
                asfPrintStatus("%s%s (ignored)\n", spaces(level), file);
            }
        }
    }
    else
      generate_ceos_thumbnail(file, size, output_format, out_dir,
                              saveMetadataFlag, scale_factor, browseFlag);
    FREE(base);
}

void process(const char *what, int level, int recursive, int size, int verbose,
             level_0_flag L0Flag, float scale_factor, int browseFlag,
             int saveMetadataFlag, int nPatchesFlag, int nPatches,
             output_format_t output_format, char *out_dir)
{
    struct stat stbuf;

    int is_JL0 = 0;
    if (L0Flag == jaxa_l0) {
        // Check to see if "what" is a JL0 basename (a folder as well) or just
        // a folder
        is_JL0 = is_JL0_basename(what);
    }

    if (stat(what, &stbuf) == -1 && !is_JL0 && L0Flag != jaxa_l0) {
      // Is not a directory - try CEOS data
      char **inBandName = NULL, **inMetaName = NULL;
      int nBands, trailer;
      if (!require_ceos_pair(what, &inBandName, &inMetaName, &nBands, &trailer)) {
          asfPrintStatus("Cannot access: %s\n", what);
          return;
      }
    }

    char *base = get_filename(what);
    if ((stbuf.st_mode & S_IFMT) == S_IFDIR && !is_JL0) {
        if (level==0 || recursive) {
            if (L0Flag == jaxa_l0 && is_JL0) {
                // Trim "what" to just include the basename ...it's a JL0 basename (which
                // is a folder BTW)
                printf("Hi");
            }
            asfPrintStatus("%s%s/\n", spaces(level), base);
            process_dir(what, level+1, recursive, size, verbose,
                        L0Flag, scale_factor, browseFlag,
                        saveMetadataFlag, nPatchesFlag, nPatches,
                        output_format, out_dir);
        }
        else {
            if (verbose) {
                asfPrintStatus("%s%s (skipped)\n", spaces(level), base);
            }
        }
    }
    else {
        process_file(what, level, size, verbose,
                     L0Flag, scale_factor, browseFlag,
                     saveMetadataFlag, nPatchesFlag, nPatches,
                     output_format, out_dir);
    }

    FREE(base);
}

void generate_level0_thumbnail(const char *file, int size, int verbose, level_0_flag L0Flag,
                               double scale_factor, int browseFlag, int saveMetadataFlag,
                               int nPatchesFlag, int nPatches,
                               output_format_t output_format, char *out_dir)
{
    char in_file[1024], out_file[1024], del_files[1024];
    char export_path[2048], tmp_basename[1024], tmp_folder[256];
    struct INPUT_ARDOP_PARAMS *params_in;
    char *band_name[1];

    time_t t;
    char t_stamp[32];
    t = time(NULL);
    strftime(t_stamp, 22, "%d%b%Y-%Hh_%Mm_%Ss", localtime(&t));
    sprintf(tmp_folder, "./create_thumbs_tmp_dir_%s_%s", get_basename(file), t_stamp);
    if (!is_dir(tmp_folder)) {
        mkdir(tmp_folder, S_IRWXU | S_IRWXG | S_IRWXO);
        if (!is_dir(tmp_folder)) {
            asfPrintError("Cannot make temporary directory:\n    %s\n", tmp_folder);
        }
    }
    else {
        // Should never reach here
        asfPrintError("Temporary folder already exists:\n    %s\n",
                      tmp_folder);
    }

    // Set up the output directory
    if (out_dir && strlen(out_dir) && !is_dir(out_dir)) {
        mkdir(out_dir, S_IRWXU | S_IRWXG | S_IRWXO);
        if (!is_dir(out_dir)) {
            asfPrintError("Cannot make output directory:\n    %s\n", out_dir);
        }
    }

    // Import the level 0 file...
    if (L0Flag == stf) {
        char *inDataName = NULL, *inMetaName = NULL;
        // Import to a temporary file
        sprintf(out_file, "%s%c%s_import", tmp_folder, DIR_SEPARATOR, get_basename(file));
        stf_file_pairs_t pair = get_stf_names(file, &inDataName, &inMetaName);
        if (pair != NO_STF_FILE_PAIR &&
            strncmp(file, inDataName, strlen(inDataName)) == 0)
        {
            asfPrintStatus("Importing from\n    %s\n      to\n    %s\n", inDataName, out_file);
            asf_import(r_AMP,               /* power                  */
                       0,                   /* db_flag                */
                       0,                   /* complex_flag           */
                       0,                   /* multilook_flag         */
                       0,                   /* amp0_flag              */
                       STF,                 /* format                 */
                       NULL,                /* band_id                */
                       NULL,                // data type
                       MAGIC_UNSET_STRING,  /*  image_data_type       */
                       NULL,                /* lut                    */
                       NULL,                /* prcPath                */
                       -99,                 /* lowerLat               */
                       -99,                 /* upperLat               */
                       0,                   /* start line subset      */
                       0,                   /* start sample subset    */
                       -99,                 /* width of subset        */
                       -99,                 /* height of subset       */
                       0,                   /* save_intermediates     */
                       NULL,                /* p_range_scale          */
                       NULL,                /* p_azimuth_scale        */
                       NULL,                /* p_correct_y_pixel_size */
                       TRUE,                /* apply_ers_gain_fix     */
                       NULL,                /* inMetaNameOption       */
                       (char *)inDataName,  /* input basename         */
                       out_file);           /* output basename        */
            char *out_meta = appendExt(out_file, ".meta");
            meta_parameters *md = meta_read(out_meta);
            if (md->general->band_count != 1) {
                asfPrintError("Bad band count (%d).  Should be one band.\n",
                              md->general->band_count);
            }
            if (strncmp(md->general->bands, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) == 0) {
                asfPrintError("Bad Level 0 STF import ...band name list is unpopulated\n");
            }
            band_name[0] = (char *)MALLOC(strlen(md->general->bands) * sizeof(char));
            strcpy(band_name[0], md->general->bands);
            meta_free(md);
            if (saveMetadataFlag) {
                char cmd[1024];
                char *out_base = appendExt(out_meta, "");

                sprintf(cmd, "cp -f %s*meta %s", out_base, out_dir);
                asfSystem(cmd);
            }
            FREE(out_meta);
        }
        else {
            remove_dir(tmp_folder);
            FREE(inDataName);
            FREE(inMetaName);
            return;
        }
        FREE(inDataName);
        FREE(inMetaName);
    }
    else if (L0Flag == ceos) {
        char *basename = (char *)MALLOC(sizeof(char)*strlen(file)+10);
        char **dataName = NULL, **metaName = NULL;
        int nBands, trailer;
        ceos_file_pairs_t pair = NO_CEOS_FILE_PAIR;

        // Import to a temporary file
        sprintf(out_file, "%s%c%s_import", tmp_folder, DIR_SEPARATOR, get_basename(file));
        pair = get_ceos_names(file, basename,
                              &dataName, &metaName, &nBands, &trailer);
        FREE(basename);
        if (pair != NO_CEOS_FILE_PAIR && strcmp(file, *dataName) == 0)
        {
            asfPrintStatus("Importing from\n    %s\n      to\n    %s\n", *dataName, out_file);
            asf_import(r_AMP,               /* power                  */
                       0,                   /* db_flag                */
                       0,                   /* complex_flag           */
                       0,                   /* multilook_flag         */
                       0,                   /* amp0_flag              */
                       CEOS,                /* format                 */
                       NULL,                /* band_id                */
                       NULL,                /* data type              */
                       MAGIC_UNSET_STRING,  /* image_data_type        */
                       NULL,                /* lut                    */
                       NULL,                /* prcPath                */
                       -99,                 /* lowerLat               */
                       -99,                 /* upperLat               */
                       0,                   /* start line subset      */
                       0,                   /* start sample subset    */
                       -99,                 /* width of subset        */
                       -99,                 /* height of subset       */
                       0,                   /* save_intermediates     */
                       NULL,                /* p_range_scale          */
                       NULL,                /* p_azimuth_scale        */
                       NULL,                /* p_correct_y_pixel_size */
                       TRUE,                /* apply_ers2_gain_fix    */
                       NULL,                /* inMetaNameOption       */
                       (char *)*dataName,   /* input basename         */
                       out_file);           /* output basename        */
            char *out_meta = appendExt(out_file, ".meta");
            meta_parameters *md = meta_read(out_meta);
            if (md->general->band_count != 1) {
                asfPrintError("Bad band count (%d).  Should be one band.\n",
                              md->general->band_count);
            }
            if (strncmp(md->general->bands, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) == 0) {
                asfPrintError("Bad Level 0 STF import ...band name list is unpopulated\n");
            }
            band_name[0] = (char *)MALLOC(strlen(md->general->bands) * sizeof(char));
            strcpy(band_name[0], md->general->bands);
            meta_free(md);
            if (saveMetadataFlag) {
                char cmd[1024];
                char *out_base = appendExt(out_meta, "");

                sprintf(cmd, "cp -f %s*meta %s", out_base, out_dir);
                asfSystem(cmd);
            }
            FREE(out_meta);
        }
        else {
            remove_dir(tmp_folder);
            return;
        }
    }
    else if (L0Flag == jaxa_l0) {
        // Import to a temporary file
        sprintf(out_file, "%s%c%s_import", tmp_folder, DIR_SEPARATOR, get_basename(file));
        if (is_JL0_basename(file))
        {
            asfPrintStatus("Importing from\n    %s\n      to\n    %s\n", file, out_file);
            asf_import(r_AMP,               /* power                  */
                       0,                   /* db_flag                */
                       0,                   /* complex_flag           */
                       0,                   /* multilook_flag         */
                       0,                   /* amp0_flag              */
                       JAXA_L0,             /* format                 */
                       NULL,                /* band_id                */
                       NULL,                // data type
                       MAGIC_UNSET_STRING,  /*  image_data_type       */
                       NULL,                /* lut                    */
                       NULL,                /* prcPath                */
                       -99,                 /* lowerLat               */
                       -99,                 /* upperLat               */
                       0,                   /* start line subset      */
                       0,                   /* start sample subset    */
                       -99,                 /* width of subset        */
                       -99,                 /* height of subset       */
                       0,                   /* save_intermediates     */
                       NULL,                /* p_range_scale          */
                       NULL,                /* p_azimuth_scale        */
                       NULL,                /* p_correct_y_pixel_size */
                       TRUE,                /* apply_ers_gain_fix     */
                       NULL,                /* inMetaNameOption       */
                       (char *)file,        /* input basename         */
                       out_file);           /* output basename        */
            if (saveMetadataFlag) {
                char cmd[1024];

                sprintf(cmd, "cp -f %s%c*meta %s", tmp_folder, DIR_SEPARATOR, out_dir);
                asfSystem(cmd);
            }
        }
        else {
            remove_dir(tmp_folder);
            return;
        }
    }
    else {
        // Should never reach here
        asfPrintError("Invalid -L0 flag detected.  Valid values are \"-L0 stf\" or \"-L0 ceos\"\n");
    }

    if (L0Flag == ceos || L0Flag == stf) {
        // Run range-doppler algorithm on the raw data
        strcpy(in_file, out_file);
        sprintf(out_file, "%s%c%s_%s_ardop", tmp_folder, DIR_SEPARATOR,
                tmp_basename, get_basename(file));
        asfPrintStatus("Ardop from\n    %s\n      to\n    %s\n", in_file, out_file);
        // get_input_ardop_params_struct() does not read the .in file.  It creates a new struct,
        // populates in/out filenames, sets status and CALPRMS to blank and "NO" respectively, and all
        // else to NULL.  See ardop() for code that reads the .in file.
        params_in = get_input_ardop_params_struct(in_file, out_file);
        if (nPatchesFlag) {
            params_in->npatches = (int*)MALLOC(sizeof(int));
            *params_in->npatches = nPatches;
        }
        ardop(params_in); // ARDOP
        if (nPatchesFlag) {
            FREE(params_in->npatches);
        }
        FREE(params_in);
        if (saveMetadataFlag) {
            char *out_base = appendExt(out_file, "");
            char cmd[1024];
            sprintf(cmd, "cp -f %s*meta %s", out_base, out_dir);
            asfSystem(cmd);
            FREE(out_base);
        }

        // Get rid of temporary files that were input to the last step
        sprintf(del_files, "rm -f %s*", in_file);
        asfSystem(del_files);

        // Convert to ground range
        char del_files2[1024];
        sprintf(del_files2, "%s_cpx", out_file); // Save these filenames for later deletion
        sprintf(in_file, "%s_amp", out_file);
        sprintf(out_file, "%s%c%s_%s_gr", tmp_folder, DIR_SEPARATOR,
                tmp_basename, get_basename(file));
        asfPrintStatus("Converting slant range to ground range from\n    %s\n      to\n    %s\n", in_file, out_file);
        sr2gr(in_file, out_file);
        if (saveMetadataFlag) {
            char *out_base = appendExt(out_file, "");
            char cmd[1024];
            sprintf(cmd, "cp -f %s*meta %s", out_base, out_dir);
            asfSystem(cmd);
            FREE(out_base);
        }

        // Get rid of temporary files that were input to the last step
        sprintf(del_files, "rm -f %s* %s* %s%c*.in", in_file, del_files2, tmp_folder, DIR_SEPARATOR);
        asfSystem(del_files);
    }

    // Resample image
    strcpy(in_file, out_file);
    sprintf(out_file, "%s%c%s_%s_resample", tmp_folder, DIR_SEPARATOR,
            tmp_basename, get_basename(file));
    meta_parameters *meta = meta_read(in_file);
    double xsf, ysf;
    if (scale_factor > 0.0) {
        xsf = 1.0/scale_factor;
        ysf = 1.0/scale_factor;
    }
    else if (size > 0) {
        xsf = ysf = (double) size / (double)(MAX(meta->general->line_count,
                         meta->general->sample_count));
    }
    else {
        fprintf(stderr, "** Invalid scale factor (%f) and invalid pixel size (%d).\n"
                "Only one of -scale and -size may be specified together,\n"
                "and values must be positive.\n", scale_factor, size);
    }
    asfPrintStatus("Resampling from\n    %s\n      to\n    %s\n", in_file, out_file);
    resample(in_file, out_file, xsf, ysf);
    if (saveMetadataFlag) {
        char *out_base = appendExt(out_file, "");
        char cmd[1024];
        sprintf(cmd, "cp -f %s*meta %s", out_base, out_dir);
        asfSystem(cmd);
        FREE(out_base);
    }

    // Get rid of temporary files that wer input to the last step
    sprintf(del_files, "rm -f %s*", in_file);
    asfSystem(del_files);

    // Flip so the image is north-up, west-left
    // FIXME: Support flipping of JAXA L0 (AVNIR-2 Level 0) files as well ...
    if (L0Flag == ceos || L0Flag == stf) {
        strcpy(in_file, out_file);
        sprintf(out_file, "%s%c%s_%s_resample_flip", tmp_folder, DIR_SEPARATOR,
                tmp_basename, get_basename(file));
        asfPrintStatus("Flipping to north-up orientation from\n    %s\n      to\n    %s\n", in_file, out_file);
        flip_to_north_up(in_file, out_file);
        if (saveMetadataFlag) {
            char *out_base = appendExt(out_file, "");
            char cmd[1024];
            sprintf(cmd, "cp -f %s*meta %s", out_base, out_dir);
            asfSystem(cmd);
            FREE(out_base);
        }

        // Get rid of temporary file that was input to the last step
        sprintf(del_files, "rm -f %s.*", in_file);
        asfSystem(del_files);
    }

    // Export to selected graphics file format
    strcpy(in_file, out_file);
    if (browseFlag) {
        sprintf(out_file, "%s", get_basename(file));
    }
    else {
        sprintf(out_file, "%s_thumb", get_basename(file));
    }
    if (out_dir && strlen(out_dir)) {
        sprintf(export_path, "%s%c%s", out_dir, DIR_SEPARATOR,
                out_file);
        mkdir(out_dir, S_IRWXU | S_IRWXG | S_IRWXO);
        if (!is_dir(out_dir)) {
            asfPrintError("Cannot make output directory:\n    %s\n", out_dir);
        }
    }
    else {
        strcpy(export_path, out_file);
    }
    asfPrintStatus("Exporting from\n    %s\n      to\n    %s\n", in_file, export_path);
    if (L0Flag == jaxa_l0) {
        asf_export_bands(output_format,
                         SIGMA,
                         0 /*rgb*/,
                         0 /*true_color*/,
                         1 /*false_color*/,
                         0 /*pauli*/,
                         0 /*sinclair*/,
                         "" /*look_up_table_name*/,
                         in_file,
                         export_path,
                         (char**)band_name);
    }
    else {
        asf_export_bands(output_format,
                        SIGMA,
                        0 /*rgb*/,
                        0 /*true_color*/,
                        0 /*false_color*/,
                        0 /*pauli*/,
                        0 /*sinclair*/,
                        "" /*look_up_table_name*/,
                        in_file,
                        export_path,
                        (char**)band_name);
    }

    // Clean up...
    remove_dir(tmp_folder);
    char *basename = get_basename(file);
    if (browseFlag) {
        sprintf(del_files, "rm -f %s*img", basename);
    }
    else {
        sprintf(del_files, "rm -f %s*_thumb*img", basename);
    }
    asfSystem(del_files);
    if (browseFlag) {
        sprintf(del_files, "rm -f %s*meta", basename);
    }
    else {
        sprintf(del_files, "rm -f %s*_thumb*meta", basename);
    }
    asfSystem(del_files);
    FREE(band_name[0]);
}

// Checks to see if a DATA file is an STF Level 0 file
// FIXME: Might be nice to enhance this to work with metadata files
//        as well, i.e. find the metadata file then derive the data
//        file from it and make sure they both exist, are SKY Telemetry
//        Format etc
int is_stf_level0(const char *file)
{
    char *inDataName = NULL, *inMetaName = NULL, *processor = NULL, *basename;
    char filename[256], dir[1024], path[2048];
    stf_data_ext_t data_ext=NO_STF_DATA;

    if (fileExists(file)) {
        char basename_filename[256];
        split_dir_and_file(file, dir, filename);
        basename = get_stf_basename(file, &data_ext);
        split_dir_and_file(basename, dir, basename_filename);
        if (strlen(filename) != strlen(basename_filename)) {
            // Prevent confusing the actual data file (file.XXX) with associated other
            // files such as file.XXX.af, file.XXX.seg.aa etc.
            data_ext = NO_STF_DATA;
        }
        if (data_ext != NO_STF_DATA) {
            inDataName = (char *)MALLOC(sizeof(char)*2048);
            sprintf(inDataName, "%s%s", dir, filename);
            if (fileExists(inDataName)) {
                char *s, *t, *u;
                inMetaName = (char *)MALLOC(sizeof(char)*2048);

                // Try basename.XXX.par
                sprintf(path, "%s%s%s",
                        dir, basename_filename, ".par");
                if (fileExists(path)) {
                    processor = lzStr(path, "prep_block.processor_name:", NULL);
                    if (strncmp(processor, "SKY", 3) == 0) {
                        FREE(inMetaName);
                        FREE(inDataName);
                        FREE(processor);
                        return 1;
                    }
                }

                // Try basename.XXX.PAR
                sprintf(path, "%s%s%s",
                        dir, basename_filename, ".PAR");
                if (fileExists(path)) {
                    processor = lzStr(path, "prep_block.processor_name:", NULL);
                    if (strncmp(processor, "SKY", 3) == 0) {
                        FREE(inMetaName);
                        FREE(inDataName);
                        FREE(processor);
                        return 1;
                    }
                }

                if (strlen(get_stf_data_extension(data_ext)) > 0) {
                    // Try basename_XXX.par
                    u = STRDUP(get_stf_data_extension(data_ext));
                    t = STRDUP(basename_filename);
                    s = strstr(t, get_stf_data_extension(data_ext));
                    *s = '\0';
                    u++;
                    sprintf(path, "%s%s_%s%s", dir, t, u, ".par");
                    if (fileExists(path)) {
                        processor = lzStr(path, "prep_block.processor_name:", NULL);
                        if (strncmp(processor, "SKY", 3) == 0) {
                            FREE(inMetaName);
                            FREE(inDataName);
                            FREE(processor);
                            FREE(s);
                            FREE(t);
                            FREE(u);
                            return 1;
                        }
                    }

                    // Try basename_XXX.PAR
                    sprintf(path, "%s%s_%s%s", dir, t, u, ".PAR");
                    if (fileExists(path)) {
                        processor = lzStr(path, "prep_block.processor_name:", NULL);
                        if (strncmp(processor, "SKY", 3) == 0) {
                            FREE(inMetaName);
                            FREE(inDataName);
                            FREE(processor);
                            FREE(s);
                            FREE(t);
                            FREE(u);
                            return 1;
                        }
                    }
                }

                FREE(s);
                FREE(t);
                FREE(u);
            }
        }
    }

    FREE(processor);
    FREE(inMetaName);
    FREE(inDataName);
    return 0;
}

int is_ceos_level0(const char *file) {
    char *basename = (char *)MALLOC(sizeof(char)*strlen(file)+10);
    char **dataName = NULL, **metaName = NULL;
    int i, nBands, trailer, ret = 0;
    ceos_file_pairs_t pair = NO_CEOS_FILE_PAIR;

    pair = get_ceos_names(file, basename,
                          &dataName, &metaName, &nBands, &trailer);
    FREE(basename);

    if (pair != NO_CEOS_FILE_PAIR) {
        char dir[1024], filename[256];
        char data_filename[256];
        split_dir_and_file(file, dir, filename);
        split_dir_and_file(*dataName, dir, data_filename);
        if (filename && data_filename &&
            strcmp(filename, data_filename) == 0)
        {
            ceos_description *ceos = get_ceos_description(file, NOREPORT);
            if (ceos->product == RAW && ceos->ceos_data_type == CEOS_RAW_DATA) {
                ret = 1;
            }
        }
    }

    for (i = 0; i < nBands; i++) {
        FREE(dataName[i]);
        FREE(metaName[i]);
    }
    FREE(dataName);
    FREE(metaName);
    return ret;
}

void flip_to_north_up(const char *in_file, const char *out_file)
{
    char input_meta_name[1024], input_data_name[1024];
    char output_meta_name[1024], output_data_name[1024];
    char file[256], path[1024-256+1];
    int vert, horz;
    FloatImage *input = NULL;
    meta_parameters *imd = NULL;

    if (!in_file  || strlen(in_file) <= 0 ||
        !out_file || strlen(out_file) <= 0)
    {
        asfPrintError("flip_to_north_up(): Input and output file(s) not specified.\n");
    }

    split_dir_and_file(in_file, path, file);
    sprintf(input_meta_name, "%s%c%s.meta", path, DIR_SEPARATOR, get_basename(file));
    sprintf(input_data_name, "%s%c%s.img", path, DIR_SEPARATOR, get_basename(file));

    split_dir_and_file(out_file, path, file);
    sprintf(output_meta_name, "%s%c%s.meta", path, DIR_SEPARATOR, get_basename(file));
    sprintf(output_data_name, "%s%c%s.img", path, DIR_SEPARATOR, get_basename(file));

    imd = meta_read(input_meta_name);
    meta_write(imd, output_meta_name);

    vert = (imd->general->orbit_direction == 'D') ? 1 : 0; // Flip if descending during take
    horz = 0; // Unless someone thinks of a need for this, default to using
              // no horizontal flip (east/west)
    asfPrintStatus("\nFlipping image %s.\n",
                   (vert && horz) ? "vertically and horizontally" :
                   (vert)         ? "vertically"                  : "horizontally");

    asfPrintStatus("Input data file:\n    %s\n", input_data_name);
    asfPrintStatus("Output data file:\n    %s\n", output_data_name);

    input = float_image_new_from_file(imd->general->sample_count,
                                      imd->general->line_count,
                                      input_data_name, 0,
                                      FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    if (horz) {
        float_image_flip_x(input);
    }

    if (vert) {
        float_image_flip_y(input);
    }

    float_image_store(input, output_data_name,
                      FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

    meta_free(imd);
}

int is_JL0_basename(const char *_what) {
    struct stat stbuf;
    char *what = STRDUP(_what);

    if (stat(what, &stbuf) == -1) {
        // Failed call to stat() ...return false to halt processing
        char msg[1024];
        sprintf(msg, "%s", what);
        perror(msg);
        FREE(what);
        return 0;
    }

    if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
        // "what" is a directory.  Check to see if it contains VCID (band data) and metadata
        // subfolders ...if so, return true since this is a valid JL0 basename.  If not, return
        // false so process_dir() can handled the folder contents...
        struct stat stbuf2;
        char vcid_dir[5][1024];

        if (what[strlen(what)-1] == '\\' || what[strlen(what)-1] == '/') {
            // Snip off any directory separator character if it is found on the very end of the
            // file/directory name
            what[strlen(what)-1] = '\0';
        }
        sprintf(vcid_dir[0], "%s%c%d", what, DIR_SEPARATOR, 32);
        sprintf(vcid_dir[1], "%s%c%d", what, DIR_SEPARATOR, 45);
        sprintf(vcid_dir[2], "%s%c%d", what, DIR_SEPARATOR, 46);
        sprintf(vcid_dir[3], "%s%c%d", what, DIR_SEPARATOR, 47);
        sprintf(vcid_dir[4], "%s%c%d", what, DIR_SEPARATOR, 48);

        int i;
        for (i = 0; i < 5; i++) {
            if (stat(vcid_dir[i], &stbuf2) == -1) {
                // File doesn't exist or something's wrong with it
                FREE(what);
                return 0;
            }
            if (!(fileExists(vcid_dir[i]) && (stbuf2.st_mode & S_IFMT) == S_IFDIR)) {
                FREE(what);
                return 0;
            }
        }
        FREE(what);
        return 1;
    }
    else {
        FREE(what);
        return 0;
    }
}
