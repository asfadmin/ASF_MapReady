
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
#define MIN_DIMENSION (16)

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
long optimize_na_valid(struct INPUT_ARDOP_PARAMS *params_in);
int is_jpeg(const char *file);
int is_tiff(const char *file);
int is_polsarpro(const char *file);

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
  float scale_factor=-1.0;
  int browseFlag=0;

  // Secret command line parameter for limiting num patches processed for Level 0
  int nPatches, nPatchesFlag=0;

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
        quietflag = 2; // Force -really-quiet behavior (warnings are silenced too)
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
        if (size <= 0) {
            asfPrintWarning("Size (-size option) setting is invalid.  Defaulting to 1024 pixels.\n");
            size = 1024;
        }
        else if (size > 0 && size < MIN_DIMENSION) {
            asfPrintWarning("Size (-size option) too small.  Minimum size is %d pixels.\n"
                    "Defaulting to 1024 pixels.\n", MIN_DIMENSION);
            size = 1024;
        }
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
#ifdef JL0_GO
        else if (strncmp(uc(tmp),"JAXA_L0",7) == 0) {
            L0Flag=jaxa_l0;
        }
#endif
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
            if (!quietflag) {
              fprintf(stderr,"\n**Invalid output format type \"%s\".  Expected tiff or jpeg.\n", tmp);
              usage();
            }
            exit(1);
        }
    }
    else if (strmatches(key,"--scale","-scale",NULL)) {
        CHECK_ARG(1);
        scaleFlag=TRUE;
        scale_factor = atof(GET_ARG(1));
        if (scale_factor < 1.0) {
            if (!quietflag) {
              fprintf(stderr,"\n**Invalid scale factor for -scale option."
                  "  Scale factor must be 1.0 or greater.\n");
              usage();
            }
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
            if (!quietflag) {
              fprintf(stderr,"\n**Invalid number of patches for -patches option."
                  "  Number of patches must be 1.0 or greater.\n");
              usage();
            }
            exit(1);
        }
    }
    else if (strmatches(key,"--",NULL)) {
        break;
    }
    else if (key[0] == '-') {
      if (!quietflag) {
        fprintf(stderr,"\n**Invalid option:  %s\n",argv[currArg-1]);
        usage();
      }
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
      if (!quietflag) {
        fprintf(stderr, "**Invalid combination of options.  You cannot use the -size and -scale"
                        " options\nat the same time.\n");
        usage();
      }
      exit(1);
  }
  if (scaleFlag) size = 0;
  if (sizeFlag)  scale_factor = 0.0;
  if (L0Flag == not_L0 && nPatchesFlag) {
      if (!quietflag) {
        fprintf(stderr, "**Invalid option.  You cannot use the -patches flag without also using\n"
            "the -L0 flag\n");
        usage();
      }
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
  if (!quietflag) {
    asfPrintStatus("Output format is   : %s\n\n",
                   (output_format == JPEG) ? "JPEG" :
                   (output_format == TIF)  ? "TIFF" :
                   "UNKNOWN - Programming error?");
  }

  int i;
  if (currArg >= argc) {
      if (!quietflag) {
        fprintf(stderr,"\n**Wrong number of options.\n");
        usage();
      }
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
/*
 *
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
*/
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

    g_report_level = REPORT_LEVEL_NONE;
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
    int nBands, trailer, ns, nLooks;
    ceos_file_pairs_t ceos_pair = NO_CEOS_FILE_PAIR;
    char *file = get_filename(input_data);

    //Check that input data is available
    ceos_pair = get_ceos_names(input_data, baseName,
                               &inBandName, &inMetaName,
                               &nBands, &trailer);
    if (ceos_pair == NO_CEOS_FILE_PAIR || is_ceos_level0(input_data)) {
        return FALSE;
    }
    int band;
    int not_data = 0;
    for (band = 0; band < nBands; band++) {
      if (is_jpeg(input_data) || is_tiff(input_data))
      {
        not_data = 1;
        break;
      }
    }
    not_data = (ceos_pair == CEOS_IMG_LED_PAIR && strncmp(file, "LED", 3) != 0) ? 1 : not_data;
    if (not_data) {
      return FALSE;
    }

    // Input metadata
    meta_parameters *imd = silent_meta_create(inMetaName[0]);
    ns = imd->general->sample_count;

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16 &&
        imd->general->data_type != COMPLEX_BYTE &&
        imd->general->data_type != COMPLEX_INTEGER16 &&
        imd->general->data_type != COMPLEX_REAL32)
    // Turning off support for these guys for now.
    //    imd->general->data_type != INTEGER32 &&
    //    imd->general->data_type != REAL32 &&
    //    imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        asfPrintWarning("Unknown or unsupported data type: %s\n"
                        "BYTE and 2-byte integers (INTEGER16) are currently supported\n"
                        "for non-level 0 products but complex and other types are not (yet.)\n"
                        "Suggestion: Use asf_import (or asf_import -amplitude for SLC L1.1\n"
                        "products), resample, then asf_export to create thumbnails or browse\n"
                        "images. \n",
                        (imd->general->data_type == BYTE)              ? "BYTE"              :
                        (imd->general->data_type == INTEGER16)         ? "INTEGER16"         :
                        (imd->general->data_type == INTEGER32)         ? "INTEGER32"         :
                        (imd->general->data_type == REAL32)            ? "REAL32"            :
                        (imd->general->data_type == REAL64)            ? "REAL64"            :
                        (imd->general->data_type == COMPLEX_BYTE)      ? "COMPLEX_BYTE"      :
                        (imd->general->data_type == COMPLEX_INTEGER16) ? "COMPLEX_INTEGER16" :
                        (imd->general->data_type == COMPLEX_INTEGER32) ? "COMPLEX_INTEGER32" :
                        (imd->general->data_type == COMPLEX_REAL32)    ? "COMPLEX_REAL32"    :
                        (imd->general->data_type == COMPLEX_REAL64)    ? "COMPLEX_REAL64"    :
                                                                         "UNKNOWN");
        return FALSE;
    }
    if (imd->optical &&
        strcmp_case(imd->general->sensor_name, "PRISM") == 0 &&
        (strcmp_case(imd->general->mode, "1A")  == 0 ||
         strcmp_case(imd->general->mode, "1B1") == 0))
    {
      asfPrintWarning("create_thumbs does not process all available strips of image\n"
                      "data for ALOS %s Level %s products at this time.\n"
                      "The thumbnail or browse image will now be created, but it will\n"
                      "have valid data in only one vertical strip within the image.\n",
                      imd->general->sensor_name,
                      imd->general->mode);
    }

    FILE *fpIn;
    int ll;
    nBands = 1;
    float larger_dim;
    size_t isf;
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
      int leftFill = 0;
      int rightFill = 0;
      if (strcmp(imd->general->sensor, "ALOS") == 0 && imd->optical) {
        get_ALOS_optical_ifiledr(inBandName[0], &image_fdr);
        leftFill = image_fdr.predata + image_fdr.lbrdrpxl;
        rightFill = image_fdr.sufdata + image_fdr.rbrdrpxl;
      }
      else {
        get_ifiledr(inBandName[0], &image_fdr);
        leftFill = image_fdr.lbrdrpxl;
        rightFill = image_fdr.rbrdrpxl;
      }
      // Use ns (16318) not sample_count in headerBytes calculation
      int first_rec_len = firstRecordLen(inBandName[0]);
      int headerBytes = first_rec_len +
              (image_fdr.reclen - (ns + leftFill + rightFill) *
              image_fdr.bytgroup);
      imd->general->sample_count -= leftFill;

      // use a larger dimension at first, for our crude scaling.  We will
      // use a better scaling method later, from GdbPixbuf
      if ((size <= 0 && scale_factor <= 0.0) ||
          (size  > 0 && scale_factor  > 0.0))
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
      // At this point, only size > 0 or scale_factor > 0
      if (size < 0 && scale_factor > 0.0) size = 0;
      if (scale_factor < 0 && size > 0) scale_factor = 0.0;
      float sf = 1.0; // Default scale factor
      nLooks = 1;
      if (size > 0) // if size == 0 then a scale factor is being used
      {
        larger_dim = (float)size;
        if (larger_dim < (float)MIN_DIMENSION) {
            // Note: 'size' has already been validated and set to not smaller than 16
            // by now.
            larger_dim = (float)MIN_DIMENSION;
        }

        // Complex data need to be multilooked
        nLooks = (imd->general->data_type >= COMPLEX_BYTE) ? imd->sar->look_count : nLooks;

        // Vertical and horizontal scale factors required to meet the
        // max_thumbnail_dimension part of the interface contract.
        float vsf = (float)imd->general->line_count / larger_dim / nLooks;
        float hsf = (float)imd->general->sample_count / larger_dim;

        // Overall scale factor to use is the greater of vsf and hsf.
        sf = hsf > vsf ? hsf : vsf;
        if (sf <= 0.0) {
            asfPrintWarning("Scale factor calculation produced an invalid scale factor (%d).\n\n"
                    "Programming error?\n"
                    "line_count = %d\n"
                    "sample_count = %d\n"
                    "largest dimension (from -size option) = %d\n\n"
                    "Defaulting to a 1.0 scale factor...\n",
                    sf, imd->general->line_count, imd->general->sample_count, larger_dim);
            sf = 1.0;
        }
      }
      else if (scale_factor > 0.0) {
        sf = scale_factor;
      }
      else {
        // Shouldn't need to trap an error here...
        asfPrintError("generate_ceos_thumbnail(): Programming error.  Exiting...\n");
      }

      // Thumbnail image sizes.
      isf = (size_t)(sf + 0.5); // Round to nearest integer
      isf = isf < 1 ? 1 : isf; // Only allow scaling down, not up
      size_t tsx = (size_t)((float)imd->general->sample_count / isf);
      size_t tsy = (size_t)((float)imd->general->line_count / isf / (float)nLooks + 0.99);
      asfPrintStatus("\nScaling image by closest integer scale factor (%d.0).  Scaling to: %d by %d\n",
          isf, tsx, tsy);
      if (size > imd->general->sample_count &&
          size > imd->general->line_count)
      {
        asfPrintStatus(
            "\nNOTE: Resizing an image to larger dimensions is not yet supported.  If the\n"
            "option was used to pick a size larger than the largest image dimension, then\n"
            "the scaling factor was limited to 1.0.\n    -size option set to: %d\n"
            "    Image dimensions are: %d lines by %d columns\n\n",
            size, imd->general->line_count, imd->general->sample_count);
      }
      larger_dim = tsx > tsy ? tsx : tsy;

      size_t ii, jj;
      float *line = MALLOC (sizeof(float) * ns);
      unsigned short *shorts = MALLOC (sizeof(unsigned short) * ns);
      unsigned char *bytes = MALLOC (sizeof(unsigned char)  * ns);
      short *cpx_shorts = MALLOC (sizeof(short) * 2 * ns);
      unsigned char *cpx_bytes = MALLOC (sizeof(unsigned char) * 2 * ns);
      float *cpx_floats = MALLOC(sizeof(float)* 2 * ns);
      float re, im;

      // Here's where we're putting all this data
      img = float_image_new(tsx, tsy);

      // Read in data line-by-line
      for ( ii = 0 ; ii < tsy ; ii++ ) {
        long long offset =
	  (long long)headerBytes+ii*isf*nLooks*(long long)image_fdr.reclen;

        FSEEK64(fpIn, offset, SEEK_SET);
        if (imd->general->data_type == INTEGER16)
        {
	  FREAD(shorts, sizeof(unsigned short), ns, fpIn);
          for (jj = 0; jj < imd->general->sample_count; ++jj) {
	    big16(shorts[jj]);
	    line[jj] = (float) shorts[jj];
          }
        }
        else if (imd->general->data_type == BYTE)
        {
          FREAD(bytes, sizeof(unsigned char), ns, fpIn);
          for (jj = 0; jj < imd->general->sample_count; ++jj) {
	    line[jj] = (float) bytes[jj];
          }
        }
	else if (imd->general->data_type == COMPLEX_REAL32) {
	  FREAD(cpx_floats, sizeof(float), 2*ns, fpIn);
	  for (jj = 0; jj < imd->general->sample_count; ++jj) {
	    big32(cpx_floats[jj*2]);
	    big32(cpx_floats[jj*2+1]);
	    re = (float) cpx_floats[jj*2];
	    im = (float) cpx_floats[jj*2+1];
	    line[jj] = sqrt(re*re + im*im);
	  }
	}
	else if (imd->general->data_type == COMPLEX_INTEGER16) {
	  FREAD(cpx_shorts, sizeof(short), 2*ns, fpIn);
	  for (jj = 0; jj < imd->general->sample_count; ++jj) {
	    big16(cpx_shorts[jj*2]);
	    big16(cpx_shorts[jj*2+1]);
	    re = (float) cpx_shorts[jj*2];
	    im = (float) cpx_shorts[jj*2+1];
	    line[jj] = sqrt(re*re + im*im);
	  }
	}
	else if (imd->general->data_type == COMPLEX_BYTE) {
	  FREAD(cpx_bytes, sizeof(unsigned char), 2*ns, fpIn);
	  for (jj = 0; jj < imd->general->sample_count; ++jj) {
	    re = (float) cpx_bytes[jj*2];
	    im = (float) cpx_bytes[jj*2+1];
	    line[jj] = sqrt(re*re + im*im);
	  }
	}

        int kk; // Array iterator
        for ( jj = 0 ; jj < tsx ; jj++ ) {
          // Current sampled value.
          double csv;

          if (isf == 1) {
            csv = line[jj];
          } else {
            // We will average a couple pixels together.
            kk = (int)(jj * isf);
            kk = kk >= imd->general->sample_count ? imd->general->sample_count : kk;
            if (kk < imd->general->sample_count - 1 ) {
              csv = (line[kk] + line[kk + 1]) / 2;
            }
            else {
              csv = (line[kk] + line[kk - 1]) / 2;
            }
          }

          float_image_set_pixel(img, jj, ii, csv);
        }
      }
      FREE (line);
      FREE (bytes);
      FREE (cpx_floats);
      FREE (cpx_shorts);
      FREE (cpx_bytes);
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
        float_image_export_as_tiff(img, out_file, larger_dim, NAN);
        break;
      case JPEG:
      default:
        if (out_dir && strlen(out_dir) > 0) {
          if (!is_dir(out_dir)) {
            create_dir(out_dir);
          }
          char *basename = get_basename(thumb_file);
          out_file = MALLOC((strlen(out_dir)+strlen(basename)+10)*sizeof(char));
          sprintf(out_file, "%s/%s.jpg", out_dir, basename);
        } else {
          out_file = appendExt(thumb_file, ".jpg");
        }
        float_image_export_as_jpeg(img, out_file, larger_dim, NAN);
        break;
    }
    if (saveMetadataFlag) {
      // Copy metadata file to output directory
      char tmp[1024], *outMetaBase, *outMeta;

      outMetaBase = get_basename(inMetaName[0]);
      outMeta = appendExt(outMetaBase, ".meta");
      sprintf(tmp, "%s%c%s", out_dir, DIR_SEPARATOR, outMeta);
      FREE(outMeta);
      FREE(outMetaBase);
      meta_write(imd, tmp);
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
    if (is_polsarpro(file)) {
      asfPrintStatus("\n***\nPolSARpro thumbnails not yet supported.  Best workaround\n"
          "is to create a thumbnail (browse image) from the original\n"
          "CEOS or AIRSAR dataset used to create the PolSARpro data files.\n***\n\n");
      return;
    }
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
        /*ceos_data_ext_t data_ext = */get_ceos_data_name(file, baseName, &dataName, &nBands);
        FREE(baseName);
        asfPrintStatus("%s%s\n", spaces(level), base);
        generate_level0_thumbnail(*dataName, size, verbose, L0Flag, scale_factor, browseFlag,
                                  saveMetadataFlag, nPatchesFlag, nPatches,
                                  output_format, out_dir);
    }
#ifdef JL0_GO
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
#endif
    else if (!is_ceos_level0(file)) {
      generate_ceos_thumbnail(file, size, output_format, out_dir,
                                saveMetadataFlag, scale_factor, browseFlag);
    }
    else {
        // Should never reach here
        asfPrintError("Unrecognized level 0 file format flag\n");
    }
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
#ifndef JL0_GO
      asfPrintError("JAXA Level 0 (AVNIR-2 only) not yet supported...\n");
#endif
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
    char export_path[2048], tmp_folder[256];
    struct INPUT_ARDOP_PARAMS *params_in;
    char **band_name;
    int band_count = 0;
    char ancillary_file[1024]="";

    // FIXME: The following should be command line parameters to create_thumbs and passed along...
    int true_color = 0;
    int false_color = 1;
    int rgb = 1;
    int red_channel = 3;    // Index 3 is the 4th band, i.e. indices 0-3 are channels 1-4, for false color
    int green_channel = 2;  // Index 2 is the 3rd band
    int blue_channel = 1;   // Index 1 is the 2nd band

    time_t t;
    char t_stamp[32];
    t = time(NULL);
    strftime(t_stamp, 22, "%d%b%Y-%Hh_%Mm_%Ss", localtime(&t));
    sprintf(tmp_folder, "./create_thumbs_tmp_dir_%s_%s", get_basename(file), t_stamp);
    if (!is_dir(tmp_folder)) {
        create_dir(tmp_folder);
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
        create_dir(out_dir);
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
		       -99,                 // lowerLon
		       -99,                 // upperLon
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
                       ancillary_file,      /* ancillary file         */
		       NULL,                // colormap
		       NULL,                // slave (gamma)
		       NULL,                // interferogram (gamma)
		       NULL,                // coherence (gamma)
                       NULL,                // baseline (gamma)
		       NULL,                // UAVSAR data type
		       FALSE,               // metaonly
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
            band_name = (char **)MALLOC(sizeof(char*));
            band_name[0] = (char *)MALLOC(strlen(md->general->bands) * sizeof(char));
            band_count = 1;
            strcpy(band_name[0], md->general->bands);
            if (saveMetadataFlag) {
                char dir[1024], file[256], meta_out[1024];
                split_dir_and_file(out_meta, dir, file);
                sprintf(meta_out, "%s%c%s", out_dir, DIR_SEPARATOR, file);
                asfPrintStatus("Saving metadata to %s\n", meta_out);
                meta_write(md, meta_out);
            }
            meta_free(md);
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
		       -99,                 // lowerLon
		       -99,                 // upperLon
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
                       ancillary_file,      /* ancillary file         */
		       NULL,                // colormap
		       NULL,                // slave (gamma)
		       NULL,                // interferogram (gamma)
		       NULL,                // coherence (gamma)
                       NULL,                // baseline (gamma)
		       NULL,                // UAVSAR data type
		       FALSE,               // metaonly
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
            band_name = (char **)MALLOC(sizeof(char*));
            band_name[0] = (char *)MALLOC(strlen(md->general->bands) * sizeof(char));
            band_count = 1;
            strcpy(band_name[0], md->general->bands);
            if (saveMetadataFlag) {
                char dir[1024], file[256], meta_out[1024];
                split_dir_and_file(out_meta, dir, file);
                sprintf(meta_out, "%s%c%s", out_dir, DIR_SEPARATOR, file);
                asfPrintStatus("Saving metadata to %s\n", meta_out);
                meta_write(md, meta_out);
            }
            meta_free(md);
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
		       -99,                 // lowerLon
		       -99,                 // upperLon
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
                       ancillary_file,      /* ancillary file         */
		       NULL,                // colormap
		       NULL,                // slave (gamma)
		       NULL,                // interferogram (gamma)
		       NULL,                // coherence (gamma)
                       NULL,                // baseline (gamma)
		       NULL,                // UAVSAR data type
		       FALSE,               // metaonly
                       out_file);           /* output basename        */
            char *out_meta = appendExt(out_file, ".meta");
            meta_parameters *md = meta_read(out_meta);
            if (md->general->band_count < 1 || md->general->band_count > 4) {
                asfPrintError("Bad band count (%d).\n",
                              md->general->band_count);
            }
            if (strncmp(md->general->bands, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) == 0) {
                asfPrintError("Bad AVNIR-2 Level 0 import ...band name list is unpopulated\n");
            }
            band_count = md->general->band_count;
            band_name = (char **)MALLOC(md->general->band_count * sizeof(char*));
            char **tmp_band_name = extract_band_names(md->general->bands, md->general->band_count);
            int band;
            asfPrintStatus("\nFound bands: ");
            for (band=0;band<band_count;band++) {
                band_name[band] = (char *)MALLOC(256 * sizeof(char));
            }
            if (rgb && !(true_color || false_color)) {
                if (band_count < 3) {
                    asfPrintError("Not enough bands for a color image.\n");
                }
                strcpy(band_name[0], tmp_band_name[red_channel]);
                strcpy(band_name[1], tmp_band_name[green_channel]);
                strcpy(band_name[2], tmp_band_name[blue_channel]);
            }
            else if (true_color) {
                if (band_count < 3) {
                    asfPrintError("Not enough bands for a color image.\n");
                }
                strcpy(band_name[0], tmp_band_name[2]);
                strcpy(band_name[1], tmp_band_name[1]);
                strcpy(band_name[2], tmp_band_name[0]);
            }
            else if (false_color) {
                if (band_count < 4) {
                    asfPrintError("Not enough bands for a false-color image.\n");
                }
                strcpy(band_name[0], tmp_band_name[3]);
                strcpy(band_name[1], tmp_band_name[2]);
                strcpy(band_name[2], tmp_band_name[1]);
            }
            else {
                int i;
                for (i = 0; i < band_count; i++) {
                    strcpy(band_name[i], tmp_band_name[i]);
                }
            }
            for (band = 0; band < band_count; band++) {
                if (band < band_count - 1) {
                    asfPrintStatus("%s, ", tmp_band_name[band]);
                }
                else {
                    asfPrintStatus("%s", tmp_band_name[band]);
                }
            }
            asfPrintStatus("\n\n");
            if (saveMetadataFlag) {
                char dir[1024], file[256], meta_out[1024];
                split_dir_and_file(out_meta, dir, file);
                sprintf(meta_out, "%s%c%s", out_dir, DIR_SEPARATOR, file);
                asfPrintStatus("Saving metadata to %s\n", meta_out);
                meta_write(md, meta_out);
            }
            meta_free(md);
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
        sprintf(out_file, "%s%c%s_ardop", tmp_folder, DIR_SEPARATOR, get_basename(file));
        asfPrintStatus("Ardop from\n    %s\n      to\n    %s\n", in_file, out_file);
        // get_input_ardop_params_struct() does not read the .in file.  It creates a new struct,
        // populates in/out filenames, sets status and CALPRMS to blank and "NO" respectively, and all
        // else to NULL.  See ardop() for code that reads the .in file.
        params_in = get_input_ardop_params_struct(in_file, out_file);
        if (nPatchesFlag) {
            params_in->npatches = (int*)MALLOC(sizeof(int));
            *params_in->npatches = nPatches;
        }
        params_in->na_valid = (int *)MALLOC(sizeof(int));
        *params_in->na_valid = optimize_na_valid(params_in);
        ardop(params_in); // ARDOP
        if (nPatchesFlag) {
            FREE(params_in->npatches);
        }
        FREE(params_in);
        if (saveMetadataFlag) {
            char dir[1024], file[256], in_path[1024], out_path[1024];
            meta_parameters *md;

            sprintf(in_path, "%s_amp.meta", out_file);
            split_dir_and_file(in_path, dir, file);
            sprintf(out_path, "%s%c%s", out_dir, DIR_SEPARATOR, file);
            md = meta_read(in_path);
            meta_write(md, out_path);
            meta_free(md);

            sprintf(in_path, "%s_cpx.meta", out_file);
            split_dir_and_file(in_path, dir, file);
            sprintf(out_path, "%s%c%s", out_dir, DIR_SEPARATOR, file);
            md = meta_read(in_path);
            meta_write(md, out_path);
            meta_free(md);
        }

        // Get rid of temporary files that were input to the last step
        sprintf(del_files, "%s.in", in_file);
        remove(del_files);
        sprintf(del_files, "%s.meta", in_file);
        remove(del_files);
        sprintf(del_files, "%s.img", in_file);
        remove(del_files);
        sprintf(del_files, "%s.fmt", in_file);
        remove(del_files);

        // Convert to ground range
        char del_files2[1024];
        sprintf(del_files2, "%s_cpx", out_file); // Save these filenames for later deletion
        sprintf(in_file, "%s_amp", out_file);
        sprintf(out_file, "%s%c%s_gr", tmp_folder, DIR_SEPARATOR, get_basename(file));
        asfPrintStatus("Converting slant range to ground range from\n    %s\n      to\n    %s\n",
                       in_file, out_file);
        sr2gr(in_file, out_file);
        if (saveMetadataFlag) {
            char dir[1024], file[256], in_path[1024], out_path[1024];
            meta_parameters *md;

            sprintf(in_path, "%s.meta", out_file);
            split_dir_and_file(in_path, dir, file);
            sprintf(out_path, "%s%c%s", out_dir, DIR_SEPARATOR, file);
            md = meta_read(in_path);
            meta_write(md, out_path);
            meta_free(md);
        }

        // Get rid of temporary files that were input to the last step
        sprintf(del_files, "%s.img", in_file);
        remove(del_files);
        sprintf(del_files, "%s.meta", in_file);
        remove(del_files);
        sprintf(del_files, "%s.img", del_files2);
        remove(del_files);
        sprintf(del_files, "%s.meta", del_files2);
        remove(del_files);
        sprintf(in_file, "%s%c%s_ardop.in", tmp_folder, DIR_SEPARATOR, get_basename(file));
        remove(in_file);
    }

    // Resample image
    strcpy(in_file, out_file);
    sprintf(out_file, "%s%c%s_resample", tmp_folder, DIR_SEPARATOR, get_basename(file));
    meta_parameters *meta = meta_read(in_file);
    double xsf, ysf;
    size_t isf;
    if (scale_factor > 0.0) {
        isf = scale_factor >= 0.5 ? (size_t)(scale_factor + 0.5) : 1.0;
        asfPrintStatus("Scaling by %d.0 ...\n", isf);

        xsf = 1.0/(double)isf;
        ysf = 1.0/(double)isf;
    }
    else if (size > 0) {
        xsf = ysf = (double) size / (double)(MAX(meta->general->line_count,
                         meta->general->sample_count));
    }
    else {
        asfPrintError("** Invalid scale factor (%f) and invalid pixel size (%d).\n"
                "Only one of -scale and -size may be specified together,\n"
                "and values must be positive.\n", scale_factor, size);
    }
    asfPrintStatus("Resampling from\n    %s\n      to\n    %s\n", in_file, out_file);
    resample(in_file, out_file, xsf, ysf);
    if (saveMetadataFlag) {
        char dir[1024], file[256], in_path[1024], out_path[1024];
        meta_parameters *md;

        sprintf(in_path, "%s.meta", out_file);
        split_dir_and_file(in_path, dir, file);
        sprintf(out_path, "%s%c%s", out_dir, DIR_SEPARATOR, file);
        md = meta_read(in_path);
        meta_write(md, out_path);
        meta_free(md);
    }

    // Get rid of temporary files that wer input to the last step
    sprintf(del_files, "%s.img", in_file);
    remove(del_files);
    sprintf(del_files, "%s.meta", in_file);
    remove(del_files);

    // Flip so the image is north-up, west-left
    // FIXME: Support flipping of JAXA L0 (AVNIR-2 Level 0) files as well ...
    if (L0Flag == ceos || L0Flag == stf) {
        strcpy(in_file, out_file);
        sprintf(out_file, "%s%c%s_resample_flip", tmp_folder, DIR_SEPARATOR, get_basename(file));
        asfPrintStatus("Flipping to north-up orientation from\n    %s\n      to\n    %s\n", in_file, out_file);
        flip_to_north_up(in_file, out_file);
        if (saveMetadataFlag) {
            char dir[1024], file[256], in_path[1024], out_path[1024];
            meta_parameters *md;

            sprintf(in_path, "%s.meta", out_file);
            split_dir_and_file(in_path, dir, file);
            sprintf(out_path, "%s%c%s", out_dir, DIR_SEPARATOR, file);
            md = meta_read(in_path);
            meta_write(md, out_path);
            meta_free(md);
        }

        // Get rid of temporary file that was input to the last step
        sprintf(del_files, "%s.img", in_file);
        remove(del_files);
        sprintf(del_files, "%s.meta", in_file);
        remove(del_files);
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
        create_dir(out_dir);
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
                         1, /*rgb - must be true if true-color or false-color is set*/
                         0, /*true_color*/
                         1, /*false_color*/
                         "", /*look_up_table_name*/
                         in_file,
                         export_path,
                         (char**)band_name,
                         NULL,/* num output files */
                         NULL);/* output file names */
    }
    else {
        meta_parameters *md = meta_read(in_file);
        char lut_file[2048] = "";
        if (md->colormap && md->colormap->look_up_table &&
            strlen(md->colormap->look_up_table) > 0) {
          strcpy(lut_file, md->colormap->look_up_table);
        }
        asf_export_bands(output_format,
                        SIGMA,
                        0, /*rgb*/
                        0, /*true_color*/
                        0, /*false_color*/
                        lut_file, /*look_up_table_name*/
                        in_file,
                        export_path,
                        (char**)band_name,
                        NULL,/* num output files */
                        NULL);/* output file names */
        meta_free(md);
    }

    // Clean up...
    remove_dir(tmp_folder);
//    char *basename = get_basename(file);
//    if (browseFlag) {
//        sprintf(del_files, "rm -f %s*img", basename);
//    }
//    else {
//        sprintf(del_files, "rm -f %s*_thumb*img", basename);
//    }
//    asfSystem(del_files);
//    if (browseFlag) {
//        sprintf(del_files, "rm -f %s*meta", basename);
//    }
//    else {
//        sprintf(del_files, "rm -f %s*_thumb*meta", basename);
//    }
//    asfSystem(del_files);
    int band;
    for (band = 0; band < band_count; band++) {
        FREE(band_name[band]);
    }
    FREE(band_name);
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
    char *basename = (char *)CALLOC(strlen(file)+10, sizeof(char));
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
        asfPrintStatus("%s\n%s\n", get_basename(filename), get_basename(data_filename));
        if (filename != NULL && data_filename != NULL &&
            strcmp(get_basename(filename), get_basename(data_filename)) == 0)
        {
            ceos_description *ceos = get_ceos_description(file, REPORT_LEVEL_NONE);
            if (ceos->product == RAW && ceos->ceos_data_type == CEOS_RAW_DATA) {
                ret = 1;
            }
        }
    }

    for (i = 0; i < nBands; i++) {
        FREE(dataName[i]);
        if ((i < 2 && pair == CEOS_IMG_LED_PAIR) || /* IMG/LED pairs only populate 2 elements */
            (pair != CEOS_IMG_LED_PAIR))
        {
            FREE(metaName[i]);
        }
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
        if (!quietflag) perror(msg);
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

/*
struct INPUT_ARDOP_PARAMS {
  char  in1[1024];
  char  out[1024];
  char  status[1024];
  char  CALPRMS[1024];
  int *pwrFlag;
  int *sigmaFlag;
  int *gammaFlag;
  int *betaFlag;
  int *hamFlag;
  int *kaiFlag;
  int *ifirstline;
  int *npatches;
  int *isave;
  int *ifirst;
  int *nla;
  float *azres;
  int *deskew;
  int *na_valid;
  float *sloper;
  float *interr;
  float *slopea;
  float *intera;
  float *dsloper;
  float *dinterr;
  float *dslopea;
  float *dintera;
  float *fd;
  float *fdd;
  float *fddd;
  int *iflag;
};
*/
long optimize_na_valid(struct INPUT_ARDOP_PARAMS *params_in) {
    // NOTE: Everything from here to the call to ardop_setup() was cut and
    // pasted directly from ardop() in libasf_ardop ...probably much is
    // unnecessary ...but was expedient.
    meta_parameters *meta;
    struct ARDOP_PARAMS params;

    fill_default_ardop_params(&params);

    /*Structures: these are passed to the sub-routines which need them.*/
    //patch *p;
    satellite *s;
    rangeRef *r;
    getRec *signalGetRec;
    file *f;

    /*Variables.*/
    int n_az,n_range;//, az_reflen;/*Region to be processed.*/
    //int patchNo;/*Loop counter.*/

    /*Setup metadata*/
    /*Create ARDOP_PARAMS struct as well as meta_parameters.*/
    if (extExists(params_in->in1,".in"))
    {/*Read parameters from ARDOP parameter file*/
        read_params(params_in->in1,&params);
        if (extExists(params_in->in1,".meta")) {
            /*Input file has .meta attached: read it*/
            meta=meta_read(params_in->in1);
        } else {
            /*No .meta exists--fabricate one*/
            meta=raw_init();
        }
    }
    else
    {
        /*Read parameters & .meta from CEOS.*/
        /* Caution: This puts hard-coded ERS parameters into params... correct
        them upon return from get_params() based on which platform/beam */
        get_params(params_in->in1,&params,&meta);
    }
    //params_in->npatches = (int *)MALLOC(sizeof(int));
    //*params_in->npatches = 1;

    /*Apply user-overridden parameters*/
    apply_in_ardop_params_to_ardop_params(params_in, &params);
    if (strlen(params.status)>0) set_status_file(params.status);

    /*Doppler*/
    if (params.fdd==-99.0)
    {
        double old_dop=params.fd;
        /*Estimate Doppler in scene center.*/
        estdop(params.in1, 1000,
               &params.fd, &params.fdd, &params.fddd);

        /*De-ambiguify doppler based on old value*/
        while (params.fd-old_dop<-0.5) params.fd+=1.0;
        while (params.fd-old_dop> 0.5) params.fd-=1.0;
    }

    /*Copy fields from ARDOP_PARAMS struct to meta_parameters struct.*/
    meta->sar->image_type              = 'S';        /*Slant range image*/
    meta->sar->look_count              = params.nlooks;
    meta->sar->deskewed                = params.deskew;
    meta->sar->range_time_per_pixel    = 1.0/params.fs;
    meta->sar->azimuth_time_per_pixel  = 1.0/params.prf;
    meta->sar->slant_shift             = params.slantOff;
    meta->sar->time_shift              = params.timeOff;
    meta->sar->slant_range_first_pixel = params.r00;
    meta->sar->wavelength              = params.wavl;
    meta->sar->prf                     = params.prf;
    meta->sar->earth_radius            = params.re;
    meta->sar->satellite_height        = params.re+params.ht;
    meta->sar->range_doppler_coefficients[0] = params.fd*params.prf;
    meta->sar->range_doppler_coefficients[1] = params.fdd*params.prf;
    meta->sar->range_doppler_coefficients[2] = params.fddd*params.prf;
    meta->sar->azimuth_doppler_coefficients[0] = params.fd*params.prf;
    meta->sar->azimuth_doppler_coefficients[1] = 0.0;
    meta->sar->azimuth_doppler_coefficients[2] = 0.0;

    meta->general->data_type = REAL32;
    meta->general->band_count = 1;
    meta->general->x_pixel_size = meta->sar->range_time_per_pixel
            * (speedOfLight/2.0);
//    meta->general->y_pixel_size = meta->sar->azimuth_time_per_pixel
//                                       * params.vel * (params.re/params.ht);
    meta->general->y_pixel_size = meta->sar->azimuth_time_per_pixel * params.vel *
            (params.re/(params.ht+params.re));

    n_az = default_n_az;  // Default patch size
    params.na_valid = -1; // Force ardop_setup() to determine number of valid lines
    ardop_setup(&params,meta,&n_az,&n_range,&s,&r,&f,&signalGetRec);

    // Optimize number of valid lines
    // NOTE: Normally, the number of valid lines is the n_az (always 4096) minus
    // the length of the azimuth reference function.  In reality, this should be
    // called a maximum number of valid lines.  It can sometimes (like always) result
    // in a less than optimal number of lines processed from the input data ...when
    // patch processing gets to the end of the file, it quits processing if there isn't
    // enough data left for a full 4096-line patch to be pulled from the file and
    // processed.  Ideally, the number of valid lines should be selected so that exactly
    // one last 4096-line patch can be pulled from the file.  The following code selects
    // a value for valid lines that attempts to do this ...or ends up as closely as
    // possible.
    long n_ref = s->az_reflen;
    long line_count = (long)meta->general->line_count;
    long n_az_valid = n_az - n_ref;
    long min_valid = 2049; // Guarantee minimum power of 2 is 12 (for a 4096 line patch)
    long best_valid;
    long patches;
    long min_diff = line_count;
    int i;
    for (i=n_az_valid; i>min_valid; i--) {
        patches = (line_count-n_az) / i + 1;
        long diff = line_count - i * patches;
        if (diff == 0) {
            best_valid = i;
            break;
        }
        else if (diff < min_diff) {
            best_valid = i;
            min_diff = diff;
        }
    }
    best_valid /= params.nlooks;
    best_valid *= params.nlooks;
    if (best_valid != n_az_valid) {
        asfPrintStatus("\nAdjusted ardop patch number of valid lines "
                       "from %d to %d to optimize data usage...\n\n",
                       n_az_valid, best_valid);
    }

    return best_valid;
}

int is_jpeg(const char *file)
{
  FILE *fp = NULL;
  unsigned char magic1;
  unsigned char magic2;

  if (fileExists(file)) {
    fp = FOPEN(file, "rb");
  }
  else {
    return 0;
  }

  fread(&magic1, 1, 1, fp);
  fread(&magic2, 1, 1, fp);
  FCLOSE(fp);

  return magic1 == 0xff && magic2 == 0xd8;
}

int is_tiff(const char *file)
{
  int is_a_tiff = 0;
  char *tiff_ext;
  FILE *fp = NULL;
  unsigned char magic1;
  unsigned char magic2;

  tiff_ext = findExt(file);
  if (tiff_ext && fileExists(file)) {
    fp = FOPEN(file, "rb");
  }
  else {
    return 0;
  }
  tiff_ext = (strcmp_case(tiff_ext, ".tif") == 0 || strcmp_case(tiff_ext, ".tiff")) ? tiff_ext : NULL;

  fread(&magic1, 1, 1, fp);
  fread(&magic2, 1, 1, fp);
  FCLOSE(fp);

  if (!quietflag) {
    // The tiff library squawks all over the screen if the file is
    // not a tiff file ...
    TIFF *tiff = TIFFOpen(file, "rb");

    is_a_tiff = tiff && tiff_ext &&
                ((magic1 == 'I' && magic2 == 'I') ||
                 (magic1 == 'M' && magic2 == 'M')   );
  }
  else {
    is_a_tiff = tiff_ext &&
        ((magic1 == 'I' && magic2 == 'I') ||
         (magic1 == 'M' && magic2 == 'M')   );
  }

  return is_a_tiff;
}

int is_polsarpro(const char * infile)
{
  int found_bin = 0;
  int found_bin_hdr = 0;
  char *bin = NULL, *bin_hdr = NULL, *dupe = NULL, *ext = NULL;

  ext = findExt(infile);
  if (!ext) {
    // If no file extension exists, then maybe it has been stripped
    // off.  Guess .bin and check for existence...
    char *inFile = (char *)MALLOC(sizeof(char) * strlen(infile) + 5);
    sprintf(inFile, "%s.bin", infile);
    int ret = is_polsarpro(inFile);
    FREE(inFile);
    return ret;
  }
  if (strcmp_case(ext, ".bin")==0) {
    bin = (char *)infile;
    bin_hdr = (char *)MALLOC(sizeof(char) * (strlen(infile) + 5));
    sprintf(bin_hdr, "%s.hdr", infile);
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(bin_hdr);
  }
  else if (strcmp_case(ext, ".hdr")==0) {
    dupe = STRDUP(infile);
    bin_hdr = (char *)infile;
    ext = findExt(dupe);
    *ext = '\0';
    ext = findExt(dupe);
    if (ext && (strcmp_case(ext, ".bin")==0)) {
      bin = dupe;
    }
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(dupe);
  }

  return (int)(found_bin && found_bin_hdr);
}

