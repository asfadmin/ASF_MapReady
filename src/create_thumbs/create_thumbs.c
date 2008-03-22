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
    ceos
} level_0_flag;

int checkForOption(char* key, int argc, char* argv[]); // in help.c
char *spaces(int n);
int strmatches(const char *key, ...);
int has_prepension(const char * data_file_name);
void process(const char *what, int top, int recursive, int size, int verbose,
             level_0_flag L0Flag, float scale_factor, int browseFlag,
             output_format_t output_format, char *out_dir);
void process_dir(const char *dir, int top, int recursive, int size, int verbose,
                 level_0_flag L0Flag, float scale_factor, int browseFlag,
                 output_format_t output_format, char *out_dir);
void process_file(const char *file, int level, int size, int verbose,
                  level_0_flag L0Flag, float scale_factor, int browseFlag,
                  output_format_t output_format, char *out_dir);
char *meta_file_name(const char * data_file_name);
meta_parameters * silent_meta_create(const char *filename);
int generate_ceos_thumbnail(const char *input_data, int size,
                            output_format_t output_format, char *out_dir);
void generate_level0_thumbnail(const char *file, int size, int verbose, level_0_flag L0Flag,
                               float scale_factor, int browseFlag,
                               output_format_t output_format, char *out_dir);
int is_stf(const char *file);

int main(int argc, char *argv[])
{
  output_format_t output_format=JPEG;
  level_0_flag L0Flag=not_L0;
  int verbose = 0;
  const int default_thumbnail_size = 256;
  char *out_dir = NULL;
  int sizeFlag=0;
  int scaleFlag=0;
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

  // FIXME: Remove this if-statement after CEOS format is supported
  if (L0Flag == ceos) {
      fprintf(stderr,"** Level 0 files in CEOS format are not currently supported (only STF).\n");
      exit(1);
  }

  if (!quietflag) {
      asfSplashScreen(argc, argv);
  }

  if (out_dir && !quietflag) asfPrintStatus("Output directory is: %s\n", out_dir);

  int i;
  if (currArg >= argc) {
      fprintf(stderr,"\n**Wrong number of options.\n");
      if (!quietflag) usage();
      exit(1);
  }
  for (i=currArg; i<argc; ++i) {
      process(argv[i], 0, recursive, size, verbose,
              L0Flag, scale_factor, browseFlag,
              output_format, out_dir);
  }

  if (fLog) fclose(fLog);
  if (out_dir) free(out_dir);

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

void process_dir(const char *dir, int top, int recursive, int size, int verbose,
                 level_0_flag L0Flag, float scale_factor, int browseFlag,
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
                            output_format_t output_format, char *out_dir)
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
        int nBands;
        char **dataName, *baseName, filename[255], dirname[255];
        baseName = (char *) MALLOC(sizeof(char)*256);
        split_dir_and_file(input_metadata, dirname, filename);
        met = MALLOC(sizeof(char)*(strlen(input_metadata)+1));
        sprintf(met, "%s%s", dirname, filename + pre);

        get_ceos_data_name(met, baseName, &dataName, &nBands);

        data_name = STRDUP(dataName[0]);
        imd = silent_meta_create(met);

        free_ceos_names(dataName, NULL);
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
    int sf;
    if (size > 1024)
    {
        sf = 1; // read in the whole thing
    }
    else
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

    // Thumbnail image sizes.
    size_t tsx = imd->general->sample_count / sf;
    size_t tsy = imd->general->line_count / sf;

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    size_t ii, jj;
    unsigned short *line = MALLOC (sizeof(unsigned short) * imd->general->sample_count);
    unsigned char *bytes = MALLOC (sizeof(unsigned char) * imd->general->sample_count);

    // Here's where we're putting all this data
    FloatImage *img = float_image_new(tsx, tsy);

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

    char *out_file;
    char *thumb_file = appendToBasename(input_data, "_thumb");


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
                char *basename = get_basename(thumb_file);
                out_file = MALLOC((strlen(out_dir)+strlen(basename)+10)*sizeof(char));
                sprintf(out_file, "%s/%s.jpg", out_dir, basename);
            } else {
                out_file = appendExt(thumb_file, ".jpg");
            }
            float_image_export_as_jpeg(img, out_file, size, NAN);
            break;
    }

    meta_free(imd);
    FREE(data_name);
    FREE(thumb_file);
    FREE(met);
    FREE(out_file);
    FREE(input_metadata);

    return TRUE;
}

void process_file(const char *file, int level, int size, int verbose,
                  level_0_flag L0Flag, float scale_factor, int browseFlag,
                  output_format_t output_format, char *out_dir)
{
    char *base = get_filename(file);
    char *ext = findExt(base);
    char *inDataName = NULL;
    char filename[256], dir[1024];

    split_dir_and_file(file, dir, filename);
    if ((L0Flag == stf || L0Flag == ceos) && is_stf(file)) { //Note that ceos is currently unsupported for L0
        if (get_stf_data_name(file, &inDataName)) {
            if (strcmp(file, inDataName) == 0) {
                asfPrintStatus("%s%s\n", spaces(level), base);
                generate_level0_thumbnail(inDataName, size, verbose, L0Flag, scale_factor, browseFlag,
                                          output_format, out_dir);
            }
        }
        else {
            if (verbose) {
                asfPrintStatus("%s%s (ignored)\n", spaces(level), base);
            }
        }
    }
    else if ((ext && strcmp_case(ext, ".D") == 0) ||
         (strncmp(base, "IMG-", 4) == 0))
    {
        asfPrintStatus("%s%s\n", spaces(level), base);
        generate_ceos_thumbnail(file, size, output_format, out_dir);
    }
    else {
        if (verbose) {
            asfPrintStatus("%s%s (ignored)\n", spaces(level), base);
        }
    }
    FREE(base);
}

void process(const char *what, int level, int recursive, int size, int verbose,
             level_0_flag L0Flag, float scale_factor, int browseFlag,
             output_format_t output_format, char *out_dir)
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
            process_dir(what, level+1, recursive, size, verbose,
                        L0Flag, scale_factor, browseFlag,
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
                     output_format, out_dir);
    }

    FREE(base);
}

void generate_level0_thumbnail(const char *file, int size, int verbose, level_0_flag L0Flag,
                               float scale_factor, int browseFlag,
                               output_format_t output_format, char *out_dir)
{
    char in_file[1024], out_file[1024], del_files[1024];
    char export_path[2048], *tmp_basename, tmp_folder[256];
    struct INPUT_ARDOP_PARAMS *params_in;

    if (!tmpnam(NULL)) {
        fprintf(stderr, "** Cannot create temporary files.\n");
        exit(1);
    }
    else {
        // Close the tmpnam() security hole by putting something
        // in the name that nobody else would use :)
        char tmp[(L_tmpnam)+256];
        sprintf(tmp_folder, "./create_thumbs_tmp_dir_%s", get_basename(file));
        if (!is_dir(tmp_folder)) {
            mkdir(tmp_folder, S_IRWXU | S_IRWXG | S_IRWXO);
        }
        sprintf(tmp, "%s_UTD_ROCKS_", tmpnam(NULL));
        tmp_basename = get_basename(tmp);
    }
    if (L0Flag == stf) {
        char *inDataName = NULL, *inMetaName = NULL;
        // Import to a temporary file
        sprintf(out_file, "%s%c%s%s_import", tmp_folder, DIR_SEPARATOR,
        tmp_basename, get_basename(file));
        stf_file_pairs_t pair = get_stf_names(file, &inDataName, &inMetaName);
        if (pair != NO_STF_FILE_PAIR &&
            strncmp(file, inDataName, strlen(inDataName)) == 0)
        {
            asf_import(r_AMP,               /* power                  */
                       0,                   /* db_flag                */
                       0,                   /* complex_flag           */
                       0,                   /* multilook_flag         */
                       "STF",               /* format                 */
                       NULL,                /* band_id                */
                       MAGIC_UNSET_STRING,  /*  image_data_type       */
                       NULL,                /* lut                    */
                       NULL,                /* prcPath                */
                       -99,                 /* lowerLat               */
                       -99,                 /* upperLat               */
                       0,                   /* start line subset      */
                       0,                   /* start sample subset    */
                       -99,                 /* width of subset        */
                       -99,                 /* height of subset       */
                       NULL,                /* p_range_scale          */
                       NULL,                /* p_azimuth_scale        */
                       NULL,                /* p_correct_y_pixel_size */
                       NULL,                /* inMetaNameOption       */
                       (char *)inDataName,  /* input basename         */
                       out_file);           /* output basename        */
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
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////
        // FIXME: Remove the error message and exit() when CEOS support is tested and works
        // FIXME: Need to insert code to check for Level 0 CEOS pair before calling asf_import(), see above for STF
        fprintf(stderr, "** CEOS format Level 0 products not yet supported...\n");
        exit(1);
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    }

    // Run range-doppler algorithm on the raw data
    strcpy(in_file, out_file);
    sprintf(out_file, "%s%c%s%s_ardop", tmp_folder, DIR_SEPARATOR,
        tmp_basename, get_basename(file));
    params_in = get_input_ardop_params_struct(in_file, out_file);
    // Un-comment out the following line to limit ardop() to the processing of only 1 patch (for speed
    // while debugging level 0 products)
//#define DEBUG_L0
#ifdef DEBUG_L0
    params_in->npatches = (int*)MALLOC(sizeof(int));
    *params_in->npatches = 1;
#endif
    ardop(params_in);
#ifdef DEBUG_L0
    FREE(params_in->npatches);
#endif
    FREE(params_in);

    // Convert to ground range
    sprintf(in_file, "%s_amp", out_file);
    sprintf(out_file, "%s%c%s%s_gr", tmp_folder, DIR_SEPARATOR,
        tmp_basename, get_basename(file));
    sr2gr(in_file, out_file);

    // Resample image, flip if necessary, and export
    strcpy(in_file, out_file);
    if (browseFlag) {
        strcpy(out_file, file);
    }
    else {
        sprintf(out_file, "%s_thumb", file);
    }
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
    char *band_name[1] = {MAGIC_UNSET_STRING};
    resample(in_file, out_file, xsf, ysf);
    //flip(); // FIXME: Ask Jeremy ...maybe flip ...maybe not
    strcpy(in_file, out_file);
    if (out_dir && strlen(out_dir)) {
        sprintf(export_path, "%s%c%s", out_dir, DIR_SEPARATOR,
        get_basename(out_file));
        if (!is_dir(out_dir)) {
            mkdir(out_dir, S_IRWXU | S_IRWXG | S_IRWXO);
        }
    }
    else {
        strcpy(export_path, out_file);
    }
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

    // Clean up...
    remove_dir(tmp_folder);
    if (browseFlag) {
        sprintf(del_files, "%s.img", file);
    }
    else {
        sprintf(del_files, "%s_thumb.img", file);
    }
    remove(del_files);
    if (browseFlag) {
        sprintf(del_files, "%s.meta", file);
    }
    else {
        sprintf(del_files, "%s_thumb.meta", file);
    }
    remove(del_files);
    FREE(tmp_basename);
}

// Checks to see if a DATA file is an STF Level 0 file
// FIXME: Might be nice to enhance this to work with metadata files
//        as well, i.e. find the metadata file then derive the data
//        file from it and make sure they both exist, are SKY Telemetry
//        Format etc
int is_stf(const char *file)
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

