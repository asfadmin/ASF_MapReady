#define ASF_NAME_STRING "build_cache"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-verbose] [-size <size>]\n"\
"                 [-recursive] <files>\n"

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

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "asf.h"
#include "get_ceos_names.h"
#include "asf_endian.h"
#include "asf_import.h"
#include "float_image.h"

const char *cache_dir = "/export/home/khogenso/.asf/cache";

int verbose = 0;
const int default_thumbnail_size = 512;
int create_location_thumbnails = TRUE;

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

char *cache_file_name(const char *data_file, const char *name, const char *ext)
{
    char *base = get_basename(data_file);
    char *dir = MALLOC(sizeof(char)*(strlen(cache_dir)+strlen(base)+10));
    sprintf(dir, "%s/%s", cache_dir, base);
    char *ret = MALLOC(sizeof(char)*(strlen(base)+strlen(name)+
                                        strlen(ext)+strlen(dir)+10));

    if (strlen(name) > 0)
        sprintf(ret, "%s/%s_%s.%s", dir, base, name, ext);
    else
        sprintf(ret, "%s/%s.%s", dir, base, ext);
   
    if (!fileExists(dir)) {
        printf("Creating directory: %s\n", dir);
        int r = mkdir(dir,0777);
        if (r != 0) perror("Couldn't create cache directory");
    }
 
    free(base);
    printf("Cache file name: %s\n", ret);
    return ret;
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

static GdkPixbuf *world = NULL;
static int world_attempt = FALSE;

char *try_share_file(const char *file)
{
    char buf[1024];
    snprintf(buf, 1024, "%s/world_maps/%s", get_asf_share_dir(), file);
    if (fileExists(buf)) {
      asfPrintStatus("Found world map file: %s\n", file);
      return STRDUP(buf);
    }
    else
      return NULL;
}

char *find_world_pixbuf_file()
{
    // prefer the bigger one, if we can get it.
    char *filename;

    filename = try_share_file("land_shallow_topo_21600.tif");
    if (filename) return filename;

    filename = try_share_file("land_shallow_topo_8192.tif");
    if (filename) return filename;

    filename = try_share_file("land_shallow_topo_8192.jpg");
    if (filename) return filename;

    filename = try_share_file("land_shallow_topo_2048.jpg");
    if (filename) return filename;

    return NULL;
}

int load_world_map()
{
    if (world_attempt) return world != NULL;
    world_attempt = TRUE;

    char *filename = find_world_pixbuf_file();

    if (!filename) {
      asfPrintWarning("Couldn't find a world map file in:\n"
                      "  %s/world_maps\n", get_asf_share_dir());
      return FALSE;
    }

    GError *err = NULL;
    world = gdk_pixbuf_new_from_file(filename, &err);

    if (!world) {
      asfPrintWarning("Couldn't load world map file %s\n"
                      "  Error: %s\n", filename, err->message);
      return FALSE;
    }
    else {
      asfPrintStatus("Loaded.\n");
      return TRUE;
    }
}

void world_latLon_to_lineSamp(double lat, double lon, int *line, int *samp)
{
    double wnl = gdk_pixbuf_get_height(world);
    double wns = gdk_pixbuf_get_width(world);

    if (lon < -180) lon += 360;
    if (lon > 180) lon -= 360;

    assert(lat > -90 && lat < 90);
    assert(lon > -180 && lon < 180);

    // world map:
    //   lines   go from   90 -> -90 (lat)
    //   samples go from -180 -> 180 (lon)
    // to map latitude from degrees to line #:
    //   line = (90-lat)/180 * wnl
    // to map longitude from degrees to sample #:
    //   sample = (lon+180)/360 * wns

    double l = (90.0 - lat)/180.0 * wnl;
    *line = (int)(l + 0.5);

    if (*line > wnl-1) *line = wnl-1;
    if (*line < 0) *line = 0;

    double s = (lon + 180.0)/360.0 * wns;
    *samp = (int)(s + 0.5);

    if (*samp > wns-1) *samp = wns-1;
    if (*samp < 0) *samp = 0;
}

void put_pixel (GdkPixbuf *pixbuf, int x, int y,
                guchar red, guchar green, guchar blue, guchar alpha)
{
    int width, height, rowstride, n_channels;
    guchar *pixels, *p;
    
    n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    
    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    //g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
    //g_assert (n_channels == 4);
    
    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);
    
    g_assert (x >= 0 && x < width);
    g_assert (y >= 0 && y < height);
    
    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);
    
    p = pixels + y * rowstride + x * n_channels;
    p[0] = red;
    p[1] = green;
    p[2] = blue;
    //p[3] = alpha;
}

int ab(int x) { return x>0 ? x : -x; }

void pixbuf_line(GdkPixbuf *pb, int line0, int samp0, int line1, int samp1)
{
    int line_step = line1 - line0;
    int samp_step = samp1 - samp0;

    int steps = ab(line_step) > ab(samp_step) ? ab(line_step) : ab(samp_step);

    int i;
    double line_incr = (double)line_step / (double)steps;
    double samp_incr = (double)samp_step / (double)steps;

    for (i=0; i<steps; ++i) {
        int line_curr = (int)(0.5 + (double)line0 + i*line_incr);
        int samp_curr = (int)(0.5 + (double)samp0 + i*samp_incr);
        put_pixel(pb, samp_curr, line_curr, 255, 0, 0, 0);
    }
}

void
generate_location_thumbnail(const char *input_data, const char *append,
                            meta_parameters *meta, int size, float pad_pct)
{
    if (load_world_map()) {

        int nl = meta->general->line_count;
        int ns = meta->general->sample_count;

        // get corners in lat/lon
        double lat_UL, lon_UL;
        double lat_UR, lon_UR;
        double lat_LL, lon_LL;
        double lat_LR, lon_LR;

        meta_get_latLon(meta, 0,    0,    0, &lat_UL, &lon_UL);
        meta_get_latLon(meta, 0,    ns-1, 0, &lat_UR, &lon_UR);
        meta_get_latLon(meta, nl-1, 0,    0, &lat_LL, &lon_LL);
        meta_get_latLon(meta, nl-1, ns-1, 0, &lat_LR, &lon_LR);

        // get corners in line/samp (of the world map)
        int line_UL, samp_UL;
        int line_UR, samp_UR;
        int line_LL, samp_LL;
        int line_LR, samp_LR;

        world_latLon_to_lineSamp(lat_UL, lon_UL, &line_UL, &samp_UL);
        world_latLon_to_lineSamp(lat_UR, lon_UR, &line_UR, &samp_UR);
        world_latLon_to_lineSamp(lat_LL, lon_LL, &line_LL, &samp_LL);
        world_latLon_to_lineSamp(lat_LR, lon_LR, &line_LR, &samp_LR);

        // find the extents in line/samp space
        int line_min = line_UL, line_max = line_UL;

        if (line_UR < line_min) line_min = line_UR;
        if (line_LR < line_min) line_min = line_UL;
        if (line_LL < line_min) line_min = line_LL;

        if (line_UR > line_max) line_max = line_UR;
        if (line_LR > line_max) line_max = line_UL;
        if (line_LL > line_max) line_max = line_LL;

        int samp_min = samp_UL, samp_max = samp_UL;

        if (samp_UR < samp_min) samp_min = samp_UR;
        if (samp_LR < samp_min) samp_min = samp_UL;
        if (samp_LL < samp_min) samp_min = samp_LL;

        if (samp_UR > samp_max) samp_max = samp_UR;
        if (samp_LR > samp_max) samp_max = samp_UL;
        if (samp_LL > samp_max) samp_max = samp_LL;

        // pad by the padding percentage
        int width = (int) ((float)(samp_max - samp_min) * pad_pct + 0.5);
        int height = (int) ((float)(line_max - line_min) * pad_pct + 0.5);

        if (width > height) height = width;
        if (height > width) width = height;

        // figure out where the upper-left corner of the subimage is
        int start_line = line_min - (height - (line_max-line_min)) / 2;
        int start_samp = samp_min - (width - (samp_max-samp_min)) / 2;

        // this doesn't actually create a new subimage, we must point
        // to the subset, then make a copy.
        GdkPixbuf *sub = gdk_pixbuf_new_subpixbuf(world,
                                                  start_samp, start_line,
                                                  width, height);
        GdkPixbuf *pb = gdk_pixbuf_copy(sub);

        // update to new cooridinate system
        line_UR -= start_line; samp_UR -= start_samp;
        line_UL -= start_line; samp_UL -= start_samp;
        line_LL -= start_line; samp_LL -= start_samp;
        line_LR -= start_line; samp_LR -= start_samp;

        // save it!
        char *out_file = cache_file_name(input_data, append, "jpg");
        GError *err = NULL;

        if (size > width || size > height) {
          // requested size is actually bigger... just leave it as it is
          // rather than stretching
          asfPrintStatus("Output smaller location thumb than requested.\n");

          // draw lines on the piece we cut out, representing the
          // boundary of the scene
          pixbuf_line(pb, line_UL, samp_UL, line_UR, samp_UR);
          pixbuf_line(pb, line_UR, samp_UR, line_LR, samp_LR);
          pixbuf_line(pb, line_LR, samp_LR, line_LL, samp_LL);
          pixbuf_line(pb, line_LL, samp_LL, line_UL, samp_UL);

          gdk_pixbuf_save(pb, out_file, "jpeg", &err, "quality", "100", NULL);
          if (err)
            asfPrintWarning("Error saving pixbuf %s: %s\n", out_file,
                            err->message);
        }
        else {
          // scale to the requested size
          GdkPixbuf *out = gdk_pixbuf_scale_simple(pb, size, size,
                                                   GDK_INTERP_BILINEAR);

          // update to new cooridinate system
          double line_scale = (double)size/(double)height;
          double samp_scale = (double)size/(double)width;
          
          line_UR *= line_scale; samp_UR *= samp_scale;
          line_UL *= line_scale; samp_UL *= samp_scale;
          line_LL *= line_scale; samp_LL *= samp_scale;
          line_LR *= line_scale; samp_LR *= samp_scale;
          
          // draw lines on the piece we cut out, representing the
          // boundary of the scene
          pixbuf_line(out, line_UL, samp_UL, line_UR, samp_UR);
          pixbuf_line(out, line_UR, samp_UR, line_LR, samp_LR);
          pixbuf_line(out, line_LR, samp_LR, line_LL, samp_LL);
          pixbuf_line(out, line_LL, samp_LL, line_UL, samp_UL);

          gdk_pixbuf_save(out, out_file, "jpeg", &err, "quality", "100", NULL);
          if (err)
            asfPrintWarning("Error saving pixbuf %s: %s\n", out_file,
                            err->message);
          g_object_unref(out);
        }

        g_object_unref(pb);
        FREE(out_file);
    }
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
    int larger_dim = 2048;

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

    char *out_file_big = cache_file_name(input_data, "thumb_big", "jpg");
    char *out_file_sml = cache_file_name(input_data, "thumb_sml", "jpg");
    
    // Create the jpegs
    float_image_export_as_jpeg(img, out_file_sml, size, NAN);
    float_image_export_as_jpeg(img, out_file_big, size*2, NAN);

    // See about creating little location thumbnails
    if (create_location_thumbnails) {
      generate_location_thumbnail(input_data, "loc1", imd, size, 2); 
      generate_location_thumbnail(input_data, "loc2", imd, size, 6); 
      generate_location_thumbnail(input_data, "loc3", imd, size, 16); 
    }

    char *meta_name = cache_file_name(input_data, "", "meta");
    meta_write(imd, meta_name);
    meta_free(imd);
    FREE(meta_name);
    FREE(data_name);
    FREE(met);
    FREE(out_file_big);
    FREE(out_file_sml);
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
  gtk_init_check(&argc, &argv);

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

  int i;
  for (i=currArg; i<argc; ++i)
      process(argv[i], 0, recursive, size);

  if (fLog) fclose(fLog);

  if (world)
    g_object_unref(world);

  exit(EXIT_SUCCESS);
}
