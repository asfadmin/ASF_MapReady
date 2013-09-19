#include "asf.h"
#include "asf_sar.h"
#include "asf_raster.h"
#include <ctype.h>

typedef struct {
  int chip_size;         // size of image chip for point target analysis
  int peak_search;       // search radius for determining maximum amplitude
  double peak_threshold; // sigma stdev threshold - above is corner reflector
  double pixel_size;     // threshold for automatic corner reflector detection
} pta_config;

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=(char *)CALLOC(256, sizeof(char));

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k);
  return value;
}

static char *read_str(char *line, char *param)
{
  static char value[255];
  char *p = strchr(line, '=');

  // skip past the '=' sign, and eat up any whitespace
  ++p;
  while (isspace(*p))
      ++p;

  strcpy(value, p);

  // eat up trailing whitespace, too
  p = value + strlen(value) - 1;
  while (isspace(*p))
      *p-- = '\0';

  return value;
}

static int read_int(char *line, char *param)
{
  char *tmp;
  int value;

  tmp = read_str(line, param);
  sscanf(tmp, "%i", &value);

  return value;
}

static double read_double(char *line, char *param)
{
  char *tmp;
  double value;

  tmp = read_str(line, param);
  sscanf(tmp, "%lf", &value);
  
  return value;
}

void read_pta_config(char *configFile, pta_config *pta)
{
  char line[128], params[25]="", *test;
  FILE *fConfig = FOPEN(configFile, "r");
  if (fConfig) {
    while (fgets(line, 128, fConfig) != NULL) {
      if (strncmp(line, "[Point target analysis]", 23) == 0)
	strcpy(params, "pta");
      if (strcmp(params, "pta") == 0) {
	test = read_param(line);
	if (strncmp(test, "chip size", 9) == 0)
	  pta->chip_size = read_int(line, "chip size");
	if (strncmp(test, "peak search", 11) == 0)
	  pta->peak_search = read_int(line, "peak search");
	if (strncmp(test, "peak threshold", 14) == 0)
	  pta->peak_threshold = read_double(line, "peak threshold");
	if (strncmp(test, "pixel size", 10) == 0)
	  pta->pixel_size = read_double(line, "pixel size");
	FREE(test);
      }
    }
  }
  FCLOSE(fConfig);
}

static int outOfBounds(int x, int y, int srcSize, int lines, int samples)
{
  if (x - srcSize/2 + 1 < 0) return TRUE;
  if (y - srcSize/2 + 1 < 0) return TRUE;
  if (x + srcSize/2  >= samples) return TRUE;
  if (y + srcSize/2  >= lines) return TRUE;
  return FALSE;
}

/*
// TopOffPeak:
// Given an array of peak values, use trilinear interpolation to determine the 
// exact (i.e. float) top. This works by finding the peak of a parabola which 
// goes though the highest point, and the three points surrounding it.
static void topOffPeak(float *peaks, int size, int i, int j, int maxI, 
		       float *di, float *dj)
{
#define modX(x) ((x+size)%size)  // Return x, wrapped to [0..srcSize-1]
#define modY(y) ((y+size)%size)  // Return y, wrapped to [0..srcSize-1]
  
  float a, b, c, d;
  a = peaks[modY(j)*maxI + modX(i-1)];
  b = peaks[modY(j)*maxI + modX(i)];
  c = peaks[modY(j)*maxI + modX(i+1)];
  d = 4*((a + c)/2 - b);
  if (d != 0)
    *di = i + (a - c)/d;
  else *di = i;
  a = peaks[modY(j-1)*maxI + modX(i)];
  b = peaks[modY(j)*maxI + modX(i)];
  c = peaks[modY(j+1)*maxI + modX(i)];
  d = 4*((a+c)/2-b);
  if (d != 0)
    *dj = j + (a - c)/d;
  else *dj = j;
}

// FindPeak: 
// Just determines the maxium amplitude value and checks whether it is 
// actually the peak for the neighborhood
static void findPeak(float *s, int size, double *peakX, double *peakY)
{
  float max=-10000000.0;
  int ii, kk, bestX=0, bestY=0;
  float bestLocX, bestLocY;
  
  // Search for the amplitude peak
  for (ii=0; ii<size; ii++)
    for (kk=0; kk<size; kk++)
      if (s[ii*size+kk] > max) {
	bestX = ii;
	bestY = kk;
	max = s[ii*size+kk];
      }
  
  topOffPeak(s, size, bestX, bestY, size, &bestLocX, &bestLocY);
  
  // Output our guess.
  *peakX = bestLocX;
  *peakY = bestLocY;
}
*/

static float findPeakSimple(float *s, int size, double *peakX, double *peakY)
{
  float max=-10000000.0;
  int ii, kk, bestX=0, bestY=0;
  
  // Search for the amplitude peak
  for (ii=0; ii<size; ii++)
    for (kk=0; kk<size; kk++)
      if (s[ii*size+kk] > max) {
	bestX = ii;
	bestY = kk;
	max = s[ii*size+kk];
      }
  
  // Output our guess.
  *peakX = (double)bestX;
  *peakY = (double)bestY;
  return max;
}

int point_target_analysis(char *inFile, char *crFile, char *ptaFile)
{
  int debug = TRUE;
  asfPrintStatus("PTA Revised version 0.1\n");
 
  // Check files
  char dataFile[1024];
  create_name(dataFile, inFile, ".img");
  if (!fileExists(dataFile))
    asfPrintError("Data file (%s) does not exist.\n", dataFile);
  if (!fileExists(crFile))
    asfPrintError("Corner reflector location file (%s) does not exist.\n", 
		  crFile);
  
  // Read configuration file
  char configFile[1024];
  sprintf(configFile, "%s%cpoint_target_analysis.cfg",
	  get_asf_share_dir(), DIR_SEPARATOR);
  pta_config pta;
  read_pta_config(configFile, &pta);

  // Read metadata
  meta_parameters *meta = meta_read(dataFile);
  int line_count = meta->general->line_count;
  int sample_count = meta->general->sample_count;
  double x_pixel_size = meta->general->x_pixel_size;
  double y_pixel_size = meta->general->y_pixel_size;
 
  char *name = meta->general->basename;
  char *scene = get_basename(name);
  if (strncmp(scene, "IMG", 3) == 0) {
    char *stripped_scene = STRDUP(scene+7);
    FREE(scene);
    scene = stripped_scene;
    char *m = strchr(scene, '-');
    if (m) *m = '\0';
  }
  printf("Scene: %s\n", scene);

  // Determine size of image chips, etc.
  double range_res = meta->general->x_pixel_size;
  double azimuth_res = meta->general->y_pixel_size;
  double low_res = (range_res > azimuth_res) ? range_res : azimuth_res;
  int prev_win_size = (int)(3000.0/low_res/2.0)*2+1;
  if (prev_win_size < 201)
    prev_win_size = 201;
  prev_win_size = pta.chip_size;
  int srcSize = prev_win_size; // dummy_roi_size
  asfPrintStatus("\nDEFAULT VALUES\nchip size: %d pixels\n", srcSize);
  asfPrintStatus("peak search: %d pixels\n", pta.peak_search);
  asfPrintStatus("peak threshold: %.3f\n", pta.peak_threshold);
  asfPrintStatus("pixel size: %.3f\n", pta.pixel_size);
  int simple = (x_pixel_size > pta.pixel_size) ? 1 : 0;
  asfPrintStatus("Simple: %s\n", simple ? "Yes" : "No");

  // Handle input and output file
  FILE *fpIn = FOPEN(crFile, "r");
  FILE *fpOut = FOPEN(ptaFile, "w");
  fprintf(fpOut, "# POINT TARGET ANALYSIS RESULTS - REVISED VERSION 0.1\n");
  fprintf(fpOut, "# Scene, Orbit Direction, ID, Lat, Lon, Height, Peak dB, Offset x, Offset y, Total Offset\n");

  // Loop through corner reflector location file
  char line[512], crID[25];
  double lat, lon, height, posX, posY;
  while (fgets(line, 512, fpIn)) {
    if (line[0] != '#') {
      strcpy(crID, get_str(line, 0));
      lat = get_double(line, 1);
      lon = get_double(line, 2);
      height = get_double(line, 3);
      meta_get_lineSamp(meta, lat, lon, height, &posY, &posX);
      if (!outOfBounds(posX, posY, srcSize, line_count, sample_count)) {
	// Get subsets
	int size = srcSize*srcSize;
	int peakSize = pta.peak_search;
	float *chip = (float *) MALLOC(sizeof(float)*size);
	float *peak = (float *) MALLOC(sizeof(float)*peakSize*peakSize);
	FILE *fpImg = FOPEN(dataFile, "rb");
	get_partial_float_lines(fpImg, meta, posY-srcSize/2, srcSize, 
				posX-srcSize/2, srcSize, chip);
	get_partial_float_lines(fpImg, meta, posY-peakSize/2, peakSize,
				posX-peakSize/2, peakSize, peak);
	FCLOSE(fpImg);

	// Make sure that peak chip is in sigma
	//int ii, kk;
	//if (meta->general->radiometry == r_AMP) {
	//  for (ii=0; ii<peakSize; ii++) {
	//    for (kk=0; kk<peakSize; kk++) {
	//      int idx = ii*peakSize + kk;
	//      float incid = meta_incid(meta, 0, posX+kk);
	//      peak[idx] = get_cal_dn(meta, incid, posX+kk, peak[idx], "", 0);
	//    }
	//  }
	//}

	// Determine the stats for the peak chip
	double min, max, mean, stdDev;
	calc_stats_ext(peak, peakSize*peakSize, 0.0001, FALSE, 
		       &min, &max, &mean, &stdDev);

    //for (ii=0; ii<peakSize; ++ii) {
    //  for (kk=0; kk<peakSize; ++kk) {
    //    printf("%8.2f ", peak[ii*peakSize + kk]);
    //  }
    //  printf("\n");
    //}

	// Find amplitude peak
	double srcPeakX, srcPeakY;
	//if (simple)
	  //findPeakSimple(peak, peakSize, &srcPeakX, &srcPeakY);
	//else
	  //findPeak(peak, peakSize, &srcPeakY, &srcPeakX);
      //printf("Regular: %f %f\n", srcPeakX, srcPeakY);
	  float peakVal = findPeakSimple(peak, peakSize, &srcPeakY, &srcPeakX);
      //printf("Simple: %f %f\n", srcPeakX, srcPeakY);
    srcPeakX += .5; srcPeakY += .5;

    simple=1;
    double xfrac = posX - (int)posX;
    double yfrac = posY - (int)posY;
    //printf("xfrac: %f, yfrac: %f\n", xfrac, yfrac);
	
	// Determine offset between peak and reference
	double offX, offY;
	if (simple) {
	  //offX = (srcPeakX - (int)(peakSize/2) + (int)(posX + 0.5) - posX)*
	  //  x_pixel_size;
	  //offY = (srcPeakY - (int)(peakSize/2) + (int)(posY + 0.5) - posY)*
	  //  y_pixel_size;
	  offX = (srcPeakX - (peakSize/2) - xfrac)*x_pixel_size * 48916;
	  offY = (srcPeakY - (peakSize/2) - yfrac)*y_pixel_size * 111444;
	}
	else {
	  offX = (srcPeakX - (int)(peakSize/2))*x_pixel_size;
	  offY = (srcPeakY - (int)(peakSize/2))*y_pixel_size;
	}
        double off = sqrt(offX*offX + offY*offY);
	if (10*log10(peakVal) > -9) {
	  asfPrintStatus("%s, %c, %s, %.5f, %.5f, %.3f, %.3f, %.3f, %.3f, %.3f, OK\n",
	 	       scene, meta->general->orbit_direction, crID, lat, lon, height, 10*log10(peakVal), offX, offY, off);
	  fprintf(fpOut, "%s, %c, %s, %.5f, %.5f, %.3f, %.3f, %.3f, %.3f, %.3f, OK\n",
		  scene, meta->general->orbit_direction, crID, lat, lon, height, 10*log10(peakVal), offX, offY, off);
        }
        else {
	  asfPrintStatus("%s, %c, %s, %.5f, %.5f, %.3f, %.3f, %.3f, %.3f, %.3f, BAD\n",
	 	       scene, meta->general->orbit_direction, crID, lat, lon, height, 10*log10(peakVal), offX, offY, off);
	  fprintf(fpOut, "%s, %c, %s, %.5f, %.5f, %.3f, %.3f, %.3f, %.3f, %.3f, BAD\n",
		  scene, meta->general->orbit_direction, crID, lat, lon, height, 10*log10(peakVal), offX, offY, off);
        } 

	// Save subset
	if (debug && (stdDev > pta.peak_threshold || simple)) {
	  char chipFile[1024], crExt[50];
	  sprintf(crExt, "_%s.img", crID);
	  create_name(chipFile, inFile, crExt);
	  FILE *fp = FOPEN(chipFile, "wb");
	  meta_parameters *meta_debug = meta_init(inFile);
	  meta_debug->general->line_count = 
	    meta_debug->general->sample_count = srcSize;
	  meta_debug->general->data_type = REAL32;
	  meta_debug->general->start_line = posY-srcSize/2;
	  meta_debug->general->start_sample = posX-srcSize/2;
	  meta_debug->general->center_latitude = lat;
	  meta_debug->general->center_longitude = lon;
	  put_float_lines(fp, meta_debug, 0, srcSize, chip);
	  meta_write(meta_debug, chipFile);
	  meta_free(meta_debug);
	  FCLOSE(fp);
	}

	// Clean up
	FREE(chip);
	FREE(peak);
      }
    }
  }

  FCLOSE(fpIn);
  FCLOSE(fpOut);
  meta_free(meta);

  return TRUE;
}
