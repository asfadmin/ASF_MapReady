#include "fgdc_meta.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_import.h"
#include "asf_raster.h"
#include "asf_endian.h"
#include "libasf_proj.h"
#include "xml_util.h"
#include "dateUtil.h"
#include <ctype.h>

fgdc_meta *fgdc_meta_init(void)
{
  fgdc_meta *fgdc = (fgdc_meta *) CALLOC(1, sizeof(fgdc_meta));
 
  // Identification Information
  strcpy(fgdc->datsetid, MAGIC_UNSET_STRING);
  strcpy(fgdc->citation.origin, "Unknown");
  strcpy(fgdc->citation.pubdate, "Unknown");
  strcpy(fgdc->citation.title, "Unknown");
  strcpy(fgdc->citation.onlink, "Unknown");
  strcpy(fgdc->abstract, MAGIC_UNSET_STRING);
  strcpy(fgdc->purpose, MAGIC_UNSET_STRING);
  fgdc->supplinf = NULL;
  strcpy(fgdc->start_time, "Unknown");
  fgdc->center_time = NULL;
  strcpy(fgdc->end_time, "Unknown");
  strcpy(fgdc->current, "Unknown");
  strcpy(fgdc->progress, "Unknown");
  strcpy(fgdc->update, "Unknown");
  fgdc->westbc = MAGIC_UNSET_DOUBLE;
  fgdc->eastbc = MAGIC_UNSET_DOUBLE;
  fgdc->northbc = MAGIC_UNSET_DOUBLE;
  fgdc->southbc = MAGIC_UNSET_DOUBLE;
  strcpy(fgdc->prolevid, "Unknown");
  strcpy(fgdc->prolevau.origin, "Unknown");
  strcpy(fgdc->prolevau.pubdate, "Unknown");
  strcpy(fgdc->prolevau.title, "Unknown");
  strcpy(fgdc->prolevau.geoform, "Unknown");
  fgdc->keywords.theme_count = 0;
  fgdc->keywords.theme = NULL;
  fgdc->keywords.place_count = 0;
  fgdc->keywords.place = NULL;
  strcpy(fgdc->platflnm, "Unknown");
  strcpy(fgdc->instflnm, "Unknown");
  fgdc->numbands = MAGIC_UNSET_INT;
  strcpy(fgdc->accconst, "Unknown");
  strcpy(fgdc->useconst, "Unknown");
  fgdc->copyright = NULL;
  fgdc->browse = NULL;
  fgdc->datacred = NULL;
  fgdc->security = NULL;

  // Data Quality Information
  fgdc->attraccr = NULL;
  strcpy(fgdc->logic, "Unknown");
  strcpy(fgdc->complete, "Unknown");
  fgdc->horizpar = NULL;
  fgdc->vertaccr = NULL;
  fgdc->source_count = 0;
  fgdc->srcinfo = NULL;
  fgdc->process_count = 0;
  fgdc->procstep = NULL;
  strcpy(fgdc->ascdscin, "Unknown");
  fgdc->mode = NULL;

  // Spatial Data Organization Information
  strcpy(fgdc->direct, "Unknown");
  strcpy(fgdc->cvaltype, MAGIC_UNSET_STRING);
  strcpy(fgdc->rasttype, "Unknown");
  fgdc->rowcount = MAGIC_UNSET_INT;
  fgdc->colcount = MAGIC_UNSET_INT;

  // Spatial Reference Information
  fgdc->projected = FALSE;
  fgdc->projection = NULL;
  strcpy(fgdc->plance, "Unknown");
  strcpy(fgdc->ptpos, "Unknown");
  strcpy(fgdc->storord, "Unknown");

  // Distribution Information
  strcpy(fgdc->distrib.cntorg, "Unknown");
  fgdc->distrib.cntpos = NULL;
  strcpy(fgdc->distrib.addrtype, "Unknown");
  strcpy(fgdc->distrib.address, "Unknown");
  strcpy(fgdc->distrib.city, "Unknown");
  strcpy(fgdc->distrib.state, "Unknown");
  strcpy(fgdc->distrib.postal, "Unknown");
  fgdc->distrib.country = NULL;
  fgdc->distrib.cntvoice = NULL;
  fgdc->distrib.cntfax = NULL;
  fgdc->distrib.cntemail = NULL;
  strcpy(fgdc->distliab, "Unknown");
  strcpy(fgdc->formname, "Unknown");
  strcpy(fgdc->networkr, "Unknown");
  strcpy(fgdc->fees, "Unknown");

  // Metadata Reference Information
  strcpy(fgdc->metc.cntorg, "Unknown");
  fgdc->metc.cntpos = NULL;
  strcpy(fgdc->metc.addrtype, "Unknown");
  strcpy(fgdc->metc.address, "Unknown");
  strcpy(fgdc->metc.city, "Unknown");
  strcpy(fgdc->metc.state, "Unknown");
  strcpy(fgdc->metc.postal, "Unknown");
  fgdc->metc.country = NULL;
  fgdc->metc.cntvoice = NULL;
  fgdc->metc.cntfax = NULL;
  fgdc->metc.cntemail = NULL;
  strcpy(fgdc->metstdn, "Unknown");
  strcpy(fgdc->metstdv, "Unknown");
  strcpy(fgdc->metprof, "Unknown");

  return fgdc;
}

meta_projection *gdal2meta_projection(GDALDatasetH hGdal, 
				      int rowcount, int colcount)
{
  char *map_projection = NULL;
  meta_projection *proj = NULL;
  double adfGeoTransform[6];
  if(GDALGetProjectionRef(hGdal) != NULL) {
    map_projection = (char *) GDALGetProjectionRef(hGdal);
    OGRSpatialReferenceH hSRS = OSRNewSpatialReference(NULL);
    if (OSRImportFromWkt(hSRS, &map_projection) == CE_None) {
      OGRErr ogr_error;
      proj = (meta_projection *) MALLOC(sizeof(meta_projection));
      meta_parameters *meta = raw_init();
      meta->general->start_line = 0;
      meta->general->start_sample = 0;
      meta->general->line_count = rowcount;
      meta->general->sample_count = colcount;      

      // Extract projection specific map projection parameters
      if (OSRIsGeographic(hSRS))
	proj->type = LAT_LONG_PSEUDO_PROJECTION;
      else if (OSRIsProjected(hSRS)) {
	char *projection = (char *) MALLOC(sizeof(char)*50);
	sprintf(projection, "%s", OSRGetAttrValue(hSRS, "PROJECTION", 0));
	if (strcmp_case(projection, "Albers_Conic_Equal_Area") == 0) {
	  proj->type = ALBERS_EQUAL_AREA;
	  proj->param.albers.std_parallel1 = 
	    OSRGetProjParm(hSRS, "standard_parallel_1", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.albers.std_parallel2 =
	    OSRGetProjParm(hSRS, "standard_parallel_2", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.albers.center_meridian =
	    OSRGetProjParm(hSRS, "longitude_of_center", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.albers.orig_latitude =
	    OSRGetProjParm(hSRS, "latitude_of_center", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.albers.false_easting =
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.albers.false_northing =
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	}
	else if (strcmp_case(projection, "Equirectangular") == 0) {
	  proj->type = EQUI_RECTANGULAR;
	  proj->param.eqr.orig_latitude =
	    OSRGetProjParm(hSRS, "latitude_of_origin", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.eqr.central_meridian =
	    OSRGetProjParm(hSRS, "central_meridian", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.eqr.false_easting =
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.eqr.false_northing =
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	}
	else if (strcmp_case(projection, "Lambert_Azimuthal_Equal_Area") == 0) {
	  proj->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
	  proj->param.lamaz.center_lat =
	    OSRGetProjParm(hSRS, "latitude_of_center", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamaz.center_lon =
	    OSRGetProjParm(hSRS, "longitude_of_center", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamaz.false_easting =
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamaz.false_northing =
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);	    
	}
	else if (strcmp_case(projection, "Lambert_Conformal_Conic_2SP") == 0) {
	  proj->type = LAMBERT_CONFORMAL_CONIC;
	  proj->param.lamcc.plat1 = 
	    OSRGetProjParm(hSRS, "standard_parallel_1", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamcc.plat2 =
	    OSRGetProjParm(hSRS, "standard_parallel_2", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamcc.lat0 =
	    OSRGetProjParm(hSRS, "latitude_of_origin", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamcc.lon0 =
	    OSRGetProjParm(hSRS, "central_meridian", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamcc.false_easting =
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.lamcc.false_northing =
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	}
	else if (strcmp_case(projection, "Mercator_1SP") == 0) {
	  proj->type = MERCATOR;
	  proj->param.mer.central_meridian =
	    OSRGetProjParm(hSRS, "central_meridian", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.mer.scale_factor =
	    OSRGetProjParm(hSRS, "scale_factor", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.mer.false_easting =
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.mer.false_northing =
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			     &ogr_error);	    
	}
	else if (strcmp_case(projection, "Polar_Stereographic") == 0) {
	  proj->type = POLAR_STEREOGRAPHIC;
	  proj->param.ps.slat =
	    OSRGetProjParm(hSRS, "latitude_of_origin", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.ps.slon =
	    OSRGetProjParm(hSRS, "central_meridian", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.ps.false_easting =
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.ps.false_northing =
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);	    
	}
	else if (strcmp_case(projection, "Transverse_Mercator") == 0) {
	  int north;
	  proj->type = UNIVERSAL_TRANSVERSE_MERCATOR;
	  proj->param.utm.zone = OSRGetUTMZone(hSRS, &north);
	  if (north)
	    proj->hem = 'N';
	  else
	    proj->hem = 'S';
	  proj->param.utm.scale_factor = 
	    OSRGetProjParm(hSRS, "scale_factor", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.utm.lat0 = 
	    OSRGetProjParm(hSRS, "latitude_of_origin", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.utm.lon0 = 
	    OSRGetProjParm(hSRS, "central_meridian", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.utm.false_easting = 
	    OSRGetProjParm(hSRS, "false_easting", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);
	  proj->param.utm.false_northing = 
	    OSRGetProjParm(hSRS, "false_northing", MAGIC_UNSET_DOUBLE, 
			   &ogr_error);	    
	}
	FREE(projection);
      }

      // Extract general map projection parameters
      GDALGetGeoTransform(hGdal, adfGeoTransform);
      proj->startX = adfGeoTransform[0];
      proj->startY = adfGeoTransform[3];
      proj->perX = adfGeoTransform[1];
      proj->perY = adfGeoTransform[5];
      char *units;
      if (proj->type == LAT_LONG_PSEUDO_PROJECTION) {
	OSRGetAngularUnits(hSRS, &units);
	if (strcmp_case(units, "degree") == 0)
	  sprintf(proj->units, "degrees");
	if (proj->startY > 0.0)
	  proj->hem = 'N';
	else
	  proj->hem = 'S';
      }
      else {
	OSRGetLinearUnits(hSRS, &units);
	if (strcmp_case(units, "metre") == 0)
	  sprintf(proj->units, "meters");
      }
      proj->re_major = OSRGetSemiMajor(hSRS, &ogr_error);
      proj->re_minor = OSRGetSemiMinor(hSRS, &ogr_error);

      char *datum = (char *) MALLOC(sizeof(char)*50);
      sprintf(datum, "%s", OSRGetAttrValue(hSRS, "DATUM", 0));
      if (strcmp_case(datum, "WGS_1984") == 0)
	proj->datum = WGS84_DATUM;
      else if (strcmp_case(datum, "North_American_Datum_1983") == 0)
	proj->datum = NAD83_DATUM;
      else if (strcmp_case(datum, "North_American_Datum_1927") == 0)
	proj->datum = NAD27_DATUM;

      char *spheroid = (char *) MALLOC(sizeof(char)*50);
      sprintf(spheroid, "%s", OSRGetAttrValue(hSRS, "SPHEROID", 0));
      if (strcmp_case(spheroid, "WGS 84") == 0)
	proj->spheroid = WGS84_SPHEROID;
      else if (strcmp_case(spheroid, "GRS 1980")==0)
	proj->spheroid = GRS1980_SPHEROID;
    }

    OSRDestroySpatialReference(hSRS);
  }
  return proj;
}

fgdc_meta *get_fgdc_meta(const char *dataFile)
{
  meta_parameters *meta = NULL;
  fgdc_meta *fgdc = fgdc_meta_init();
  int isCEOS = FALSE;

  if (!fileExists(dataFile))
    asfPrintError("File (%s) does not exist!\n", dataFile);

  // We are currently assuming that we deal with raster data in form of pixels.
  // Other data can certainly be covered. At that stage we probably want to
  // define some subroutines that take of that.

  GDALDatasetH hGdal = NULL;
  GDALRasterBandH hBand = NULL;
  GDALDriverH hDriver = NULL;

  // Register all known drivers
  GDALAllRegister();

  // Open file
  hGdal = GDALOpen(dataFile, GA_ReadOnly);
  if (!hGdal)
    asfPrintError("Failed to open file (%s)\n", dataFile);

  sprintf(fgdc->datsetid, "%s", dataFile);

  // Determine what we are dealing with
  hDriver = GDALGetDatasetDriver(hGdal);
  strcpy(fgdc->formname, GDALGetDriverShortName(hDriver));
  if (strcmp_case(fgdc->formname, "GTiff") == 0)
    strcpy(fgdc->formname, "GeoTIFF");

  // For CEOS we might need some extra help
  // Just in case some of the parameters are not correctly filled
  // e.g. KSAT raw did not have correct number of samples
  if (strcmp_case(fgdc->formname, "CEOS") == 0) {
    report_level_t level = REPORT_LEVEL_NONE;
    ceos_description *ceos = get_ceos_description(dataFile, level);
    if (ceos->product == RAW)
      meta = meta_read_raw(dataFile);
    else
      meta = meta_read(dataFile);
    isCEOS = TRUE;
    FREE(ceos);
  }

  // Some information can only be extracted from the metadata structure
  if (meta) {
    if (meta->general->orbit_direction == 'A')
      sprintf(fgdc->ascdscin, "ascending");
    else if (meta->general->orbit_direction == 'D')
      sprintf(fgdc->ascdscin, "descending");
    fgdc->mode = (char *) MALLOC(sizeof(char)*25);
    strcpy(fgdc->mode, meta->general->mode);

    if (!fgdc->center_time) {
      // Read center time
      int ii;
      const char *monthName[12]=
	{"JAN","FEB","MAR","APR","MAY","JUN",
	 "JUL","AUG","SEP","OCT","NOV","DEC"};
      ymd_date center_date, start_date, end_date;
      hms_time center_time, start_time, end_time;
      char month[5];
      sscanf(meta->general->acquisition_date, "%d-%[^- ]-%d, %d:%d:%lf",
	     &center_date.day, month, &center_date.year, 
	     &center_time.hour, &center_time.min, &center_time.sec);    
      for (ii=0; ii<12; ii++)
	if (strcmp_case(month, monthName[ii]) == 0)
	  center_date.month = ii+1;
      
      // Determine time range
      double half_time = meta->sar->azimuth_time_per_pixel * 
	meta->sar->original_line_count / 2;
      start_date = end_date = center_date;
      start_time = end_time = center_time;
      add_time(half_time, &end_date, &end_time);
      end_time.sec += 0.5;
      sub_time(half_time, &start_date, &start_time);
      start_time.sec += 0.5;
      sprintf(fgdc->start_time, "%04d-%02d-%02d %02d:%02d:%02.0lf",
	      start_date.year, start_date.month, start_date.day,
	      start_time.hour, start_time.min, start_time.sec);
      sprintf(fgdc->end_time, "%04d-%02d-%02d %02d:%02d:%02.0lf",
	      end_date.year, end_date.month, end_date.day,
	      end_time.hour, end_time.min, end_time.sec);
    }
  }

  // Determine data type
  // FIX ME: Check endianess issue
  hBand = GDALGetRasterBand(hGdal, 1);
  int dataType = GDALGetRasterDataType(hBand);

  // Apparently GDAL does not know anything about complex_byte
  if ((dataType == GDT_Unknown || dataType == GDT_Byte) && isCEOS) {
    if (meta->general->data_type == BYTE)
      strcpy(fgdc->cvaltype, "unsigned eight bit integer");
    if (meta->general->data_type == INTEGER16)
      strcpy(fgdc->cvaltype, "unsigned sixteen bit integer");
    if (meta->general->data_type == INTEGER32)
      strcpy(fgdc->cvaltype, "unsigned thirtytwo bit integer");
    if (meta->general->data_type == REAL32)
      strcpy(fgdc->cvaltype, "single precision IEEE floating point");
    if (meta->general->data_type == REAL64)
      strcpy(fgdc->cvaltype, "double precision IEEE floating point");
    if (meta->general->data_type == COMPLEX_BYTE)
      strcpy(fgdc->cvaltype, "complex eight bit integer");
    if (meta->general->data_type == COMPLEX_INTEGER16)
      strcpy(fgdc->cvaltype, "complex sixteen bit integer");
    if (meta->general->data_type == COMPLEX_INTEGER32)
      strcpy(fgdc->cvaltype, "complex thirtytwo bit integer");
    if (meta->general->data_type == COMPLEX_REAL32)
      strcpy(fgdc->cvaltype, "complex single precision IEEE floating point");
    if (meta->general->data_type == COMPLEX_REAL64)
      strcpy(fgdc->cvaltype, "complex double precision IEEE floating point");
  }
  else if (dataType == GDT_Byte)
    strcpy(fgdc->cvaltype, "unsigned eight bit integer");
  else if (dataType == GDT_UInt16)
    strcpy(fgdc->cvaltype, "unsigned sixteen bit integer");
  else if (dataType == GDT_Int16)
    strcpy(fgdc->cvaltype, "signed sixteen bit integer");
  else if (dataType == GDT_UInt32)
    strcpy(fgdc->cvaltype, "unsigned thirtytwo bit integer");
  else if (dataType == GDT_Int32)
    strcpy(fgdc->cvaltype, "signed thirtytwo bit integer");
  else if (dataType == GDT_Float32)
    strcpy(fgdc->cvaltype, "single precision IEEE floating point");
  else if (dataType == GDT_Float64)
    strcpy(fgdc->cvaltype, "double precision IEEE floating point");
  else if (dataType == GDT_CInt16)
    strcpy(fgdc->cvaltype, "complex sixteen bit integer");
  else if (dataType == GDT_CInt32)
    strcpy(fgdc->cvaltype, "complex thirtytwo bit integer");
  else if (dataType == GDT_CFloat32)
    strcpy(fgdc->cvaltype, "complex single precision IEEE floating point");
  else if (dataType == GDT_CFloat64)
    strcpy(fgdc->cvaltype, "complex double precision IEEE floating point");

  // Check image dimensions
  fgdc->colcount = GDALGetRasterXSize(hGdal);
  if (fgdc->colcount <= 0 && isCEOS)
    fgdc->colcount = meta->general->sample_count;
  fgdc->rowcount = GDALGetRasterYSize(hGdal);
  if (fgdc->rowcount <= 0 && isCEOS)
    fgdc->rowcount = meta->general->line_count;
  fgdc->numbands = GDALGetRasterCount(hGdal);
  if (fgdc->numbands <= 0 && isCEOS)
    fgdc->numbands = meta->general->band_count;

  // FGDC metadata do not define the geographic corner coordinates of an image.
  // Those need to be determined from the map projection corner coordinates.
  // Unprojected data does not contain this type of information. This will
  // need additional functionality such as metadata structure for CEOS data.

  // Check projection information
  meta_projection *proj = gdal2meta_projection(hGdal, 
					       fgdc->rowcount, fgdc->colcount);
  fgdc->projection = proj;
  if (proj)
    fgdc->projected = TRUE;

  // Setup up metadata structure if we have to
  if (!meta) {
    meta = raw_init();
    meta->general->start_line = 0;
    meta->general->start_sample = 0;
    meta->general->line_count = fgdc->rowcount;
    meta->general->sample_count = fgdc->colcount;      
  }

  // Determine bounding box
  if (!meta->projection) {
    meta->projection = meta_projection_init();
    meta->projection = proj;
  }
  meta_get_bounding_box(meta, &fgdc->southbc, &fgdc->northbc, 
			&fgdc->westbc, &fgdc->eastbc);
  meta->projection = NULL;
  
  GDALClose(hGdal);
  GDALDestroyDriverManager();
  if (meta)
    meta_free(meta);

  return fgdc;
}

void write_fgdc_meta(fgdc_meta *fgdc, const char *outFile) 
{
  int ii, kk;
  int year, month, day, hour, minute, second;
  FILE *fp;
  fp = FOPEN(outFile, "w");

  fprintf(fp, "<?xml version=\"1.0\" ?>\n");
  fprintf(fp, "<metadata>\n");
  // Identification Information
  fprintf(fp, "  <idinfo>\n");
  fprintf(fp, "    <datsetid>%s</datsetid>\n", fgdc->datsetid);
  fprintf(fp, "    <citation>\n");
  fprintf(fp, "      <citeinfo>\n");
  fprintf(fp, "        <origin>%s</origin>\n", fgdc->citation.origin);
  fprintf(fp, "        <pubdate>%s</pubdate>\n", fgdc->citation.pubdate);
  fprintf(fp, "        <title>%s</title>\n", fgdc->citation.title);
  fprintf(fp, "        <onlink>%s</onlink>\n", fgdc->citation.onlink);
  fprintf(fp, "      </citeinfo>\n");
  fprintf(fp, "    </citation>\n");
  fprintf(fp, "    <descript>\n");
  fprintf(fp, "      <abstract>%s</abstract>\n", fgdc->abstract);
  fprintf(fp, "      <purpose>%s</purpose>\n", fgdc->purpose);
  if (fgdc->supplinf) 
    fprintf(fp, "      <supplinf>%s</supplinf>\n", fgdc->supplinf);
  fprintf(fp, "    </descript>\n");
  fprintf(fp, "    <timeperd>\n");
  fprintf(fp, "      <timeinfo>\n");
  if (fgdc->center_time) {
    sscanf(fgdc->center_time, "%d-%d-%d %d:%d:%d",
	   &year, &month, &day, &hour, &minute, &second);
    fprintf(fp, "        <sngdate>\n");
    fprintf(fp, "          <caldate>%04d%02d%02d</caldate>\n", 
	    year, month, day);
    fprintf(fp, "          <time>%02d%02d%02d</time>\n", 
	    hour, minute, second);
    fprintf(fp, "        </sngdate>\n");
  }
  else {
    fprintf(fp, "        <rngdates>\n");
    sscanf(fgdc->start_time, "%d-%d-%d %d:%d:%d",
	   &year, &month, &day, &hour, &minute, &second);
    fprintf(fp, "          <begdate>%04d%02d%02d</begdate>\n", 
	    year, month, day);
    fprintf(fp, "          <begtime>%02d%02d%02d</begtime>\n", 
	    hour, minute, second);
    sscanf(fgdc->end_time, "%d-%d-%d %d:%d:%d",
	   &year, &month, &day, &hour, &minute, &second);
    fprintf(fp, "          <enddate>%04d%02d%02d</enddate>\n", 
	    year, month, day);
    fprintf(fp, "          <endtime>%02d%02d%02d</endtime>\n", 
	    hour, minute, second);
    fprintf(fp, "        </rngdates>\n");
  }
  fprintf(fp, "      </timeinfo>\n");
  fprintf(fp, "      <current>%s</current>\n", fgdc->current);
  fprintf(fp, "    </timeperd>\n");
  fprintf(fp, "    <status>\n");
  fprintf(fp, "      <progress>%s</progress>\n", fgdc->progress);
  fprintf(fp, "      <update>%s</update>\n", fgdc->update);
  fprintf(fp, "    </status>\n");
  fprintf(fp, "    <spdom>\n");
  fprintf(fp, "      <bounding>\n");
  fprintf(fp, "        <westbc>%.4lf</westbc>\n", fgdc->westbc);
  fprintf(fp, "        <eastbc>%.4lf</eastbc>\n", fgdc->eastbc);
  fprintf(fp, "        <northbc>%.4lf</northbc>\n", fgdc->northbc);
  fprintf(fp, "        <southbc>%.4lf</southbc>\n", fgdc->southbc);
  fprintf(fp, "      </bounding>\n");
  fprintf(fp, "    </spdom>\n");
  fprintf(fp, "    <proclevl>\n");
  fprintf(fp, "      <prolevid>%s</prolevid>\n", fgdc->prolevid);
  fprintf(fp, "      <prolevau>\n");
  fprintf(fp, "        <citeinfo>\n");
  fprintf(fp, "          <origin>%s</origin>\n", fgdc->prolevau.origin);
  fprintf(fp, "          <pubdate>%s</pubdate>\n", fgdc->prolevau.pubdate);
  fprintf(fp, "          <title>%s</title>\n", fgdc->prolevau.title);
  fprintf(fp, "          <geoform>%s</geoform>\n", fgdc->prolevau.geoform);
  fprintf(fp, "        </citeinfo>\n");
  fprintf(fp, "      </prolevau>\n");
  fprintf(fp, "    </proclevl>\n");
  fprintf(fp, "    <keywords>\n");
  for (kk=0; kk<fgdc->keywords.theme_count; kk++) {
    fprintf(fp, "      <theme>\n");
    fprintf(fp, "        <themekt>%s</themekt>\n", 
	    fgdc->keywords.theme[kk].thesaurus);
    for (ii=0; ii<fgdc->keywords.theme[kk].key_count; ii++) {
      fprintf(fp, "        <themekey>%s</themekey>\n",
	      fgdc->keywords.theme[kk].key[ii]);
    }
    fprintf(fp, "      </theme>\n");
  }
  if (fgdc->keywords.place_count > 0) {
    for (kk=0; kk<fgdc->keywords.place_count; kk++) {
      fprintf(fp, "      <place>\n");
      fprintf(fp, "        <placekt>%s</placekt>\n", 
	      fgdc->keywords.place[kk].thesaurus);
      for (ii=0; ii<fgdc->keywords.place[kk].key_count; ii++) {
	fprintf(fp, "        <placekey>%s</placekey>\n",
		fgdc->keywords.place[kk].key[ii]);
      }
      fprintf(fp, "      </place>\n");
    }
  }
  fprintf(fp, "    </keywords>\n");
  fprintf(fp, "    <plainsid>\n");
  fprintf(fp, "      <platflnm>%s</platflnm>\n", fgdc->platflnm);
  fprintf(fp, "      <instflnm>%s</instflnm>\n", fgdc->instflnm);
  if (fgdc->mode)
    fprintf(fp, "      <mode>%s</mode>\n", fgdc->mode);
  fprintf(fp, "    </plainsid>\n");
  fprintf(fp, "    <bandidnt>\n");
  fprintf(fp, "      <numbands>%d</numbands>\n", fgdc->numbands);
  fprintf(fp, "    </bandidnt>\n");
  fprintf(fp, "    <accconst>%s</accconst>\n", fgdc->accconst);
  fprintf(fp, "    <useconst>%s</useconst>\n", fgdc->useconst);
  if (fgdc->copyright)
    fprintf(fp, "    <copyright>%s</copyright>\n", fgdc->copyright);
  fprintf(fp, "    <ptcontac>\n");
  fprintf(fp, "      <cntinfo>\n");
  fprintf(fp, "        <cntperp>\n");
  fprintf(fp, "          <cntper>%s</cntper>\n", fgdc->ptcontac.cntper);
  fprintf(fp, "          <cntorg>%s</cntorg>\n", fgdc->ptcontac.cntorg);
  fprintf(fp, "        </cntperp>\n");
  if (fgdc->ptcontac.cntpos && strlen(fgdc->ptcontac.cntpos) > 1)
    fprintf(fp, "        <cntpos>%s</cntpos>\n", fgdc->ptcontac.cntpos);
  fprintf(fp, "        <cntaddr>\n");
  fprintf(fp, "          <addrtype>%s</addrtype>\n", 
	  fgdc->ptcontac.addrtype);
  fprintf(fp, "          <address>%s</address>\n", fgdc->ptcontac.address);
  fprintf(fp, "          <city>%s</city>\n", fgdc->ptcontac.city);
  fprintf(fp, "          <state>%s</state>\n", fgdc->ptcontac.state);
  fprintf(fp, "          <postal>%s</postal>\n", fgdc->ptcontac.postal);
  if (fgdc->ptcontac.country && strlen(fgdc->ptcontac.country) > 1)
    fprintf(fp, "          <country>%s</country>\n", fgdc->ptcontac.country);
  fprintf(fp, "        </cntaddr>\n");
  if (fgdc->ptcontac.cntvoice && strlen(fgdc->ptcontac.cntvoice) > 1)
    fprintf(fp, "        <cntvoice>%s</cntvoice>\n", 
	    fgdc->ptcontac.cntvoice);
  if (fgdc->ptcontac.cntfax && strlen(fgdc->ptcontac.cntfax) > 1)
    fprintf(fp, "        <cntfax>%s</cntfax>\n", fgdc->ptcontac.cntfax);
  if (fgdc->ptcontac.cntemail && strlen(fgdc->ptcontac.cntemail) > 1)
    fprintf(fp, "        <cntemail>%s</cntemail>\n", 
	    fgdc->ptcontac.cntemail);
  fprintf(fp, "      </cntinfo>\n");
  fprintf(fp, "    </ptcontac>\n");
  if (fgdc->browse) {
    fprintf(fp, "    <browse>\n");
    fprintf(fp, "      <browsen>%s</browsen>\n", fgdc->browse->browsen);
    fprintf(fp, "      <browsed>%s</browsed>\n", fgdc->browse->browsed);
    fprintf(fp, "      <browset>%s</browset>\n", fgdc->browse->browset);
    fprintf(fp, "    </browse>\n");
  }
  if (fgdc->datacred)
    fprintf(fp, "    <datacred>%s</datacred>\n", fgdc->datacred);
  if (fgdc->security) {
    fprintf(fp, "    <secinfo>\n");
    fprintf(fp, "      <secsys>%s</secsys>\n", fgdc->security->secsys);
    fprintf(fp, "      <secclass>%s</secclass>\n", fgdc->security->secclass);
    fprintf(fp, "      <sechandl>%s</sechandl>\n", fgdc->security->sechandl);
    fprintf(fp, "    </secinfo>\n");
  }
  fprintf(fp, "  </idinfo>\n");

  // Data Quality Information
  fprintf(fp, "  <dataqual>\n");
  if (fgdc->attraccr) {
    fprintf(fp, "    <attracc>\n");
    fprintf(fp, "      <attraccr>%s</attraccr>\n", fgdc->attraccr);
    fprintf(fp, "    </attracc>\n");
  }
  fprintf(fp, "    <logic>%s</logic>\n", fgdc->logic);
  fprintf(fp, "    <complete>%s</complete>\n", fgdc->complete);
  if (fgdc->horizpar || fgdc->vertaccr) {
    fprintf(fp, "    <posacc>\n");
    if (fgdc->horizpar) {
      fprintf(fp, "      <horizpa>\n");
      fprintf(fp, "        <horizpar>%s</horizpar>\n", fgdc->horizpar);
      fprintf(fp, "      </horizpa>\n");
    }
    if (fgdc->vertaccr) {
      fprintf(fp, "      <vertacc>\n");
      fprintf(fp, "        <vertaccr>%s</vertaccr>\n", fgdc->vertaccr);
      fprintf(fp, "      </vertacc>\n");
    }
    fprintf(fp, "    </posacc>\n");
  }
  fprintf(fp, "    <lineage>\n");
  for (ii=0; ii<fgdc->source_count; ii++) {
    fprintf(fp, "      <srcinfo>\n");
    fprintf(fp, "        <srccite>\n");
    fprintf(fp, "          <citeinfo>\n");
    fprintf(fp, "            <origin>%s</origin>\n", 
	    fgdc->srcinfo[ii].srccite.origin);
    fprintf(fp, "            <pubdate>%s</pubdate>\n", 
	    fgdc->srcinfo[ii].srccite.pubdate);
    fprintf(fp, "            <title>%s</title>\n", 
	    fgdc->srcinfo[ii].srccite.title);
    fprintf(fp, "            <geoform>%s</geoform>\n", 
	    fgdc->srcinfo[ii].srccite.geoform);
    fprintf(fp, "          </citeinfo>\n");
    fprintf(fp, "        </srccite>\n");
    fprintf(fp, "        <typesrc>%s</typesrc>\n", fgdc->srcinfo[ii].typesrc);
    fprintf(fp, "        <srctime>\n");
    fprintf(fp, "          <timeinfo>\n");
    fprintf(fp, "            <sngdate>\n");
    fprintf(fp, "              <caldate>%s</caldate>\n",
	    fgdc->srcinfo[ii].srctime);
    fprintf(fp, "            </sngdate>\n");
    fprintf(fp, "          </timeinfo>\n");
    fprintf(fp, "          <srccurr>%s</srccurr>\n", 
	    fgdc->srcinfo[ii].srccurr);
    fprintf(fp, "        </srctime>\n");
    fprintf(fp, "        <srccitea>%s</srccitea>\n", 
	    fgdc->srcinfo[ii].srccitea);
    fprintf(fp, "        <srccontr>%s</srccontr>\n",
	    fgdc->srcinfo[ii].srccontr);
    fprintf(fp, "      </srcinfo>\n");
  }
  for (ii=0; ii<fgdc->process_count; ii++) {
    fprintf(fp, "      <procstep>\n");
    fprintf(fp, "        <procdesc>%s</procdesc>\n", 
	    fgdc->procstep[ii].procdesc);
    fprintf(fp, "        <procdate>%s</procdate>\n",
	    fgdc->procstep[ii].procdate);
    if (strlen(fgdc->procstep[ii].proccont.cntper) > 2 ||
	strlen(fgdc->procstep[ii].proccont.cntorg) > 2) {
      fprintf(fp, "        <proccont>\n");
      fprintf(fp, "          <cntinfo>\n");
      fprintf(fp, "            <cntperp>\n");
      fprintf(fp, "              <cntper>%s</cntper>\n", 
	      fgdc->procstep[ii].proccont.cntper);
      fprintf(fp, "              <cntorg>%s</cntorg>\n", 
	      fgdc->procstep[ii].proccont.cntorg);
      fprintf(fp, "            </cntperp>\n");
      if (fgdc->procstep[ii].proccont.cntpos &&
	  strlen(fgdc->procstep[ii].proccont.cntpos) > 1)
	fprintf(fp, "            <cntpos>%s</cntpos>\n", 
		fgdc->procstep[ii].proccont.cntpos);
      fprintf(fp, "            <cntaddr>\n");
      fprintf(fp, "              <addrtype>%s</addrtype>\n", 
	      fgdc->procstep[ii].proccont.addrtype);
      fprintf(fp, "              <address>%s</address>\n", 
	      fgdc->procstep[ii].proccont.address);
      fprintf(fp, "              <city>%s</city>\n", 
	      fgdc->procstep[ii].proccont.city);
      fprintf(fp, "              <state>%s</state>\n", 
	      fgdc->procstep[ii].proccont.state);
      fprintf(fp, "              <postal>%s</postal>\n", 
	      fgdc->procstep[ii].proccont.postal);
      if (fgdc->procstep[ii].proccont.country &&
	  strlen(fgdc->procstep[ii].proccont.country) > 1)
	fprintf(fp, "              <country>%s</country>\n", 
		fgdc->procstep[ii].proccont.country);
      fprintf(fp, "            </cntaddr>\n");
      if (fgdc->procstep[ii].proccont.cntvoice &&
	  strlen(fgdc->procstep[ii].proccont.cntvoice) > 1)
	fprintf(fp, "            <cntvoice>%s</cntvoice>\n", 
		fgdc->procstep[ii].proccont.cntvoice);
      if (fgdc->procstep[ii].proccont.cntfax &&
	  strlen(fgdc->procstep[ii].proccont.cntfax) > 1)
	fprintf(fp, "            <cntfax>%s</cntfax>\n", 
		fgdc->procstep[ii].proccont.cntfax);
      if (fgdc->procstep[ii].proccont.cntemail &&
	  strlen(fgdc->procstep[ii].proccont.cntemail) > 1)
	fprintf(fp, "            <cntemail>%s</cntemail>\n", 
		fgdc->procstep[ii].proccont.cntemail);
      fprintf(fp, "          </cntinfo>\n");
      fprintf(fp, "        </proccont>\n");
    }
    fprintf(fp, "      </procstep>\n");
  }
  fprintf(fp, "    </lineage>\n");
  if (strcmp_case(fgdc->ascdscin, "Unknown") != 0 || fgdc->mode) {
    fprintf(fp, "    <acqinfo>\n");
    fprintf(fp, "      <satlocza>0.0</satlocza>\n");
    fprintf(fp, "      <satlocaa>0.0</satlocaa>\n");
    fprintf(fp, "      <solarzea>0.0</solarzea>\n");
    fprintf(fp, "      <solaraza>0.0</solaraza>\n");
    fprintf(fp, "      <clocdrft>0.0</clocdrft>\n");
    if (strcmp_case(fgdc->ascdscin, "ascending") == 0)
      fprintf(fp, "      <ascdscin>0</ascdscin>\n");
    else if (strcmp_case(fgdc->ascdscin, "descending") == 0)
      fprintf(fp, "      <ascdscin>1</ascdscin>\n");
    fprintf(fp, "    </acqinfo>\n");
  }
  fprintf(fp, "  </dataqual>\n");

  // Spatial Data Organization Information
  fprintf(fp, "  <spdoinfo>\n");
  fprintf(fp, "    <direct>%s</direct>\n", fgdc->direct);
  fprintf(fp, "    <rastinfo>\n");
  fprintf(fp, "      <cvaltype>%s</cvaltype>\n", fgdc->cvaltype);
  fprintf(fp, "      <rasttype>%s</rasttype>\n", fgdc->rasttype);
  fprintf(fp, "      <rowcount>%d</rowcount>\n", fgdc->rowcount);
  fprintf(fp, "      <colcount>%d</colcount>\n", fgdc->colcount);
  fprintf(fp, "    </rastinfo>\n");
  fprintf(fp, "  </spdoinfo>\n");

  // Spatial Reference Information
  if (fgdc->projected) {
    meta_projection *proj = fgdc->projection;
    fprintf(fp, "  <spref>\n");
    fprintf(fp, "    <horizsys>\n");
    if (proj->type == LAT_LONG_PSEUDO_PROJECTION) {
      fprintf(fp, "      <geograph>\n");
      fprintf(fp, "        <latres>%.6lf</latres>\n", fabs(proj->perY));
      fprintf(fp, "        <longres>%.6lf</longres>\n", fabs(proj->perX));
      fprintf(fp, "        <geogunit>%s</geogunit>\n", proj->units);
      fprintf(fp, "      </geograph>\n");
    }
    else {
      fprintf(fp, "      <planar>\n");
      if (proj->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
	fprintf(fp, "        <gridsys>\n");
	fprintf(fp, "          <gridsysn>Universal Transverse Mercator"
		"</gridsysn>\n");
	fprintf(fp, "          <utm>\n");
	fprintf(fp, "            <utmzone>%d</utmzone>\n", 
		proj->param.utm.zone);
	fprintf(fp, "            <transmer>\n");
	fprintf(fp, "              <sfctrmer>%.4lf</sfctrmer>\n",
		proj->param.utm.scale_factor);
	fprintf(fp, "              <longcm>%.4lf</longcm>\n",
		proj->param.utm.lon0);
	fprintf(fp, "              <latprjo>%.4lf</latprjo>\n",
		proj->param.utm.lat0);
	fprintf(fp, "              <feast>%.3lf</feast>\n",
		proj->param.utm.false_easting);
	fprintf(fp, "              <fnorth>%.3lf</fnorth>\n",
		proj->param.utm.false_northing);
	fprintf(fp, "            </transmer>\n");
	fprintf(fp, "          </utm>\n");
	fprintf(fp, "        </gridsys>\n");
      }
      else {
	fprintf(fp, "        <mapproj>\n");
	if (proj->type == ALBERS_EQUAL_AREA) {
	  fprintf(fp, "          <mapprojn>Albers Conic Equal Area"
		  "</mapprojn>\n");
	  fprintf(fp, "          <albers>\n");
	  fprintf(fp, "            <stdparll>%.4lf</stdparll>\n", 
		  proj->param.albers.std_parallel1);
	  fprintf(fp, "            <stdparll>%.4lf</stdparll>\n",
		  proj->param.albers.std_parallel2);
	  fprintf(fp, "            <longcm>%.4lf</longcm>\n",
		  proj->param.albers.center_meridian);
	  fprintf(fp, "            <latprjo>%.4lf</latprjo>\n",
		  proj->param.albers.orig_latitude);
	  fprintf(fp, "            <feast>%.3lf</feast>\n",
		  proj->param.albers.false_easting);
	  fprintf(fp, "            <fnorth>%.3lf</fnorth>\n",
		  proj->param.albers.false_northing);
	  fprintf(fp, "          </albers>\n");
	}
	else if (proj->type == EQUI_RECTANGULAR) {
	  fprintf(fp, "          <mapprojn>Equirectangular</mapprojn>\n");
	  fprintf(fp, "          <equirect>\n");
	  fprintf(fp, "            <latprjo>%.4lf</latprjo>\n",
		  proj->param.eqr.orig_latitude);
	  fprintf(fp, "            <longcm>%.4lf</longcm>\n",
		  proj->param.eqr.central_meridian);
	  fprintf(fp, "            <feast>%.3lf</feast>\n",
		  proj->param.eqr.false_easting);
	  fprintf(fp, "            <fnorth>%.3lf</fnorth>\n",
		  proj->param.eqr.false_northing);
	  fprintf(fp, "          </equirect>\n");
	}
	else if (proj->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
	  fprintf(fp, "          <mapprojn>Lambert Azimuthal Equal Area"
		  "</mapprojn>\n");
	  fprintf(fp, "          <lamberta>\n");
	  fprintf(fp, "            <latprjo>%.4lf</latprjo>\n",
		  proj->param.lamaz.center_lat);
	  fprintf(fp, "            <longcm>%.4lf</longcm>\n",
		  proj->param.lamaz.center_lon);
	  fprintf(fp, "            <feast>%.3lf</feast>\n",
		  proj->param.lamaz.false_easting);
	  fprintf(fp, "            <fnorth>%.3lf</fnorth>\n",
		  proj->param.lamaz.false_northing);
	  fprintf(fp, "          </lamberta>\n");
	}
	else if (proj->type == LAMBERT_CONFORMAL_CONIC) {
	  fprintf(fp, "          <mapprojn>Lambert Conformal Conic"
		  "</mapprojn>\n");
	  fprintf(fp, "          <lambertc>\n");
	  fprintf(fp, "            <stdparll>%.4lf</stdparll>\n", 
		  proj->param.lamcc.plat1);
	  fprintf(fp, "            <stdparll>%.4lf</stdparll>\n",
		  proj->param.lamcc.plat2);
	  fprintf(fp, "            <latprjo>%.4lf</latprjo>\n",
		  proj->param.lamcc.lat0);
	  fprintf(fp, "            <longcm>%.4lf</longcm>\n",
		  proj->param.lamcc.lon0);
	  fprintf(fp, "            <feast>%.3lf</feast>\n",
		  proj->param.lamcc.false_easting);
	  fprintf(fp, "            <fnorth>%.3lf</fnorth>\n",
		  proj->param.lamcc.false_northing);
	  fprintf(fp, "          </lambertc>\n");
	}
	else if (proj->type == MERCATOR) {
	  fprintf(fp, "          <mapprojn>Mercator</mapprojn>\n");
	  fprintf(fp, "          <mercator>\n");
	  fprintf(fp, "            <longcm>%.4lf</longcm>\n",
		  proj->param.mer.central_meridian);
	  fprintf(fp, "            <sfprjorg>%.4lf</sfprjorg>\n",
		  proj->param.mer.scale_factor);
	  fprintf(fp, "            <feast>%.3lf</feast>\n",
		  proj->param.mer.false_easting);
	  fprintf(fp, "            <fnorth>%.3lf</fnorth>\n",
		  proj->param.mer.false_northing);
	  fprintf(fp, "          </mercator>\n");
	}
	else if (proj->type == POLAR_STEREOGRAPHIC) {
	  fprintf(fp, "          <mapprojn>Polar Stereographic</mapprojn>\n");
	  fprintf(fp, "          <polarst>\n");
	  fprintf(fp, "            <longcm>%.4lf</longcm>\n",
		  proj->param.ps.slon);
	  fprintf(fp, "            <stdparll>%.4lf</stdparll>\n",
		  proj->param.ps.slat);
	  fprintf(fp, "            <feast>%.3lf</feast>\n",
		  proj->param.ps.false_easting);
	  fprintf(fp, "            <fnorth>%.3lf</fnorth>\n",
		  proj->param.ps.false_northing);
	  fprintf(fp, "          </polarst>\n");
	}
	else
	  fprintf(fp, "          <mapprojn>Unknown Projection</mapprojn>\n");
	fprintf(fp, "        </mapproj>\n");
      }
      fprintf(fp, "        <planci>\n");
      fprintf(fp, "          <plance>%s</plance>\n", fgdc->plance);
      fprintf(fp, "          <coordrep>\n");
      fprintf(fp, "            <absres>%.3lf</absres>\n", fabs(proj->perX));
      fprintf(fp, "            <ordres>%.3lf</ordres>\n", fabs(proj->perY));
      fprintf(fp, "          </coordrep>\n");
      fprintf(fp, "          <plandu>%s</plandu>\n", proj->units);
      fprintf(fp, "        </planci>\n");
      fprintf(fp, "      </planar>\n");
    }
    fprintf(fp, "      <geodetic>\n");

    // Horizontal Datums
    if (proj->datum == EGM96_DATUM)
      fprintf(fp, "        <horizdn>Earth Gravity Model 1996</horizdn>\n");
    else if (proj->datum == ED50_DATUM)
      fprintf(fp, "        <horizdn>European Datum 1950</horizdn>\n");
    else if (proj->datum == ETRS89_DATUM)
      fprintf(fp, "        <horizdn>European Terrestrial Reference System 1989"
	      "</horizdn>\n");
    else if (proj->datum == ETRF89_DATUM)
      fprintf(fp, "        <horizdn>European Terrestrial Reference Frame"
	      "</horizdn>\n");
    else if (proj->datum == ITRF97_DATUM)
      fprintf(fp, "        <horizdn>International Terrestrial Reference Frame "
	      "1997</horizdn>\n");
    else if (proj->datum == NAD27_DATUM)
      fprintf(fp, "        <horizdn>North American Datum of 1927</horizdn>\n");
    else if (proj->datum == NAD83_DATUM)
      fprintf(fp, "        <horizdn>North American Datum of 1983</horizdn>\n");
    else if (proj->datum == WGS72_DATUM)
      fprintf(fp, "        <horizdn>World Geodetic System 1972</horizdn>\n");
    else if (proj->datum == WGS84_DATUM)
      fprintf(fp, "        <horizdn>World Geodetic System 1984</horizdn>\n");
    else if (proj->datum == TOKYO_DATUM)
      fprintf(fp, "        <horizdn>Tokyo</horizdn>\n");
    else if (proj->datum == JGD2000_DATUM)
      fprintf(fp, "        <horizdn>Japanese Geodetic Datum 2000</horizdn>\n");
    else if (proj->datum == HUGHES_DATUM)
      fprintf(fp, "        <horizdn>Hughes</horizdn>\n");
    else
      fprintf(fp, "        <horizdn>Unknown datum</horizdn>\n");

    // Ellipsoids
    if (proj->spheroid == BESSEL_SPHEROID)
      fprintf(fp, "        <ellips>Bessel 1841</ellips>\n");
    else if (proj->spheroid == CLARKE1866_SPHEROID)
      fprintf(fp, "        <ellips>Clarke 1866</ellips>\n");
    else if (proj->spheroid == CLARKE1880_SPHEROID)
      fprintf(fp, "        <ellips>Clarke 1880</ellips>\n");
    else if (proj->spheroid == GEM6_SPHEROID)
      fprintf(fp, "        <ellips>GEM 6</ellips>\n");
    else if (proj->spheroid == GEM10C_SPHEROID)
      fprintf(fp, "        <ellips>GEM 10C</ellips>\n");
    else if (proj->spheroid == GRS1980_SPHEROID)
      fprintf(fp, "        <ellips>Geodetic Reference System 1980</ellips>\n");
    else if (proj->spheroid == INTERNATIONAL1924_SPHEROID)
      fprintf(fp, "        <ellips>International 1924</ellips>\n");
    else if (proj->spheroid == INTERNATIONAL1967_SPHEROID)
      fprintf(fp, "        <ellips>International 1967</ellips>\n");
    else if (proj->spheroid == INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SPHEROID)
      fprintf(fp, "        <ellips>International Terrestrial Reference Frame "
	      "1997</ellips>\n");
    else if (proj->spheroid == WGS66_SPHEROID)
      fprintf(fp, "        <ellips>World Geodetic System 1966</ellips>\n");
    else if (proj->spheroid == WGS72_SPHEROID)
      fprintf(fp, "        <ellips>World Geodetic System 1972</ellips>\n");
    else if (proj->spheroid == WGS84_SPHEROID)
      fprintf(fp, "        <ellips>World Geodetic System 1984</ellips>\n");
    else if (proj->spheroid == HUGHES_SPHEROID)
      fprintf(fp, "        <ellips>Hughes</ellips>\n");
    else if (proj->spheroid == TOKYO_SPHEROID)
      fprintf(fp, "        <ellips>Tokyo</ellips>\n");
    else
      fprintf(fp, "        <ellips>Unknown ellipsoid</ellips>\n");

    fprintf(fp, "        <semiaxis>%.3lf</semiaxis>\n", proj->re_major);
    double denflat = proj->re_major / (proj->re_major - proj->re_minor);
    fprintf(fp, "        <denflat>%.6lf</denflat>\n", denflat);
    fprintf(fp, "      </geodetic>\n");
    fprintf(fp, "    </horizsys>\n");

    /* stub for vertical coordinate system definition
    fprintf(fp, "    <vertdef>\n");
    fprintf(fp, "      <altsys>\n");
    fprintf(fp, "        <altdatum></altdatum>\n");
    fprintf(fp, "        <altunits></altunits>\n");
    fprintf(fp, "      </altsys>\n");
    fprintf(fp, "    </vertdef>\n");
    */

    fprintf(fp, "    <georefin>\n");
    fprintf(fp, "      <georecra>\n");
    fprintf(fp, "        <pixlreso>\n");
    fprintf(fp, "          <coordrep>\n");
    fprintf(fp, "            <absres>%.3lf</absres>\n", fabs(proj->perX));
    fprintf(fp, "            <ordres>%.3lf</ordres>\n", fabs(proj->perY));
    fprintf(fp, "          </coordrep>\n");
    fprintf(fp, "          <plandu>%s</plandu>\n", proj->units);
    fprintf(fp, "        </pixlreso>\n");
    fprintf(fp, "        <gridinit>\n");
    fprintf(fp, "          <grinitx>%.3lf</grinitx>\n", proj->startX);
    fprintf(fp, "          <grinity>%.3lf</grinity>\n", proj->startY);
    fprintf(fp, "        </gridinit>\n");
    fprintf(fp, "        <gridori>\n");
    fprintf(fp, "          <rowdeltx>%.4lf</rowdeltx>\n", fabs(proj->perX));
    fprintf(fp, "          <rowdelty>%.4lf</rowdelty>\n", fabs(proj->perY));
    fprintf(fp, "          <coldeltx>%.4lf</coldeltx>\n", fabs(proj->perX));
    fprintf(fp, "          <coldelty>%.4lf</coldelty>\n", fabs(proj->perY));
    fprintf(fp, "        </gridori>\n");
    fprintf(fp, "        <ptpos>%s</ptpos>\n", fgdc->ptpos);
    fprintf(fp, "        <storord>%s</storord>\n", fgdc->storord);
    fprintf(fp, "      </georecra>\n");
    fprintf(fp, "    </georefin>\n");

    fprintf(fp, "  </spref>\n");
  }

  // Distribution Information
  fprintf(fp, "  <distinfo>\n");
  fprintf(fp, "    <distrib>\n");
  fprintf(fp, "      <cntinfo>\n");
  fprintf(fp, "        <cntorgp>\n");
  fprintf(fp, "          <cntorg>%s</cntorg>\n", fgdc->distrib.cntorg);
  fprintf(fp, "        </cntorgp>\n");
  if (fgdc->distrib.cntpos)
    fprintf(fp, "        <cntpos>%s</cntpos>\n", fgdc->distrib.cntpos);
  fprintf(fp, "        <cntaddr>\n");
  fprintf(fp, "          <addrtype>%s</addrtype>\n", fgdc->distrib.addrtype);
  fprintf(fp, "          <address>%s</address>\n", fgdc->distrib.address);
  fprintf(fp, "          <city>%s</city>\n", fgdc->distrib.city);
  fprintf(fp, "          <state>%s</state>\n", fgdc->distrib.state);
  fprintf(fp, "          <postal>%s</postal>\n", fgdc->distrib.postal);
  if (fgdc->distrib.country)
    fprintf(fp, "          <country>%s</country>\n", fgdc->distrib.country);
  fprintf(fp, "        </cntaddr>\n");
  if (fgdc->distrib.cntvoice)
    fprintf(fp, "        <cntvoice>%s</cntvoice>\n", fgdc->distrib.cntvoice);
  if (fgdc->distrib.cntfax)
    fprintf(fp, "        <cntfax>%s</cntfax>\n", fgdc->distrib.cntfax);
  if (fgdc->distrib.cntemail)
    fprintf(fp, "        <cntemail>%s</cntemail>\n", fgdc->distrib.cntemail);
  fprintf(fp, "      </cntinfo>\n");
  fprintf(fp, "    </distrib>\n");
  fprintf(fp, "    <distliab>%s</distliab>\n", fgdc->distliab);
  fprintf(fp, "    <stdorder>\n");
  fprintf(fp, "      <digform>\n");
  fprintf(fp, "        <digtinfo>\n");
  fprintf(fp, "          <formname>%s</formname>\n", fgdc->formname);
  fprintf(fp, "        </digtinfo>\n");
  fprintf(fp, "        <digtopt>\n");
  fprintf(fp, "          <onlinopt>\n");
  fprintf(fp, "            <computer>\n");
  fprintf(fp, "              <networka>\n");
  fprintf(fp, "                <networkr>%s</networkr>\n", fgdc->networkr);
  fprintf(fp, "              </networka>\n");
  fprintf(fp, "            </computer>\n");
  fprintf(fp, "          </onlinopt>\n");
  fprintf(fp, "        </digtopt>\n");
  fprintf(fp, "      </digform>\n");
  fprintf(fp, "      <fees>%s</fees>\n", fgdc->fees);
  fprintf(fp, "    </stdorder>\n");
  fprintf(fp, "  </distinfo>\n");
  fprintf(fp, "  <metainfo>\n");
  fprintf(fp, "    <metd>%s</metd>\n", fgdc_date());
  fprintf(fp, "    <metc>\n");
  fprintf(fp, "      <cntinfo>\n");
  fprintf(fp, "        <cntorgp>\n");
  fprintf(fp, "          <cntorg>%s</cntorg>\n", fgdc->metc.cntorg);
  fprintf(fp, "        </cntorgp>\n");
  if (fgdc->metc.cntpos)
    fprintf(fp, "        <cntpos>%s</cntpos>\n", fgdc->metc.cntpos);
  fprintf(fp, "        <cntaddr>\n");
  fprintf(fp, "          <addrtype>%s</addrtype>\n", fgdc->metc.addrtype);
  fprintf(fp, "          <address>%s</address>\n", fgdc->metc.address);
  fprintf(fp, "          <city>%s</city>\n", fgdc->metc.city);
  fprintf(fp, "          <state>%s</state>\n", fgdc->metc.state);
  fprintf(fp, "          <postal>%s</postal>\n", fgdc->metc.postal);
  if (fgdc->metc.country)
    fprintf(fp, "          <country>%s</country>\n", fgdc->metc.country);
  fprintf(fp, "        </cntaddr>\n");
  if (fgdc->metc.cntvoice)
    fprintf(fp, "        <cntvoice>%s</cntvoice>\n", fgdc->metc.cntvoice);
  if (fgdc->metc.cntfax)
    fprintf(fp, "        <cntfax>%s</cntfax>\n", fgdc->metc.cntfax);
  if (fgdc->metc.cntemail)
    fprintf(fp, "        <cntemail>%s</cntemail>\n", fgdc->metc.cntemail);
  fprintf(fp, "      </cntinfo>\n");
  fprintf(fp, "    </metc>\n");
  fprintf(fp, "    <metstdn>%s</metstdn>\n", fgdc->metstdn);
  fprintf(fp, "    <metstdv>%s</metstdv>\n", fgdc->metstdv);
  fprintf(fp, "    <metextns>\n");
  fprintf(fp, "      <metprof>%s</metprof>\n", fgdc->metprof);
  fprintf(fp, "    </metextns>\n");
  fprintf(fp, "  </metainfo>\n");
  fprintf(fp, "</metadata>\n");

  FCLOSE(fp);
}

void import_fgdc(const char *inBaseName, const char *configFile,
		 const char *outBaseName)
{
  fgdc_meta *fgdc = get_fgdc_meta(inBaseName);
  if (configFile)
    update_fgdc_meta(fgdc, configFile);
  write_fgdc_meta(fgdc, outBaseName);
}

