#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <cla.h>
#include <envi.h>
#include <esri.h>
#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>
#include <jpeglib.h>
/*#include <proj_api.h>*/
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>

void
export_as_esri (const char *metadata_file_name,
                const char *image_data_file_name,
                const char *output_file_name)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  char esri_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  esri_header *esri;
  FILE *fp;
  time_t time;
  char t_stamp[15];
  char esri_prj_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  char projection[100], datum[100], spheroid_str[100]="", semimajor[25]="";
  char flattening[25];
  double central_meridian;
  spheroid_type_t spheroid;
  char esri_data_file_name[2 * MAX_IMAGE_NAME_LENGTH];

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  if (   md->general->data_type == BYTE
      || md->general->data_type == INTEGER16
      || md->general->data_type == INTEGER32
      || md->general->data_type == REAL32
      || md->general->data_type == REAL64)
  {
    asfPrintError("Input data cannot be complex.\n");
  }

  create_name (esri_file_name, output_file_name, ".hdr");
  esri = meta2esri (md);

  /* Write ESRI header file */
  fp = FOPEN(esri_file_name, "w");
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&time));
  fprintf(fp, "ESRI header file (created %s)\n\n", t_stamp);
  fprintf(fp, "NROWS            %i\n", esri->nrows);
  fprintf(fp, "NCOLS            %i\n", esri->ncols);
  fprintf(fp, "NBANDS           %i\n", esri->nbands);
  fprintf(fp, "NBITS            %i\n", esri->nbits);
  fprintf(fp, "BYTEORDER        %c\n", esri->byteorder);
  fprintf(fp, "LAYOUT           %s\n", esri->layout);
  fprintf(fp, "SKIPBYTES        %i\n", esri->skipbytes);
  if (md->projection) {
    fprintf(fp, "ULXMAP           %.3f\n", esri->ulxmap);
    fprintf(fp, "ULYMAP           %.3f\n", esri->ulymap);
  }
  fprintf(fp, "XDIM             %.3f\n", esri->xdim);
  fprintf(fp, "YDIM             %.3f\n", esri->ydim);
  /* bandrowbytes, totalrowbytes, bandgapdata and nodata currently not used */
  FCLOSE(fp);

  free (esri);

  /* Write ESRI projection file */
  create_name (esri_prj_file_name, output_file_name, ".prj");

  if (md->projection && md->projection->type < SCANSAR_PROJECTION) {

    if (FLOAT_EQUIVALENT(md->general->re_major,6378137) &&
        FLOAT_EQUIVALENT(md->general->re_minor,6356752.3143))
        spheroid = WGS84_SPHEROID;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378135.0) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356750.519915))
             spheroid = WGS72_SPHEROID;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378206.4) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356583.8))
             spheroid = CLARKE1866_SPHEROID;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378249.145) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356514.86955))
             spheroid = CLARKE1880_SPHEROID;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6377397.155) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356078.9628))
             spheroid = BESSEL_SPHEROID;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378157.5) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356772.2))
             spheroid = INTERNATIONAL1967_SPHEROID;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378137.0) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356752.31414))
             spheroid = GRS1980_SPHEROID;

    switch (spheroid) {
    case WGS84_SPHEROID:
      strcpy(spheroid_str,"WGS84_SPHEROID");
      strcpy(semimajor,"6378137");
      strcpy(flattening,"298.257223563");
      break;
    case WGS72_SPHEROID:
      strcpy(spheroid_str,"WGS72_SPHEROID");
      strcpy(semimajor,"6378135");
      strcpy(flattening,"298.26");
      break;
    case CLARKE1866_SPHEROID:
      strcpy(spheroid_str,"CLARKE1866_SPHEROID");
      strcpy(semimajor,"6378206.4");
      strcpy(flattening,"294.9786982");
      break;
    case CLARKE1880_SPHEROID:
      strcpy(spheroid_str,"CLARKE1880_SPHEROID");
      strcpy(semimajor,"6378249.138");
      strcpy(flattening,"293.466307656");
      break;
/*
    case GRS_1967:
      strcpy(spheroid_str,"GRS_1967_Truncated");
      strcpy(semimajor,"6378160");
      strcpy(flattening,"298.25");
      break;
*/
    case GRS1980_SPHEROID:
      strcpy(spheroid_str,"GRS1980_SPHEROID");
      strcpy(semimajor,"6378137");
      strcpy(flattening,"298.257222101");
      break;
    case BESSEL_SPHEROID:
      strcpy(spheroid_str,"BESSEL_SPHEROID");
      strcpy(semimajor,"6377397.155");
      strcpy(flattening,"299.1528128");
      break;
    case INTERNATIONAL1924_SPHEROID:
      strcpy(spheroid_str,"INTERNATIONAL1924_SPHEROID");
      strcpy(semimajor,"6378388");
      strcpy(flattening,"297");
      break;
    case INTERNATIONAL1967_SPHEROID:
      strcpy(spheroid_str,"INTERNATIONAL1967_SPHEROID");
      strcpy(semimajor,"6378160");
      strcpy(flattening,"298.25");
      break;
    case GEM6_SPHEROID:
      asfPrintError ("Exporting in ESRI format using GEM6 spheroid is not "
		     "implemented.");
      break;
    case GEM10C_SPHEROID:
      asfPrintError ("Exporting in ESRI format using the GEM10C spheroid is "
		     "not implemented.");
      break;
    }

    switch (md->projection->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      switch (spheroid) {
      case WGS84_SPHEROID:
        strcpy(projection, "WGS84_SPHEROID");
        strcpy(datum, "WGS84_SPHEROID");
        break;
      case WGS72_SPHEROID:
        strcpy(projection, "WGS72_SPHEROID");
        strcpy(datum, "WGS72_SPHEROID");
        break;
      case GRS1980_SPHEROID:
        strcpy(projection, "NAD_1983");
        strcpy(datum, "North_American_1983");
        break;
      case CLARKE1866_SPHEROID:
        strcpy(projection, "NAD_1927");
        strcpy(datum, "North_American_1927");
        break;
      case CLARKE1880_SPHEROID:
        /* Not implemented yet.  */
        asfPrintError ("Cannot deal with CLARK_1880 datum.\n");
        break;
      case BESSEL_SPHEROID:
        /* Not implemented yet.  */
        asfPrintError ("Cannot deal with BESSEL_SPHEROID datum.\n");
        break;
      case INTERNATIONAL1924_SPHEROID:
        /* Not implemented yet.  */
        asfPrintError ("Cannot deal with INTERNATIONAL1924_SPHEROID datum.\n");
        break;
      case INTERNATIONAL1967_SPHEROID:
        /* Not implemented yet.  */
        asfPrintError ("Cannot deal with INTERNATIONAL1967_SPHEROID datum.\n");
        break;
/*
      case GRS_1967:
        asfPrintError ("Cannot deal with GRS_1967 datum.\n");
        break;
*/
      default:
        asfPrintError ("Unknown datum.\n");
        break;
      }
      central_meridian = ((6 * abs(md->projection->param.utm.zone)) - 183);
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
              "PROJCS[\"%s_UTM_Zone_%d%c\","
              "GEOGCS[\"GCS_%s\","
              "DATUM[\"D_%s\","
              "SPHEROID[\"%s\",%s,%s]],"
              "PRIMEM[\"Greenwich\",0],"
              "UNIT[\"Degree\",0.017453292519943295]],"
              "PROJECTION[\"Transverse_Mercator\"],"
              "PARAMETER[\"False_Easting\",500000],"
              "PARAMETER[\"False_Northing\",0],"
              "PARAMETER[\"Central_Meridian\",%.0f],"
              "PARAMETER[\"Scale_Factor\",0.9996],"
              "PARAMETER[\"Latitude_Of_Origin\",0],"
              "UNIT[\"Meter\",1]]",
              projection, md->projection->param.utm.zone, md->projection->hem, datum,
              datum, spheroid_str, semimajor, flattening, central_meridian);
      FCLOSE(fp);
      break;
    case POLAR_STEREOGRAPHIC:
      if (md->projection->hem == 'N') strcpy(projection, "North_Pole_Stereographic");
      else if (md->projection->hem == 'S') strcpy(projection, "South_Pole_Stereographic");
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
              "PROJCS[\"%s\","
              "GEOGCS[\"GCS_WGS84_SPHEROID\","
              "DATUM[\"D_WGS84_SPHEROID\","
              "SPHEROID[\"%s\",%s,%s]],"
              "PRIMEM[\"Greenwich\",0],"
              "UNIT[\"Degree\",0.017453292519943295]],"
              "PROJECTION[\"Stereographic\"],"
              "PARAMETER[\"False_Easting\",0],"
              "PARAMETER[\"False_Northing\",0],"
              "PARAMETER[\"Central_Meridian\",%.0f],"
              "PARAMETER[\"Scale_Factor\",1],"
              "PARAMETER[\"Latitude_Of_Origin\",%.0f],"
              "UNIT[\"Meter\",1]]",
              projection, spheroid_str, semimajor, flattening,
              md->projection->param.ps.slon, md->projection->param.ps.slat);
      FCLOSE(fp);
      break;
    case ALBERS_EQUAL_AREA:
      if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,25) &&
          FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,20) &&
          FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,-23) &&
          FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,0)) {
        strcpy(projection, "Africa_Albers_Equal_Area_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-154) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,55) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,65) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,50)) {
        strcpy(projection, "Alaska_Albers_Equal_Area_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,95) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,15) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,65) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,30)) {
        strcpy(projection, "Asia_North_Albers_Equal_Area_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,125) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,7) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,-32) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,-15)) {
        strcpy(projection, "Asia_South_Albers_Equal_Area_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,50) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,70) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,40)) {
        strcpy(projection, "Canada_Albers_Equal_Area_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,10) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,43) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,62) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,30)) {
        strcpy(projection, "Europe_Albers_Equal_Area_Conic");
        strcpy(datum, "European_1950");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-157) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,8) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,18) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,13)) {
        strcpy(projection, "Hawaii_Albers_Equal_Area_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,20) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,60) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,40)) {
        strcpy(projection, "North_America_Albers_Equal_Area_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-60) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,-5) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,-42) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,-32)) {
        strcpy(projection, "South_America_Albers_Equal_Area_Conic");
        strcpy(datum, "South_American_1969");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,29.5) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,45.5) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,37.5)) {
        strcpy(projection, "USA_Contiguous_Albers_Equal_Area_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,29.5) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,45.5) &&
               FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,23.0)) {
        strcpy(projection, "USA_Contiguous_Albers_Equal_Area_Conic_USGS_version");
        strcpy(datum, "North_American_1983");
      }
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
              "PROJCS[\"%s\","
              "GEOGCS[\"GCS_%s\","
              "DATUM[\"D_%s\","
              "SPHEROID[\"%s\",%s,%s]],"
              "PRIMEM[\"Greenwich\",0],"
              "UNIT[\"Degree\",0.0174532925199432955]],"
              "PROJECTION[\"Albers\"],"
              "PARAMETER[\"False_Easting\",0],"
              "PARAMETER[\"False_Northing\",0],"
              "PARAMETER[\"Central_Meridian\",%.1f],"
              "PARAMETER[\"Standard_Parallel_1\",%.1f],"
              "PARAMETER[\"Standard_Parallel_2\",%.1f],"
              "PARAMETER[\"Latitude_Of_Origin\",%.1f],"
              "UNIT[\"Meter\",1]],",
              projection, datum, datum, spheroid_str, semimajor, flattening,
              md->projection->param.albers.center_meridian,
              md->projection->param.albers.std_parallel1,
              md->projection->param.albers.std_parallel2,
              md->projection->param.albers.orig_latitude);
      FCLOSE(fp);
     break;
    case LAMBERT_CONFORMAL_CONIC:
      if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,25) &&
          FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,20) &&
          FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,-23) &&
          FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,0)) {
        strcpy(projection, "Africa_Lambert_Conformal_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,105) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,30) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,62) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,0)) {
        strcpy(projection, "Asia_Lambert_Conformal_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,95) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,15) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,65) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,30)) {
        strcpy(projection, "Asia_North_Lambert_Conformal_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,125) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,7) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,-32) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,-15)) {
        strcpy(projection, "Asia_South_Albers_Equal_Area_Conic");
        strcpy(datum, "WGS84_SPHEROID");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,50) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,70) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,40)) {
        strcpy(projection, "Canada_Lambert_Conformal_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,10) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,43) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,62) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,30)) {
        strcpy(projection, "Europe_Lambert_Conformal_Conic");
        strcpy(datum, "European_1950");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,20) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,60) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,40)) {
        strcpy(projection, "North_America_Lambert_Conformal_Conic");
        strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-60) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,-5) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,-42) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,-32)) {
        strcpy(projection, "South_America_Lambert_Conformal_Conic");
        strcpy(datum, "South_American_1969");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-96) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,33) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,45) &&
               FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,39)) {
        strcpy(projection, "USA_Contiguous_Lambert_Conformal_Conic");
        strcpy(datum, "North_American_1983");
      }
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
              "PROJCS[\"%s\","
              "GEOGCS[\"GCS_%s\","
              "DATUM[\"D_%s\","
              "SPHEROID[\"%s\",%s,%s]],"
              "PRIMEM[\"Greenwich\",0],"
              "UNIT[\"Degree\",0.0174532925199432955]],"
              "PROJECTION[\"Lambert_Conformal_Conic\"],"
              "PARAMETER[\"False_Easting\",0],"
              "PARAMETER[\"False_Northing\",0],"
              "PARAMETER[\"Central_Meridian\",%.0f],"
              "PARAMETER[\"Standard_Parallel_1\",%.0f],"
              "PARAMETER[\"Standard_Parallel_2\",%.0f],"
              "PARAMETER[\"Latitude_Of_Origin\",%.0f],"
              "UNIT[\"Meter\",1]]",
              projection, datum, datum, spheroid_str, semimajor, flattening,
              md->projection->param.lamcc.lon0,
              md->projection->param.lamcc.plat1,
              md->projection->param.lamcc.plat2,
              md->projection->param.lamcc.lat0);
      FCLOSE(fp);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      asfPrintError ("Exporting pseudoprojected images in ESRI format is not "
		     "implemented.");
      break;
    case STATE_PLANE:
      asfPrintError ("Exporting state plane images in ESRI format is not "
		     "implemented.");
      break;
    case SCANSAR_PROJECTION:
      asfPrintError ("Exporting scansar projected images probably doesn't "
		     "make sense and isn't supported.");
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      asfPrintError ("Exporting pseudoprojected images in ESRI format is not "
		     "implemented.");
      break;
    }
  }

  meta_free(md);

  /* Write ESRI data file */
  strcpy (esri_data_file_name, output_file_name);
  strcat (esri_data_file_name, ".bil");
  fileCopy(image_data_file_name, esri_data_file_name);
}
