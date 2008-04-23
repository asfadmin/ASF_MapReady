#include <ctype.h>
#include "asf.h"
#include "lzFetch.h"
#include "decoder.h"
#include "dateUtil.h"
#include "get_stf_names.h"
#include "spheroids.h"
#include "asf_nan.h"

/* allocation routine for meta_state_vectors */
meta_state_vectors *meta_state_vectors_init(int num_of_vectors);
/* Initializes base meta structure */
meta_parameters *raw_init(void);
void propagate_state(meta_parameters *meta, int nStVec, double data_int);
/* In (local_directory)/fetch_prc_stvec.c */
int fetch_prc_stvec(char *prc_path, ymd_date *seekDate, hms_time *seekTime,
  stateVector *retVec, ymd_date *retDate, hms_time *retTime, int orbit);

void get_spheroid_axes_lengths(char *spheroid, double *re_major, double *re_minor);

/******************************************************************************
 * lzStateTime:
 * Convert LZ-state vector-style date to structure: YYYY-MM-DD hh:mm:ss.ttt*/
void lzStateTime(const char *lzStr,ymd_date *date,hms_time *time)
{
  char month[100];/*3-letter, all-caps description of the month*/
  const char *monthNames[12]=/*List of months of year, in format above*/
    {"JAN","FEB","MAR","APR","MAY","JUN",
     "JUL","AUG","SEP","OCT","NOV","DEC"};
  int monthNo;
  if (6!=sscanf(lzStr,"%d-%[^- ]-%d %d:%d:%lf", &date->day, month, &date->year,
                &time->hour, &time->min, &time->sec))
  { /*We couldn't read the date correctly*/
    asfPrintError("* createMeta_lz:lzStTime couldn't parse LZP state vector date string:\n"
                  "* '%s'!\n"
          "* Month='%s'\n",lzStr,month);
  }
/*Try to figure out the month*/
  monthNo=0;
  while (monthNo<12 && (0!=strncmp(month,monthNames[monthNo],3)))
    monthNo++;
  if (monthNo==12)
    asfPrintError("* createMeta_lz:lzStTime couldn't match month '%s'!\n",month);
  date->month=monthNo+1;
}

/******************************************************************************
 * createMeta_lz:
 * Acquire, figure, & write our fancy dancy .meta and .in files from the
 * .par file. */
void createMeta_lz(bin_state *s, char *inN, char *outN, char *img_timeStr,
                   int nVec,float fd, float fdd, float fddd, int prcflag,
                   char *prcPath)
{
  double clock_ang;
  char *parN, buf[256];
  meta_parameters *meta=raw_init();
  stateVector stVec;/*Source state vector*/
  ymd_date st_date,img_date;
  julian_date st_jdate,img_jdate;
  hms_time st_time,img_time;
  char *st_timeStr;/*Source state vector time, YYYY-MM-DD hh:mm:ss.ttt*/
  int num_vecs;

  parN = (char *) MALLOC(sizeof(char)*512);

  /*Need a SAR block*/
  if (!meta->sar) meta->sar = meta_sar_init();

  /*Open the parameter file*/
  get_stf_metadata_name(inN, &parN);

  /*Find start of current scene*/
  if (img_timeStr == NULL)
    img_timeStr=lzStr(parN,"prep_block.location[0].line_date:",NULL);
  date_dssr2date(img_timeStr,&img_date,&img_time);
  date_ymd2jd(&img_date,&img_jdate);

/*Read a state vector.*/
  if (!prcflag) {
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].x:", nVec);
      stVec.pos.x = lzDouble(parN,buf,NULL);
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].y:", nVec);
      stVec.pos.y = lzDouble(parN,buf,NULL);
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].z:", nVec);
      stVec.pos.z = lzDouble(parN,buf,NULL);
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].xv:", nVec);
      stVec.vel.x = lzDouble(parN,buf,NULL);
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].yv:", nVec);
      stVec.vel.y = lzDouble(parN,buf,NULL);
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].zv:", nVec);
      stVec.vel.z = lzDouble(parN,buf,NULL);
      sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].Date:", nVec);
      st_timeStr=lzStr(parN,buf,NULL);
      parse_ymdTime(st_timeStr, &st_date, &st_time);
  }
  else {
      int orbit = lzInt(parN,"prep_block.OrbitNr:",NULL);
      fetch_prc_stvec(prcPath,&img_date,&img_time,&stVec,&st_date,&st_time,orbit);
  }

  date_ymd2jd(&st_date,&st_jdate);

/*  Convert from GEI to fixed-earth: (NOT needed-- already fixed?)
  gei2fixed(&stVec,utc2gha(st_jdate.year,st_jdate.jd,
    st_time.hour,st_time.min,st_time.sec));*/

/*Create a state vector structure to hold our state vector, with *one* state_loc, and copy it over.*/
  meta->state_vectors = meta_state_vectors_init(1);
  meta->state_vectors->vecs[0].vec = stVec;

/*Set time of image start & state vector, and propagate state vector*/
  meta->state_vectors->year = img_date.year;
  meta->state_vectors->julDay = img_jdate.jd;
  meta->state_vectors->second = date_hms2sec(&img_time);
  meta->state_vectors->vecs[0].time = date2sec(&st_jdate,&st_time)-date2sec(&img_jdate,&img_time);
  num_vecs = 2 + (int)(s->nLines/s->prf)/30.0;
  propagate_state(meta, num_vecs+1, (s->nLines/s->prf)/num_vecs);

/*Figure out satellite look direction*/
  clock_ang = lzDouble(parN,"prep_block.clock_angle:",NULL);
  if (clock_ang  == 90.0)
    s->lookDir='R';
  else if (clock_ang == -90.0)
    s->lookDir='L';
  else
    asfPrintError("* Clock angle in metadata file is %f!\n",clock_ang);

/*Update s-> fields with new state vector*/
  addStateVector(s,&meta->state_vectors->vecs[0].vec);

/*Update fields for which we have decoded header info.*/
  updateMeta(s,meta,inN,1);

/*Put some finishing touches on the updated Meta */
  meta->sar->range_doppler_coefficients[0] = fd;
  meta->sar->range_doppler_coefficients[1] = fdd;
  meta->sar->range_doppler_coefficients[2] = fddd;
  meta->sar->azimuth_doppler_coefficients[0] = fd;
  meta->sar->azimuth_doppler_coefficients[1] = 0.0;
  meta->sar->azimuth_doppler_coefficients[2] = 0.0;

/* Fill in missing metadata */
  meta_general *mg = meta->general;
  meta_sar *ms = meta->sar;
  char *sensor_name;
  char *pol;
  char acquisition_date[256];
  char polarization[256];
  ymd_date acq_date_first, acq_date_last, acq_date;
  hms_time acq_time_first, acq_time_last, acq_time;

  strcpy(mg->basename, inN);
  sensor_name = lzStr(parN, "ss_block.instrument:", NULL);
  if (sensor_name && strlen(sensor_name)) {
      char *s = sensor_name + strlen(sensor_name) - 1;
      while (isspace((int)*s)) s--;
      *++s = '\0';
      strcpy(mg->sensor_name, sensor_name);
  }
  else {
      strcpy(mg->sensor_name, MAGIC_UNSET_STRING);
  }
  char *first_date = lzStr(parN, "prep_block.first_date:", NULL);
  char *last_date  = lzStr(parN, "prep_block.last_date:", NULL);
  parse_ymdTime(first_date, &acq_date_first, &acq_time_first);
  parse_ymdTime(last_date, &acq_date_last, &acq_time_last);
  average_ymdTimes(&acq_date_first, &acq_date_last,
                   &acq_time_first, &acq_time_last,
                   &acq_date,       &acq_time);
  FREE(first_date);
  FREE(last_date);
  sprintf(acquisition_date, "%02d-%s-%04d, %02d:%02d:%02d",
          acq_date.day,
          (acq_date.month == 1)  ? "Jan" :
          (acq_date.month == 2)  ? "Feb" :
          (acq_date.month == 3)  ? "Mar" :
          (acq_date.month == 4)  ? "Apr" :
          (acq_date.month == 5)  ? "May" :
          (acq_date.month == 6)  ? "Jun" :
          (acq_date.month == 7)  ? "Jul" :
          (acq_date.month == 8)  ? "Aug" :
          (acq_date.month == 9)  ? "Sep" :
          (acq_date.month == 10) ? "Oct" :
          (acq_date.month == 11) ? "Nov" :
          (acq_date.month == 12) ? "Dec" : "Unknown",
          acq_date.year,
          acq_time.hour, acq_time.min, (int)acq_time.sec);
  strcpy(mg->acquisition_date, acquisition_date);
  mg->frame = MAGIC_UNSET_INT; // This is set in import_stf()
  // mg->center_longitude and center_latitude are set in import_stf() separately from here
  sprintf(mg->bands, "%s",
          (mg->band_count == 1) ? "01" :
          (mg->band_count == 2) ? "01,02" :
          (mg->band_count == 3) ? "01,02,03" :
          (mg->band_count == 4) ? "01,02,03,04" : MAGIC_UNSET_STRING);
  char *spheroid = lzStr(parN,"prep_block.ellipsoid_name:",NULL);
  if (spheroid && strlen(spheroid)) {
      get_spheroid_axes_lengths(spheroid, &mg->re_major, &mg->re_minor);
  }
  else {
      asfPrintWarning("Could not determine reference spheroid from metadata (.par) file.\n"
              "Defaulting to WGS84...\n");
      mg->re_major = WGS84_SEMIMAJOR;
      mg->re_minor = WGS84_SEMIMAJOR * (1.0 - 1.0/WGS84_INV_FLATTENING);
  }
  int num_pols = lzInt(parN, "prep_block.sensor.beam.PolarizationBlock.NrPolarizations:", NULL);
  int i;
  strcpy(polarization, "");
  for (i = 0; i < num_pols; i++) {
    if (num_pols == 1) {
      sprintf(buf, "prep_block.sensor.beam.PolarizationBlock.Polarization.polarization:");
    }
    else {
      sprintf(buf, "prep_block.sensor.beam.PolarizationBlock.Polarization[%d].polarization:", i);
    }
    pol = lzStr(parN, buf, NULL);
    if (pol && strlen(pol)) {
      if (i == 0) {
          char *s = pol + strlen(pol) - 1;
          while (isspace((int)*s)) s--;
          *++s = '\0';
          strcpy(polarization, pol);
      }
      else {
        sprintf("%s,%s", polarization, pol);
      }
    }
    else {
      strcat(polarization, MAGIC_UNSET_STRING);
    }
    FREE(pol);
  }
  strcpy(ms->polarization, polarization);
  if (strlen(polarization) > 0) strcpy(mg->bands, polarization);
  ms->multilook = 0;
  ms->earth_radius = MAGIC_UNSET_DOUBLE; // (Forces meta_get_earth_radius() to calculate the radius at c.o. scene)
  ms->earth_radius_pp = MAGIC_UNSET_DOUBLE;
  ms->pitch = lzDouble(parN, "prep_block.sensor.ephemeris.Attitude.pitch:", NULL);
  ms->roll = lzDouble(parN, "prep_block.sensor.ephemeris.Attitude.roll:", NULL);
  ms->yaw = lzDouble(parN, "prep_block.sensor.ephemeris.Attitude.yaw:", NULL);
  ms->chirp_rate = lzDouble(parN, "prep_block.sensor.beam.chirp_rate:", NULL);
  ms->pulse_duration = s->pulsedur;
  ms->range_sampling_rate = s->fs;
  ms->earth_radius = meta_get_earth_radius(meta, mg->line_count/2, mg->sample_count/2);

/*Write out ARDOP .in parameter file.*/
  writeARDOPparams(s,outN,fd,fdd,fddd);

/*Write out and free the metadata structure*/
  // NOTE: import_stf() reads the metadata file (later), corrects a few items,
  // then writes it back out.
  meta_write(meta,outN);
  meta_free(meta);

  FREE(sensor_name);
  FREE(parN);
}

/******************************************************************************
 * convertMetadata_lz:
 * Creates ARDOP .in and .fmt files, as well as determining the number of
 * lines in the level-0 file, by reading the granule (.gran) file. */
#include <ctype.h>
bin_state *convertMetadata_lz(char *inName,char *outName,int *numLines,
            readPulseFunc *readNextPulse)
{
  bin_state *s;
  char *lzName;
  int ii;
  char *satName;

  lzName = (char *) MALLOC(sizeof(char)*512);

  get_stf_metadata_name(inName, &lzName);

/*Figure out what kind of SAR data we have; initialize the appropriate decoder*/
  satName=lzStr(lzName,"prep_block.satellite:",NULL);
  /*Trim white space from end of satellite name*/
  ii=strlen(satName)-1;
  while (isspace((int)satName[ii]))
    satName[ii--]=0;

  /*Initialize the appropriate decoder routine*/
  if (0==strncmp(satName,"ERS",3))
    s=ERS_decoder_init(inName,outName,readNextPulse);
  else if (0==strncmp(satName,"JERS",4)) {
    /* JERS data temporarily disabled */
//    asfPrintError("   JERS data ingest currently under development\n");
    /**********************************/
    s=JRS_decoder_init(inName,outName,readNextPulse);
  }
  else if (0==strncmp(satName,"RSAT",4))
    s=RSAT_decoder_init(inName,outName,readNextPulse);
  else
    asfPrintError("   Unrecognized satellite '%s'!\n",satName);

/*Read in essential parameters from granule file.*/
       /** numLines from parfile is often wrong;
  ** will learn the actual number of lines later in main()
  **/
  *numLines = lzInt(lzName,"prep_block.number_lines:",NULL);
  s->nFrames = lzInt(lzName,"prep_block.number_frames:",NULL);

/*Write out ARDOP .fmt parameter file. This must be done before processing
  ** because starting line, window shift, and agc scaling are
  ** perpetually updated via the updateAGC_window function
  ** during processing.*/
  writeARDOPformat(s,outName);

  return s;
}

void get_spheroid_axes_lengths(char *spheroid, double *re_major, double *re_minor)
{
    if (spheroid && strncmp(spheroid, "BESSEL", 6) == 0) {
        spheroid_axes_lengths(BESSEL_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "CLARKE1866", 10) == 0) {
        spheroid_axes_lengths(CLARKE1866_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "CLARKE1880", 10) == 0) {
        spheroid_axes_lengths(CLARKE1880_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "GEM6", 4) == 0) {
        spheroid_axes_lengths(GEM6_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "GEM10C", 6) == 0) {
        spheroid_axes_lengths(GEM10C_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "GRS1980", 7) == 0) {
        spheroid_axes_lengths(GRS1980_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "HUGHES", 6) == 0) {
        spheroid_axes_lengths(HUGHES_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "INTERNATIONAL1924", 17) == 0) {
        spheroid_axes_lengths(INTERNATIONAL1924_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997", 50) == 0) {
        spheroid_axes_lengths(INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SPHEROID, re_major, re_minor);
    }
    else if (spheroid &&
             (strncmp(spheroid, "INTERNATIONAL1967", 17) == 0 || strncmp(spheroid, "INTERNATIONAL", 13) == 0))
    {
        spheroid_axes_lengths(INTERNATIONAL1967_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "WGS66", 5) == 0) {
        spheroid_axes_lengths(WGS66_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "WGS84", 5) == 0) {
        spheroid_axes_lengths(WGS84_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "WGS72", 5) == 0) {
        spheroid_axes_lengths(WGS72_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "TOKYO", 5) == 0) {
        spheroid_axes_lengths(TOKYO_SPHEROID, re_major, re_minor);
    }
    else if (spheroid && strncmp(spheroid, "JGD2000", 7) == 0) {
        spheroid_axes_lengths(JGD2000_SPHEROID, re_major, re_minor);
    }
    else {
        asfPrintWarning("Unrecognized spheroid name string (%s).\n"
                "Cannot determine spheroid axis lengths.  Defaulting\n"
                "to WGS84...", (spheroid && strlen(spheroid)) ? spheroid : "UNKNOWN");
    }
}
