/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "get_ceos_names.h"
#include "meta_init.h"
#include "ceos.h"
#include "dateUtil.h"
#include "meta_init_stVec.h"


meta_state_vectors *meta_state_vectors_init(int vector_count);

/*********************************************************************
 * Convert the given platform position data record & time since ppdr
 * start into a GHA, in degrees.  Note that this is only called if the
 * scene's state vectors are in Inertial coordinates.*/
double ppdr2gha(struct pos_data_rec *ppdr,ceos_description *ceos,double desiredTime);
double ppdr2gha(struct pos_data_rec *ppdr,ceos_description *ceos,double desiredTime)
{
    double degPerSec=360.0/(24.0*60.0*60.0);/*Earth rotation rate, degrees/second.*/
    double start_gha=ppdr->hr_angle;/*GHA at image start.*/

    return start_gha+desiredTime*degPerSec;

/*Old way: using UTC:
    int year,julDay;
    hms_time time;
    year=ppdr->year;
    julDay=ppdr->gmt_day;
    date_sec2hms(ppdr->gmt_sec,&time);
    return utc2gha(year,julDay,time.hour,time.min,time.sec);*/
}

/***************************************************************
 * Get_timeDelta:
 * Return the time difference, in seconds, between the start
 * of the state vectors (in the PPDR) and the start of the image
 * (as described in the DSSR).  Write this time to the given
 * meta_parameters->state_vectors structure.*/
double get_timeDelta(ceos_description *ceos,struct pos_data_rec *ppdr,meta_parameters *meta)
{
    ymd_date imgDate;
    julian_date imgJD,stJD;
    hms_time imgTime,stTime;
    double imgSec,stSec;/*Seconds since 1900 for start of image and start of state vectors*/

/*Read time of *start* of state vectors*/
    stJD.year = (int) ppdr->year;
    stJD.jd = (int) ppdr->gmt_day;
    date_sec2hms(ppdr->gmt_sec,&stTime);
    stSec=date2sec(&stJD,&stTime);

/*Compute the *scene* start time from the CEOS DSSR: */
    /* begin with scene center time */
    if (ceos->facility == BEIJING)
      date_dssr2time(ceos->dssr.inp_sctim,&imgDate,&imgTime);
    else
      date_dssr2date(ceos->dssr.inp_sctim,&imgDate,&imgTime);
    date_ymd2jd(&imgDate,&imgJD);
    imgSec=date2sec(&imgJD,&imgTime);

    if (ceos->processor==FOCUS && ceos->product==CCSD) {
        /* do nothing-- dssr "inp_sctim" already gives scene start time */
    }
    else if (0!=strcmp(ceos->dssr.az_time_first,""))
    { /* e.g., ESA data.  "az_time_first" field gives start of image */
        date_dssr2time(ceos->dssr.az_time_first,&imgDate,&imgTime);
        imgSec=date2sec(&imgJD,&imgTime);
    }
    else if (ceos->facility==EOC)
    {
        imgSec-=ceos->dssr.sc_lin*fabs(meta->sar->azimuth_time_per_pixel);
    }
    else {/*Convert scene center time to scene *start* time, by
       subtracting off the center line # * the time/line */
        imgSec-=ceos->dssr.sc_lin*fabs(meta->sar->azimuth_time_per_pixel);
    }
    /*
    // Complex ALOS data have an additional 1 sec shift
    // Still under investigation: Need word from DQ on this
    if (ceos->facility==EOC && ceos->product==SLC)
      imgSec -= 1.0;
    */

    /*Convert scene center # of seconds back to date/time*/
    sec2date(imgSec,&imgJD,&imgTime);

/*Write image time to meta->state_vectors structure*/
    meta->state_vectors->year   = (int) imgJD.year;
    meta->state_vectors->julDay = (int) imgJD.jd;
    meta->state_vectors->second = date_hms2sec(&imgTime);

/*Return the time between state vector start and image start*/
    return stSec-imgSec;
}


/***************************************************************
 * Ceos_init_stVec:
 * Reads state vectors from given CEOS file, writing them in the
 * appropriate format to SAR parameters structure.*/
void ceos_init_stVec(const char *fName, ceos_description *ceos,
             meta_parameters *meta)
{
  struct pos_data_rec ppdr;

  /*Fetch platform position data record.*/
  get_ppdr(fName,&ppdr);

  // Read the state vectors from the leader data file, adjust coordinate system, etc.
  // and write them to the SAR parameters structures
  ceos_read_stVecs(fName, ceos, meta);

  // For ALOS Palsar orbits only
  // Don't propagate but select nine state vectors around the center for the
  // higher order interpolation scheme
  if (ceos->processor == ALOS_PROC) {

    // Determine closest state vector
    int ii, min;
    double diff = 99999999;
    for (ii=0; ii<meta->state_vectors->vector_count; ii++) {
      if (fabs(meta->state_vectors->vecs[ii].time) < diff) {
	diff = fabs(meta->state_vectors->vecs[ii].time);
	min = ii;
      }
    }

    // Populate a new state vector 
    ymd_date img_ymd;
    julian_date img_jd;
    hms_time img_time;
    img_jd.year = meta->state_vectors->year;
    img_jd.jd   = meta->state_vectors->julDay;
    date_sec2hms(meta->state_vectors->second,&img_time);
    date_jd2ymd(&img_jd, &img_ymd);
    add_time(ii*60, &img_ymd, &img_time);
    date_ymd2jd(&img_ymd, &img_jd);
    meta_state_vectors *new_st = meta_state_vectors_init(9);
    new_st->year   = img_jd.year;
    new_st->julDay = img_jd.jd;
    new_st->second = date_hms2sec(&img_time);
    for (ii=0; ii<9; ii++)
      new_st->vecs[ii] = meta->state_vectors->vecs[ii+min-4];
    FREE(meta->state_vectors);
    meta->state_vectors = new_st;
  }
  // Propagate three state vectors for regular frames
  else if (ceos->processor != PREC && ceos->processor != unknownProcessor) {
      int vector_count=3;
      double data_int = meta->sar->original_line_count / 2
                  * fabs(meta->sar->azimuth_time_per_pixel);
      meta->state_vectors->vecs[0].time = get_timeDelta(ceos, &ppdr, meta);
      if (ceos->processor != PREC && data_int < 360.0) {
          while (fabs(data_int) > 15.0) {
              data_int /= 2;
              vector_count = vector_count*2-1;
          }
      // propagate three state vectors: start, center, end
          propagate_state(meta, vector_count, data_int);
      }
  }
}

void ceos_read_stVecs(const char *fName, ceos_description *ceos, meta_parameters *meta)
{
    int i;
    double timeStart=0.0;/*Time of first state vector, relative to image start.*/
    int areInertial=1;/*Flag: are state vectors in non-rotating frame?*/
    int areInertialVelocity=0;/*Flag: have state vectors not been corrected for non-rotating frame?*/
    int areFixedVelocity=0;/*Flag: were state vectors in fixed-earth velocity; but inertial coordinates?*/
    meta_state_vectors *s;
    struct pos_data_rec ppdr;

    /*Fetch platform position data record.*/
    get_ppdr(fName,&ppdr);

    /*Allocate output record.*/
    meta->state_vectors = meta_state_vectors_init(ppdr.ndata);
    meta->state_vectors->vector_count = ppdr.ndata;
    s = meta->state_vectors;

    /*Determine State Vector Format.*/
    if (0==strncmp(ppdr.ref_coord,"INERTIAL",9))
        areInertial=1;/*Listed as Inertial-- believe it.*/
    if (0==strncmp(ppdr.ref_coord,"EARTH CENTERED ROT",18))
        areInertial=0;/*Listed as rotating-- believe it.*/
    else if (0==strncmp(ppdr.ref_coord,"Earth Centred Rot",17))
        areInertial = 0;
    else if (0 == strncmp(ppdr.ref_coord, "ECR", 3)) {
        areInertial = 0;
    //areInertialVelocity = 1;
    }
    else if (0 == strncmp(ppdr.ref_coord, "EARTH FIXED REFERENCE SYSTEM",28))
      areInertial = 0;
    if (ppdr.hr_angle<=-99.0)
        areInertial=0;/*Bogus GHA-- must be fixed-earth*/

    if (ceos->facility==ASF && ceos->processor==SP2 && ceos->version <=2.50)
        /*The SCANSAR processor made very odd state vectors before 2.51*/
        areInertial=0,areInertialVelocity=1;

    /*Fill output record with inital time.*/
    if (ceos->facility==ASF && ceos->processor!=LZP)
    {/* ASF's state vectors start at the
        same time as the images themselves.*/
/*TAL        timeStart = 1.0; */
        timeStart=get_timeDelta(ceos,&ppdr,meta);
        s->year   = (int) ppdr.year;
        s->julDay = (int) ppdr.gmt_day;
        s->second = ppdr.gmt_sec;
    }
    else
    {/*Most facility's state vectors don't necessarily start
        at the same time as the image.*/
        timeStart=get_timeDelta(ceos,&ppdr,meta);
        s->year   = (int) ppdr.year;
        s->julDay = (int) ppdr.gmt_day;
        s->second = ppdr.gmt_sec;
    }

    /*Fill ouput record with state vectors.*/
    for (i=0; i<s->vector_count; i++) {
        /*Read a state vector from the record, fixing the units.*/
        stateVector st;
        st.pos.x = ppdr.pos_vec[i][0];
        st.pos.y = ppdr.pos_vec[i][1];
        st.pos.z = ppdr.pos_vec[i][2];
	vecScale(&st.pos,get_units(vecMagnitude(st.pos),EXPECTED_POS));
        st.vel.x = ppdr.pos_vec[i][3];
        st.vel.y = ppdr.pos_vec[i][4];
        st.vel.z = ppdr.pos_vec[i][5];
	vecScale(&st.vel,get_units(vecMagnitude(st.vel),EXPECTED_VEL));

        /*Correct for non-rotating frame.*/
        if (areInertial)
            gei2fixed(&st,ppdr2gha(&ppdr,ceos,i*ppdr.data_int));

        /*Perform velocity fixes.*/
        if (areInertialVelocity)
            gei2fixed(&st,0.0);
        else if (areFixedVelocity)
            fixed2gei(&st,0.0);

        /*Write out resulting state vectors.*/
        s->vecs[i].vec = st;
        s->vecs[i].time = timeStart+i*ppdr.data_int;
    }
}

void ceos_init_alos_stVec(const char *fName, ceos_description *ceos, 
			  meta_parameters *meta)
{
  struct pos_data_rec ppdr;

  // Read platform position record
  get_ppdr(fName,&ppdr);

  // Initialize state vector
  int vector_count=3;
  double data_int = meta->sar->original_line_count / 2
    * fabs(meta->sar->azimuth_time_per_pixel);
  while (fabs(data_int) > 15.0) {
    data_int /= 2;
    vector_count = vector_count*2-1;
  }
  meta->state_vectors = meta_state_vectors_init(vector_count);
  meta->state_vectors->vector_count = vector_count;

  // Determine image start time
  ymd_date imgDate;
  julian_date imgJD;
  hms_time imgTime;
  date_dssr2date(ceos->dssr.inp_sctim, &imgDate, &imgTime);
  date_ymd2jd(&imgDate, &imgJD);
  double imgSec = date2sec(&imgJD, &imgTime);
  imgSec -= ceos->dssr.sc_lin*fabs(meta->sar->azimuth_time_per_pixel);
  
  sec2date(imgSec, &imgJD, &imgTime);
  meta->state_vectors->year   = (int) imgJD.year;
  meta->state_vectors->julDay = (int) imgJD.jd;
  meta->state_vectors->second = date_hms2sec(&imgTime);

  // Assign position and velocity for center of image
  int n = (vector_count - 1)/2;
  double timeStart = get_timeDelta(ceos, &ppdr, meta);
  double time = timeStart + n*data_int;
  stateVector st;
  st.pos.x = ppdr.orbit_ele[0];
  st.pos.y = ppdr.orbit_ele[1];
  st.pos.z = ppdr.orbit_ele[2];
  st.vel.x = ppdr.orbit_ele[3];
  st.vel.y = ppdr.orbit_ele[4];
  st.vel.z = ppdr.orbit_ele[5];
  meta->state_vectors->vecs[n].vec = st;
  meta->state_vectors->vecs[n].time = time - timeStart;

  int ii;
  double newTime;
  for (ii=0; ii<n; ii++) {
    newTime = time - (n - ii)*data_int;
    meta->state_vectors->vecs[ii].time = newTime - timeStart;
    meta->state_vectors->vecs[ii].vec = propagate(st, time, newTime);
  }
  for (ii=n+1; ii<vector_count; ii++) {
    newTime = time + (vector_count - n - 1)*data_int;
    meta->state_vectors->vecs[ii].time = newTime - timeStart;
    meta->state_vectors->vecs[ii].vec = propagate(st, time, newTime);
  }
}
