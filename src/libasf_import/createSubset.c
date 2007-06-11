/******************************************************************************
createSubset.c
  Contains all subroutines to determine the parameters needed to cut out
  a subset corresponding to a given latitude constraint
*/

#include "asf.h"
#include "lzFetch.h"
#include "decoder.h"
#include "dateUtil.h"
#include "string.h"
#include "ctype.h"
#include "get_stf_names.h"

stateVector propagate(stateVector source,double sourceSec,double destSec);


/****************************
time_range2Doppler:
  Estimates the Doppler for a given time and slant range using
  the two-dimensional Doppler function given in the par file
*/

static void
yax2bxc(float x_vec[],float y_vec[],int n,float *a,float *b,float *c)
{
  double x1, x2, x3, x4;         /* Sum of x, x^2, x^3, x^4 */
  double y1, yx, yx2;            /* Sum of y, y*x, y*x^2    */
  double d1, d2, d3, d4, d5;     /* Intermediate Values     */
  double t1, t2, t3;             /* Equation Solutions      */
  int i;

  /* Calculate all of the first order sums */
  x1 = x2 = x3 = x4 = y1 = yx = yx2 = 0.0;

  for (i=0; i<n; i++) {
    x1  += x_vec[i];
    x2  += x_vec[i] * x_vec[i];
    x3  += x_vec[i] * x_vec[i] * x_vec[i];
    x4  += x_vec[i] * x_vec[i] * x_vec[i] * x_vec[i];
    y1  += y_vec[i];
    yx  += y_vec[i] * x_vec[i];
    yx2 += y_vec[i] * x_vec[i] * x_vec[i];
  }

  d1 = n*x2  - x1*x1;
  d2 = n*x3  - x1*x2;
  d3 = n*x4  - x2*x2;
  d4 = n*yx  - x1*y1;
  d5 = n*yx2 - x2*y1;

  t1 = (d1*d5 - d2*d4) / (d1*d3 - d2*d2);
  t2 = (d4 - d2*t1) / d1;
  t3 = (y1 - x2*t1 - x1*t2) / n;

  *a = t1;
  *b = t2;
  *c = t3;

  return;
}


void time_range2Doppler(char *inN, double time, double slantFirst, float *fd, float *fdd, float *fddd)
{
  float *dopVec,*sampVec,currSamp=0.0;
  double aVal[3][4],range,t_ref,r_ref,xPix;
  int i,j,k,numCoef1,numCoef2,samples,nSteps=100;
  char parN[255],buf[255];

  /* Allocate memory for sample & Doppler vectors (nSteps)*/
  dopVec = (float *)MALLOC(sizeof(float)*nSteps);
  sampVec = (float *)MALLOC(sizeof(float)*nSteps);

  /* Read parameters out of par file */
  get_stf_metadata_name(inN, parN);

  samples = lzInt(parN, "prep_block.sensor.beam.nr_of_samples:", NULL);
  xPix = 1 / lzDouble(parN, "prep_block.sensor.beam.sampling_freq:", NULL)*(speedOfLight/2.0);
  r_ref = lzDouble(parN, "prep_block.sensor.beam.DopplerCentroidParameters.doppler_centroid_coefficients."
                   "reference_first_dimension:", NULL);
  t_ref = lzDouble(parN, "prep_block.sensor.beam.DopplerCentroidParameters.doppler_centroid_coefficients."
                   "reference_second_dimension:", NULL);
  numCoef1 = lzInt(parN, "prep_block.sensor.beam.DopplerCentroidParameters.doppler_centroid_coefficients."
                   "number_of_coefficients_first_dimension:", NULL);
  numCoef2 = lzInt(parN, "prep_block.sensor.beam.DopplerCentroidParameters.doppler_centroid_coefficients."
                   "number_of_coefficients_second_dimension:", NULL);

  for (i=0; i<numCoef2; i++) {
    for (j=0; j<numCoef1; j++) {
      sprintf(buf, "prep_block.sensor.beam.DopplerCentroidParameters.doppler_centroid_coefficients.a%d%d:",i,j);
      aVal[i][j] = lzDouble(parN, buf, NULL);
    }
  }

  /* Calculate slant range from near to far, with nSteps along the range */
  for(k=0; k<nSteps; k++) {
    sampVec[k] = currSamp;
    range = slantFirst + (currSamp*xPix);
    dopVec[k] = 0.0;
    for(i=0; i<numCoef2; i++) {
      for(j=0; j<numCoef1; j++) {
        dopVec[k] += aVal[i][j] * pow((time-t_ref), i) * pow((range-r_ref), j);
      }
    }
    currSamp += samples/nSteps;
  }

  /* Quadratic Regression */
  yax2bxc(sampVec, dopVec, nSteps, fddd, fdd, fd);
}


void estimateDoppler(char *inN, float *fd, float *fdd, float *fddd)
{
  hms_time hmsTime;
  ymd_date ymdDate;
  julian_date jDate;
  char parN[255],buf[255],*timeStr=NULL;
  int i=0,done=0;
  double firstDate,lastDate,time,centerTime,oldTime,newTime,range;

  get_stf_metadata_name(inN, parN);

  /* Determine center time for swath */
  timeStr = lzStr(parN, "prep_block.first_date:", NULL);
  date_dssr2date(timeStr,&ymdDate,&hmsTime);
  firstDate = date_hms2sec(&hmsTime);
  timeStr = lzStr(parN, "prep_block.last_date:", NULL);
  date_dssr2date(timeStr,&ymdDate,&hmsTime);
  lastDate = date_hms2sec(&hmsTime);
  time = (lastDate - firstDate)/2 + firstDate;
  date_ymd2jd(&ymdDate, &jDate);
  centerTime = time + (double)(date_getMJD(&jDate) * 3600 *24);

  /* Find closest location block for center time to get a range */
  get_stf_metadata_name(inN, parN);
  oldTime = time;
  while (!done) {
    sprintf(buf, "prep_block.location[%d].line_date:", i);
    timeStr = lzStr(parN, buf, NULL);
    date_dssr2date(timeStr,&ymdDate,&hmsTime);
    newTime = date_hms2sec(&hmsTime);
    if (newTime>time && oldTime<time) done=1;
    else oldTime = newTime;
    i++;
  }
  sprintf(buf, "prep_block.location[%d].near_range:", i-2);
  range = lzDouble(parN, buf, NULL);

  /* Estimate Doppler */
  time_range2Doppler(inN, centerTime, range, fd, fdd, fddd);

}

/*****************************
lat2stVec:
  Propagates the closest state vector to the starting time
  of the closest location block to a given latitude and
  determines the Doppler for the state vector time.
  Returns a state vector for the location block starting time
  as well as the Doppler and range.
*/

void lat2stVec(char *inN, float lat, stateVector *outVec, double *seconds, int *nLoc, int *nVec)
{
  hms_time loc_time,vec_time;
  ymd_date loc_date,vec_date;
  stateVector stVec,locVec;
  int i=0,n=1,done=0;
  float oldLat,newLat;
  char parN[255],buf[255],*loc_timeStr,*vec_timeStr,*values;
  double old_delta,delta,new_delta,loc_sec,vec_sec;

  /* Find closest location block for given latitude */
  get_stf_metadata_name(inN, parN);
  oldLat = lat;
  while (!done) {
    sprintf(buf, "prep_block.location[%d].line_date:", i);
    loc_timeStr = lzStr(parN, buf, NULL);
    date_dssr2date(loc_timeStr,&loc_date,&loc_time);
    loc_sec = date_hms2sec(&loc_time);
    sprintf(buf,"prep_block.location[%d].first_pixel_ll:", i);
    values = lzStr(parN, buf, NULL);
    sscanf(values, "%f", &newLat);
    if ((newLat<lat && oldLat>lat) || (newLat>lat && oldLat<lat)) done=1;
    else oldLat = newLat;
    i++;
  }
  sprintf(buf, "prep_block.location[%d].line_date:", i-2);
  loc_timeStr = lzStr(parN, buf, NULL);
  date_dssr2date(loc_timeStr,&loc_date,&loc_time);
  loc_sec = date_hms2sec(&loc_time);

  /* Find closest state vector to the location block */
  done = 0;
  old_delta = delta = 1000; /* some larger number for initialization */
  while (!done) {
    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].Date:", n);
    vec_timeStr = lzStr(parN, buf, NULL);
    parse_ymdTime(vec_timeStr,&vec_date,&vec_time);
    vec_sec = date_hms2sec(&vec_time);
    new_delta = fabs(vec_sec - loc_sec);
    if ((new_delta>delta) && (old_delta>delta)) done=1;
    old_delta = delta;
    delta = new_delta;
    n++;
  }
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].Date:", n-2);
  vec_timeStr = lzStr(parN, buf, NULL);
  parse_ymdTime(vec_timeStr,&vec_date,&vec_time);
  vec_sec = date_hms2sec(&vec_time);
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].x:", n-2);
  stVec.pos.x = lzDouble(parN, buf, NULL);
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].y:", n-2);
  stVec.pos.y = lzDouble(parN, buf, NULL);
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].z:", n-2);
  stVec.pos.z = lzDouble(parN, buf, NULL);
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].xv:", n-2);
  stVec.vel.x = lzDouble(parN, buf, NULL);
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].yv:", n-2);
  stVec.vel.y = lzDouble(parN, buf, NULL);
  sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].zv:", n-2);
  stVec.vel.z = lzDouble(parN, buf, NULL);

  /* Propagate state vector to starting time of location block */
  locVec = propagate(stVec, vec_sec, loc_sec);
  *outVec = locVec;
  *seconds = loc_sec;
  *nLoc = i-2;
  *nVec = n-2;
}


/*****************************
lat2time:
  Propagates the state vector in a time loop
  to a given latitude
*/

void lat2time(stateVector locVec, double loc_sec, double targetLat,
              double range, double doppler, double *targetTime)
{
  vector relPos,sarPos,targetPos;
  stateVector outVec;
  double time,slantRange,look,yaw,lat,lat_geod,re2,rp2,a,b,c,d,e,old;
  int done=0,check=0;
  float inc=1.0;
  GEOLOCATE_REC *g;

  g = init_geolocate(&locVec);
  re2 = g->re*g->re;
  rp2 = g->rp*g->rp;
  time = loc_sec-inc;
  slantRange = range;
  g->earth_radius = (g->re*g->rp) /
    sqrt(rp2*cos(targetLat)*cos(targetLat) + re2*sin(targetLat)*sin(targetLat));

  while (!done) {

    /* Look angle and yaw for Doppler and range */
    getLookYaw(g,slantRange,doppler,&look,&yaw);

    /* Propagate state vector to new time */
    outVec = propagate(locVec, loc_sec, time);

    /* Latitude for state vector */
    lat=atan(outVec.pos.z/
                   sqrt(outVec.pos.x*outVec.pos.x+outVec.pos.y*outVec.pos.y));

    /* Geocentric latitude for image time */
    g = init_geolocate(&outVec);
    sarPos = g->stVec.pos;
    relPos.x = sin(yaw);
    relPos.y = -sin(look)*cos(yaw);
    relPos.z = -cos(look)*cos(yaw);
    vecMul(g->look_matrix,relPos,&relPos);
    a = (relPos.x*relPos.x+relPos.y*relPos.y)/re2 + relPos.z*relPos.z/rp2;
    b = 2.0*((sarPos.x*relPos.x + sarPos.y*relPos.y)/re2 + sarPos.z*relPos.z/rp2);
    c = (sarPos.x*sarPos.x+sarPos.y*sarPos.y)/re2 + sarPos.z*sarPos.z/rp2 - 1.0;
    d = (b*b-4.0*a*c);
    slantRange =(-b - sqrt(d))/(2.0*a);
    vecScale(&relPos,slantRange);
    vecAdd(sarPos,relPos,&targetPos);
    lat=atan2(targetPos.z, 
	      sqrt(targetPos.x*targetPos.x+targetPos.y*targetPos.y));
    g->earth_radius = (g->re*g->rp) /
      sqrt(rp2*cos(lat)*cos(lat) + re2*sin(lat)*sin(lat));

    /* Geodetic latitude for image time */
    e = (re2-rp2)/re2;
    lat_geod = atan(tan(lat)/sqrt(1-e));

    if (check && (old / (lat_geod*180/PI-targetLat)) < 0) inc /= -2;
    if (fabs(lat_geod*180/PI-targetLat) < 0.00001) done=1;

    old = lat_geod*180/PI-targetLat;
    check = 1;
    time += inc;
  }

  *targetTime = time;
}


/*****************************
createSubset:
  Returns the start and end line
  to the corresponding latitude constraints
*/

void createSubset(char *inN, float lowerLat, float upperLat, long *imgStart, long *imgEnd, char *imgTimeStr, int *nVec,
      float *fd, float *fdd, float *fddd)
{
  hms_time hmsTime;
  ymd_date ymdDate;
  julian_date jDate;
  char parN[255],buf[255],*locTimeStr,ymdStr[10],hmsStr[10],tmp[10]="";
  int i,nLoc,centerVec,upperVec,lowerVec;
  float centerLat;
  stateVector locVec;
  double centerTime,upperTime,lowerTime,imgTime,loc_sec,range,prf,azPixTime,line;

  centerLat = lowerLat + (upperLat-lowerLat)/2;

  /* Determine image start time and calculate azimuth pixel time */
  get_stf_metadata_name(inN, parN);
  locTimeStr = lzStr(parN, "prep_block.first_date:", NULL);
  date_dssr2date(locTimeStr,&ymdDate,&hmsTime);
  imgTime = date_hms2sec(&hmsTime);
  prf = lzDouble(parN, "prep_block.sensor.beam.PRF:", NULL);
  azPixTime = 1 / prf;

  /* Propagate a state vector to center latitude */
  lat2stVec(inN, centerLat, &locVec, &loc_sec, &nLoc, &centerVec);

  /* Determine Doppler for time of location block */
  sprintf(buf, "prep_block.location[%d].near_range:", nLoc);
  range = lzDouble(parN, buf, NULL);
  sprintf(buf, "prep_block.location[%d].line_date:", nLoc);
  locTimeStr = lzStr(parN, buf, NULL);
  date_dssr2date(locTimeStr,&ymdDate,&hmsTime);
  centerTime = date_hms2sec(&hmsTime);
  date_ymd2jd(&ymdDate, &jDate);
  centerTime += (double)(date_getMJD(&jDate) * 3600 * 24);
  time_range2Doppler(inN, centerTime, range, fd, fdd, fddd);

  /* Calculate time for the center latitude of the subset */
  lat2time(locVec, loc_sec, centerLat, range, *fd, &centerTime);
  line = (centerTime - imgTime) / azPixTime;
  date_ymd2jd(&ymdDate, &jDate);
  centerTime += (double)(date_getMJD(&jDate) * 3600 * 24);
  time_range2Doppler(inN, centerTime, range, fd, fdd, fddd);

  /* Propagate a state vector to upper latitude */
  lat2stVec(inN, upperLat, &locVec, &loc_sec, &nLoc, &upperVec);

  /* Calculate time for the upper latitude of the subset */
  lat2time(locVec, loc_sec, upperLat, range, *fd, &upperTime);
  line = (upperTime - imgTime) / azPixTime;
  *imgStart = (long)line;

  /* Propagate a state vector to lower latitude */
  lat2stVec(inN, lowerLat, &locVec, &loc_sec, &nLoc, &lowerVec);

  /* Calculate time for the lower latitude of the subset */
  lat2time(locVec, loc_sec, lowerLat, range, *fd, &lowerTime);
  line = (lowerTime - imgTime) / azPixTime;
  *imgEnd = (long)line;

  /* Calculate starting time string and state vector */
  imgTime = (lowerTime < upperTime) ? lowerTime : upperTime;
  date_sec2hms(imgTime, &hmsTime);
  date_printDate(&ymdDate,'\0',ymdStr);
  date_printTime(&hmsTime,3,'\0',hmsStr);
  for (i=0; i<strlen(hmsStr); i++)
    if (isdigit(hmsStr[i])) sprintf(tmp, "%s%c", tmp, hmsStr[i]);
  sprintf(imgTimeStr, "%s%s", ymdStr, tmp);

  *nVec = (lowerVec < upperVec) ? lowerVec : upperVec;

  /* Swap start line and end line if necessary */
  if (*imgStart > *imgEnd) {
    line = *imgStart;
    *imgStart = *imgEnd;
    *imgEnd = line;
    line = lowerTime;
    lowerTime = upperTime;
    upperTime = line;
  }
  asfPrintStatus(logbuf,"   Starting line of subset: %ld (%lf)\n", 
		 *imgStart, upperTime);
  asfPrintStatus(logbuf, "   End line of subset: %ld (%lf)\n", *imgEnd, lowerTime);

}
