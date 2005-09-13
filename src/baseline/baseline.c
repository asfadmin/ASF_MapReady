#include "asf.h"
#include "asf_meta.h"
#include "geolocate.h"
#include "dateUtil.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s  <list>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   list   Name of baseline list file.");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s calculates interferometric baselines for a list of baseline files.\n",
         name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char file[255]="", inFile[255], outFile[255], *listFile;
  char buffer[1000], mode[10], time1[30], time2[30];
  int frame, orbit1, orbit2, beginYear, endYear, sign;
  int sequence1, sequence2;
  FILE *fpIn, *fpOut, *fpList;
  stateVector stVec1, stVec2;
  double range, doppler, Bp, Bn, dt, latitude, longitude;
  julian_date jd1, jd2;
  hms_time hms1, hms2;
  
  double lat,phi,earthRadius;
  vector target,up,relPos;
  vector upBeam,alongBeam,beamNormal;
  GEOLOCATE_REC *g;
  
  quietflag=1;

  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  
  listFile = argv[1];
  fpList = FOPEN(listFile, "r");  
  while (fgets(buffer, 1000, fpList)) {
    strncpy(file, buffer, 9);
    sprintf(outFile, "%s.out", file);
    sprintf(inFile, "%s.in", file);
    fpIn = FOPEN(inFile, "r");
    fpOut = FOPEN(outFile, "w");
    
    while (fgets(buffer, 1000, fpIn)) {
      sscanf(buffer, "%s %d %d %d %s %lf %lf %lf %lf %lf %lf %lf %lf "
	     "%d %d %s %lf %lf %lf %lf %lf %lf %lf %lf",
	     mode, &frame, &orbit1, &sequence1, time1, 
	     &stVec1.pos.x, &stVec1.pos.y, &stVec1.pos.z, 
	     &stVec1.vel.x, &stVec1.vel.y, &stVec1.vel.z, 
	     &range, &doppler, &orbit2, &sequence2, time2, 
	     &stVec2.pos.x, &stVec2.pos.y, &stVec2.pos.z, 
	     &stVec2.vel.x, &stVec2.vel.y, &stVec2.vel.z,
	     &latitude, &longitude);
      
      /* Scale state vector position to [m] and velocity to [m/s] */ 
      vecScale(&stVec1.pos, 1000);
      vecScale(&stVec2.pos, 1000);
      
      /* Get inertial state vectors into earth-fixed. */
      sscanf(time1, "%4d-%3dT%2d:%2d:%lf", 
	     &jd1.year, &jd1.jd, &hms1.hour, &hms1.min, &hms1.sec);
      gei2fixed(&stVec1, utc2gha(jd1.year, jd1.jd, hms1.hour, hms1.min, hms1.sec));
      sscanf(time2, "%4d-%3dT%2d:%2d:%lf", 
	     &jd2.year, &jd2.jd, &hms2.hour, &hms2.min, &hms2.sec);
      gei2fixed(&stVec2, utc2gha(jd2.year, jd2.jd, hms2.hour, hms2.min, hms2.sec));
    
      /*Target is the patch of ground at beam center.*/
      /*Initialize the transformation.*/
      g=init_geolocate(&stVec1);
      g->lambda = 0.0565646;
      getLoc(g,range,doppler,
	     &lat,&phi,&earthRadius);
      free_geolocate(g);
      sph2cart(earthRadius,lat,phi,&target);
      
      /*Create beam plane unit vectors.*/
      vecSub(stVec1.pos,target,&alongBeam);
      vecNormalize(&alongBeam);
      up=stVec1.pos;
      vecNormalize(&up);
      vecCross(up,alongBeam,&beamNormal);
      vecNormalize(&beamNormal);
      vecCross(alongBeam,beamNormal,&upBeam);
      vecNormalize(&upBeam);
      
      /*Now we have the second satellite sitting in the plane of the first's beam,
	so we just separate that position into components along-beam and across-beam,
	and return.*/
      vecSub(stVec2.pos,stVec1.pos,&relPos);
      Bp=vecDot(alongBeam,relPos);
      Bn=vecDot(upBeam,relPos);

      /* Calculate temporal baseline */
      dt = 0;
      if (jd1.year < jd2.year) {
        beginYear = jd1.year;
        endYear = jd2.year;
        sign = 1;
      }
      else {
        beginYear = jd2.year;
        endYear = jd1.year;
        sign = -1;
      }
      while (beginYear<endYear)
        dt += date_getDaysInYear(beginYear++)*sign;
      dt += jd2.jd-jd1.jd;
      
      fprintf(fpOut, "%s\t%d\t%d\t%d\t%s\t%d\t%d\t%s\t%.0f\t%.0f\t%.0f\t%.6f\t%.6f\n", 
	      mode, frame, orbit1, sequence1, time1, orbit2, sequence2, time2, 
	      Bp, Bn, fabs(dt), latitude, longitude);
    }
    
    FCLOSE(fpIn);
    FCLOSE(fpOut);
  }
  FCLOSE(fpList);
  return(0);
}
