#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\nUSAGE:   %s <tle> <output>\n",name);
  printf("\nREQUIRED ARGUMENTS:\n");
  printf("   tle    Two Line Element file.\n");
  printf("   output Output file containing true equatorial coordinates");
  printf("\n\nDESCRIPTION:\n");
  printf("   %s converts two line elements into true equatorial coordinates.",name);
  printf("\nVersion %.2f, ASF SAR Tools\n\n",VERSION);
  exit(1);
}

int main(int argc, char **argv)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  char infile[255], outfile[255], line[512], tmp[25], e_str[25], n_str[25];
  int sat;
  double i; /* inclination */
  double capOmega; /* right ascension of the ascending node */
  double e; /* eccentricity */
  double omega; /* argument of perigee */
  double capM; /* mean anomaly */
  double n; /* mean motion */
  double capE; /* eccentric anomaly */
  double a; /* semi-major axis */
  double alpha; /* right ascension */
  double delta; /* declination */
  double d2r = M_PI/180; /* degrees to radians */
  double epsilon = 1e-15; /* covergence threshold */
  double GM = 398600.4415;
  double capE_temp, t, px, py, pz, qx, qy, qz, x, y, z, r, vx, vy, vz;

  /* Parse command line args */
  if ((argc) < 2) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  sprintf(infile, "%s", argv[1]);
  sprintf(outfile, "%s", argv[2]);

  /* Reading Kepler orbital elements out of TLE file */
  fpIn = fopen(infile, "r");
  while (fgets(line, 512, fpIn)) {};
  sscanf(line, "2 %i %lf %lf %s %lf %lf %s",
	 &sat, &i, &capOmega, tmp, &omega, &capM, n_str);
  sprintf(e_str, "0.%s", tmp);
  e = strtod(e_str, NULL);
  n_str[11] = '\0';
  n = strtod(n_str, NULL);

  /* Determine the eccentric anomaly */
  capE_temp = capM;
  capE = capE_temp - 
    (capE_temp - e*sin(capE_temp*d2r) - capM) / (1 - e*cos(capE_temp*d2r)); 
  while (fabs(capE-capE_temp) > epsilon) {
    capE_temp = capE;
    capE = capE_temp - 
      (capE_temp - e*sin(capE_temp*d2r) - capM) / (1 - e*cos(capE_temp*d2r)); 
  }

  /* Calculate unit vectors */
  px = cos(omega*d2r)*cos(capOmega*d2r) - sin(omega*d2r)*cos(i*d2r)*sin(capOmega*d2r);
  py = cos(omega*d2r)*sin(capOmega*d2r) + sin(omega*d2r)*cos(i*d2r)*cos(capOmega*d2r);
  pz = sin(omega*d2r)*sin(i*d2r);
  qx = -sin(omega*d2r)*cos(capOmega*d2r) - cos(omega*d2r)*cos(i*d2r)*sin(capOmega*d2r);
  qy = -sin(omega*d2r)*sin(capOmega*d2r) + cos(omega*d2r)*cos(i*d2r)*cos(capOmega*d2r);
  qz = cos(omega*d2r)*sin(i*d2r);

  /* Determine semi-major axis */
  n /= d2r;
  t = GM*n*n;
  a = cbrt(t);

  /* Calculate Cartesian coordinates */
  x = a*(cos(capE*d2r)-e)*px + a*sqrt(1-e*e)*sin(capE*d2r)*qx;
  y = a*(cos(capE*d2r)-e)*py + a*sqrt(1-e*e)*sin(capE*d2r)*qy;
  z = a*(cos(capE*d2r)-e)*pz + a*sqrt(1-e*e)*sin(capE*d2r)*qz;

  /* Calculate Cartesian velocities */
  r = sqrt(x*x + y*y + z*z);
  vx = sqrt(GM*a)/r * (-sin(capE*d2r)*px + sqrt(1-e*e)*cos(capE*d2r)*qx);
  vy = sqrt(GM*a)/r * (-sin(capE*d2r)*py + sqrt(1-e*e)*cos(capE*d2r)*qy);
  vz = sqrt(GM*a)/r * (-sin(capE*d2r)*pz + sqrt(1-e*e)*cos(capE*d2r)*qz);

  /* Derive right ascension and declination from Cartesian coordinates */
  alpha = atan2(y, x)/d2r;
  t = sqrt(x*x + y*y);
  delta = atan2(z, t)/d2r;

  /* Writing true equatorial coordinates to output file */
  fpOut = fopen(outfile, "w");
  fprintf(fpOut, "CARTESIAN COORDINATES: POSITION\n");
  fprintf(fpOut, "x: %.4f\n", x);
  fprintf(fpOut, "y: %.4f\n", y);
  fprintf(fpOut, "z: %.4f\n\n", z);
  fprintf(fpOut, "CARTESIAN COORDINATES: VELOCITY\n");
  fprintf(fpOut, "vx: %.4f\n", vx);
  fprintf(fpOut, "vy: %.4f\n", vy);
  fprintf(fpOut, "vz: %.4f\n\n", vz);
  fprintf(fpOut, "TRUE EQUATORIAL COORDINATES\n");
  fprintf(fpOut, "right ascension: %.4f\n", alpha);
  fprintf(fpOut, "declination: %.4f\n", delta);

  /* Clean up and exit */
  fclose(fpIn);
  fclose(fpOut);
  return (0);

}

