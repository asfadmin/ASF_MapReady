#include "asf.h"
#include "sgpsdp.h"
#include <stdio.h>

#define TEST_STEPS 5

typedef struct {
        double t;
        double x;
        double y;
        double z;
        double vx;
        double vy;
        double vz;
} dataset_t;

const dataset_t expected[TEST_STEPS] = {
        { 0.0,
          7473.37066650, 428.95261765, 5828.74786377,
          5.1071513, 6.44468284, -0.18613096 },
        { 360.0,
          -3305.22537232, 32410.86328125, -24697.17675781,
          -1.30113538, -1.15131518, -0.28333528 },
        { 720.0,
          14271.28759766, 24110.46411133, -4725.76837158,
          -0.32050445, 2.67984074, -2.08405289 },
        { 1080.0,
          -9990.05883789, 22717.35522461, -23616.890662501,
          -1.01667246, -2.29026759, 0.72892364 },
        { 1440.0,
          9787.86975097, 33753.34667969, -15030.81176758,
          -1.09425966, 0.92358845, -1.52230928}
};

int
main (int argc, char *argv[])
{
  int i;
  if (argc != 4) {
    printf("Usage: sgpsdp <tle filename> <satellite name> <start time>\n");
    exit(1);
  }

  FILE *tle = fopen(argv[1], "r");
  char *sat = argv[2];
  double t = atof(argv[3]);

  char line0[256], line1[256], line2[256];

  while (1) {
    if (fgets(line0, 256, tle)) {
      if (strncmp(line0, sat, strlen(sat))==0) {
        if (fgets(line1, 256, tle)) {
          if (fgets(line2, 256, tle)) {
            
            CSGP4_SDP4 *s = SGP4_SDP4_new_from_tle(line0,line1,line2);

            for (i=0; i<TEST_STEPS; ++i) {
              t = expected[i].t + 2444514.48708465;
              int ret = SGP(s,t);
              if (!ret) {
                printf("Failed.\n");
                exit(1);
              }

              CalculateLatLonAlt(s,t);

              printf("%6.1f %10.2f %10.2f\n", expected[i].t,
                     GetLat(s), GetLon(s));

              VECTOR pos = GetUserPos(s);

              printf("       X: %15.8f %15.8f %15.8f (%.5f%%)\n",
                     pos.x, expected[i].x, fabs(pos.x-expected[i].x),
                     100*fabs(pos.x-expected[i].x)/fabs(expected[i].x));
              printf("       Y: %15.8f %15.8f %15.8f (%.5f%%)\n",
                     pos.y, expected[i].y, fabs(pos.y-expected[i].y),
                     100*fabs(pos.y-expected[i].y)/fabs(expected[i].y));
              printf("       Z: %15.8f %15.8f %15.8f (%.5f%%)\n",
                     pos.z, expected[i].z, fabs(pos.z-expected[i].z),
                     100*fabs(pos.z-expected[i].z)/fabs(expected[i].z));

              VECTOR vel = GetUserVel(s);

              printf("       X: %15.8f %15.8f %15.8f (%.5f%%)\n",
                     vel.x, expected[i].vx, fabs(vel.x-expected[i].vx),
                     100*fabs(vel.x-expected[i].vx)/fabs(expected[i].vx));
              printf("       Y: %15.8f %15.8f %15.8f (%.5f%%)\n",
                     vel.y, expected[i].vy, fabs(vel.y-expected[i].vy),
                     100*fabs(vel.y-expected[i].vy)/fabs(expected[i].vy));
              printf("       Z: %15.8f %15.8f %15.8f (%.5f%%)\n",
                     vel.z, expected[i].vz, fabs(vel.z-expected[i].vz),
                     100*fabs(vel.z-expected[i].vz)/fabs(expected[i].vz));
            }

            fclose(tle);
            return TRUE;

          } else
            break;
        } else
          break;
      }
    } else
      break;
  }
  fclose(tle);
  printf("Not found in %s: %s\n", argv[1], sat);

  return FALSE;
}
