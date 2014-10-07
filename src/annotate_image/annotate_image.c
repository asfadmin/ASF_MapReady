/******************************************************************************** 
 *  Add annotation to an image... Places crosses at the given locations.
 * 
 *  Part of the RTC project.  Reads a binary offset file from GAMMA.
 *  Marks locations on the MLI image based upon the start, step, and number of points
 *  in both the azimuth and range direction.
 *
 *******************************************************************************/
#include "asf_nan.h"
#include "asf.h"
#include "asf_meta.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <asf_license.h>
#include <asf_contact.h>

void annotate(int line,int samp,float *buf,int nsamps, float val);
double orbital_inclination(meta_parameters *meta);

int   LENGTH=95;
int   WIDTH=31;

int main(int argc,char *argv[])
{
  char  infile[256];
  char  outfile[256];
  char  csvFile[256];
  int   i,j;


  if (argc != 10) {
      printf("Usage:  %s <infile> <ptfile> <rng_start> <rng_spacing> <rpts> <az_start> <az_spacing> <apts> <outfile>\n",argv[0]);
      printf("\n%s : **Not enough arguments (need 3, got %d).\n",argv[0],argc);
      return(1);
  }

  printf("Creating annotated image from %s using %s\n",argv[1],argv[2]);

  if (!quietflag) asfSplashScreen(argc, argv);

  create_name(infile,argv[1],".img");
  meta_parameters *meta = meta_read(infile);
  create_name(outfile,argv[9],".img");
  create_name(csvFile,argv[9],".csv");

  FILE *fp1 = FOPEN(infile, "rb");
  FILE *fp2 = FOPEN(argv[2], "rb");
  FILE *ofp = FOPEN(outfile, "wb");
  FILE *fpCSV = FOPEN(csvFile, "w");
  fprintf(fpCSV, "ID,LAT,LON,DX,DY,MAGNITUDE,DIRECTION\n");
  
  int start_range = atoi(argv[3]);
  int step_range = atoi(argv[4]);
  int range_pts = atoi(argv[5]);
  int start_az = atoi(argv[6]);
  int step_az = atoi(argv[7]);
  int az_pts = atoi(argv[8]); 

  int pixel_count = meta->general->line_count*meta->general->sample_count;
  float *buf = (float *) MALLOC(pixel_count * sizeof(float));

  if (meta->general->sample_count < 2000) { LENGTH = 61; WIDTH = 21; }
  else if (meta->general->sample_count < 6000) { LENGTH = 121; WIDTH = 41; }
  else { LENGTH = 241; WIDTH = 81;}

/*   LENGTH = 0.7*(meta->general->sample_count/range_pts);
    WIDTH = 0.333 * LENGTH;
*/

  printf("Using length %i width %i\n",LENGTH,WIDTH);

  get_float_lines(fp1,meta,0,meta->general->line_count, buf);
  FCLOSE(fp1);

  int npts = range_pts*az_pts*2;
  float *offsets = (float *)MALLOC(npts*sizeof(float));
  ASF_FREAD(offsets,sizeof(float),npts,fp2);
  FCLOSE(fp2);

  double inc; 
  if (meta->general->orbit_direction == 'D')
    inc = orbital_inclination(meta) - 90.0;
  else
    inc = 90.0 - orbital_inclination(meta);

  int line, samp;
  int pt = 0, good_pts = 0;
  double dx, dy, lat, lon, magnitude, direction;
  for (i=0, line=start_az; i<az_pts; i++, line+=step_az) {
    if (i>meta->general->line_count) { printf("ERROR: past end of file\n"); exit(1);}
    for (j=0, samp=start_range; j<range_pts; j++, samp+=step_range) {
      if (j>meta->general->sample_count) { printf("ERROR: past end of line\n"); exit(1);}
      if (offsets[pt] != 0.0 && offsets[pt+1] != 0.0) {
        dx = (double)offsets[pt];
        dy = (double)offsets[pt+1];
        meta_get_latLon(meta, (double)line, (double)samp, 0.0, &lat, &lon);
        magnitude = sqrt(dx*dx + dy*dy);
        direction = atan2(dx, dy)*R2D + inc;
        fprintf(fpCSV, "%d,%.4f,%.4lf,%.4lf,%.4f,%.4lf,%.4lf\n",
          (pt/2)+1, lat, lon, dx, dy, magnitude, direction);
        annotate(line,samp,buf,meta->general->sample_count,meta->stats->band_stats[0].max);
        good_pts++;
      } /* else { printf("rejected\n");} */
      pt+=2;
    }
  }

  put_float_lines(ofp, meta, 0,meta->general->line_count,buf);
  meta_write(meta, outfile);
  meta_free(meta);
  fclose(ofp);
  FCLOSE(fpCSV);
  printf("Annotated %i points in the image\n",good_pts);

  asfPrintStatus("Done.\n");
  exit(EXIT_SUCCESS);
}

void annotate(int line,int samp,float *buf,int nsamps, float val)
 {
   int i,j;
 
   /* do the vertical line */
   for (i=line-LENGTH/2; i<line+LENGTH/2; i++) 
     for (j=samp-WIDTH/2; j<samp+WIDTH/2; j++)
        buf[i*nsamps+j] = val;

   /* do the horizontal line */ 
   for (i=line-WIDTH/2; i<line+WIDTH/2; i++) 
     for (j=samp-LENGTH/2; j<samp+LENGTH/2; j++)
        buf[i*nsamps+j] = val;
 }
 
double orbital_inclination(meta_parameters *meta)
{
  double x = meta->state_vectors->vecs[0].vec.pos.x;
  double y = meta->state_vectors->vecs[0].vec.pos.y;
  double z = meta->state_vectors->vecs[0].vec.pos.z;
  double vx = meta->state_vectors->vecs[0].vec.vel.x;
  double vy = meta->state_vectors->vecs[0].vec.vel.y;
  double vz = meta->state_vectors->vecs[0].vec.vel.z;
  double hx = y*vz - z*vy;
  double hy = z*vx - x*vz;
  double hz = x*vy - y*vx;
  double h = sqrt(hx*hx + hy*hy + hz*hz);
  double i = acos(hz/h)*R2D;
  
  return i;
}
