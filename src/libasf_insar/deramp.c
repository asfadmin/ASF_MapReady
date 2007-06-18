#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_insar.h"

/* local constants */
#define VERSION 2.5

int deramp(char *inFile, char *baseFile, char *outFile, int back)
{
  FILE  *fin, *fout;
  meta_parameters *meta;
  baseline base;
  int samples, lines, ss, sl, x, y;
  double xScale, yScale;
    char  szInPhase[255], szInAmp[255], szOutPhase[255], szOutAmp[255];
  float *data;
  double *sflat, *cflat;
  
  /* Get input scene size and windowing info, check validity */
  meta = meta_read(szInPhase);
  
  samples = meta->general->sample_count;
  lines = meta->general->line_count;
  ss = meta->general->start_sample - 1;
  sl = meta->general->start_line - 1;
  xScale = meta->sar->sample_increment;
  yScale = meta->sar->line_increment;
  
  create_name(szInAmp, inFile, "_amp.img");
  create_name(szInPhase, inFile, "_phase.img");
  create_name(szOutAmp, outFile, "_amp.img");
  meta_write(meta, szOutAmp);
  create_name(szOutPhase, outFile, "_phase.img");
  meta_write(meta, szOutPhase);
  
  /*Link over ".amp" file, if it exists.*/
  if (fileExists(szInAmp)&&!fileExists(szOutAmp))
    {
      char command[1024];
      sprintf(command,"ln -s %s %s\n", szInAmp, szOutAmp);
      system(command);
    }
  
  /* buffer mallocs, read data file */
  data = (float *)MALLOC(sizeof(float)*samples);
  sflat = (double *)MALLOC(sizeof(double)*samples);
  cflat = (double *)MALLOC(sizeof(double)*samples);
  fin = fopenImage(szInPhase,"rb");
  fout = fopenImage(szOutPhase,"wb");
  
  /* read in CEOS parameters & convert to meters */
  base = read_baseline(baseFile);
  
  /* calculate slant ranges and look angles - Ian's thesis eqn 3.10 */
  for (x = 0; x < samples; x++) {
    double flat=meta_flat(meta,0.0,x*xScale+ss);
    sflat[x]=sin(flat);
    cflat[x]=cos(flat);
  }

  for (y = 0; y < lines; y++) {
    double Bn_y,Bp_y;
    double twok = back*2.0*meta_get_k(meta);
    meta_interp_baseline(meta,base,y*(int)yScale+sl,&Bn_y,&Bp_y);
    get_float_line(fin, meta, y, data);
    
    /* calculate flat-earth range phase term & remove it */ 
    for (x = 0; x < samples; x++)
      {
	double d=data[x];
	if (d!=0.0) /*Ignore points which didn't phase unwrap.*/
	  d -= twok*(Bp_y*cflat[x]-Bn_y*sflat[x]);
	/*Was: d-=ceos_flat_phase(ceos,base,x,y);*/
	data[x]=d;
      }
    
    put_float_line(fout, meta, y, data);
    asfLineMeter(y, lines);
  }
  
  // Clean up
  FCLOSE(fin); 
  FCLOSE(fout);
  FREE(data); 
  FREE(sflat);
  FREE(cflat);
  
  return 0;
}
