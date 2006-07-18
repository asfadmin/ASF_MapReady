#include "asf_elevation.h"

void asf_elevation(char *logFile, char *unwrapped_phase, char *phase_mask, 
		   char *baseFile, char *seeds, char *slant_amplitude, 
		   char *slant_coherence, char *ground_elevation, 
		   char *ground_elevation_error, char *ground_amplitude, 
		   char *ground_coherence)
{
  int x, y, ss, sl, nrows, ncols;
  double xScale, yScale, k, *phase2elevBase, *sinFlat, *cosFlat;
  meta_parameters *meta;
  baseline base;
  FILE *fphase, *felev, *feleverr, *fseed, *fmask, *fcoh;
  char slant_elevation[255], slant_elevation_error[255];
  float *uwp, *coh, *elev, *eleverr;
  unsigned char *mask;
  double delta_phase, delta_height;
  double seed_phase, seed_height;

  // Assigning temporary file names
  sprintf(slant_elevation, "tmp%d_sr_elev.img", (int)getpid());
  sprintf(slant_elevation_error, "tmp%d_sr_eleverr.img", (int)getpid());

  printf("\n   Generating slant range elevation and elevation error ...\n");

  /* Get input scene size and windowing info. Get datafile values*/
  meta = meta_read(unwrapped_phase);
  nrows  = meta->general->line_count;
  ncols  = meta->general->sample_count;
  sl     = meta->general->start_line;
  ss     = meta->general->start_sample;
  yScale = meta->sar->line_increment;
  xScale = meta->sar->sample_increment;
  
  // Write metadata files for temporary slant range images
  //  meta->general->image_data_type = DEM;
  meta_write(meta, slant_elevation);
  meta_write(meta, slant_elevation_error);
  
  /* Allocate space for vectors and matricies*/
  uwp = (float *) MALLOC(sizeof(float)*ncols);
  coh = (float *) MALLOC(sizeof(float)*ncols);
  elev = (float *) MALLOC(sizeof(float)*ncols);
  eleverr = (float *) MALLOC(sizeof(float)*ncols);
  mask = (unsigned char *) MALLOC(sizeof(unsigned char)*ncols);

  // Wavenumber K
  k = meta_get_k(meta);

  // Read in baseline values
  base = read_baseline(baseFile);
  
  // Open input files
  fphase = fopenImage(unwrapped_phase, "rb");
  fseed = FOPEN(seeds, "r");
  fmask = fopenImage(phase_mask, "rb");
  fcoh = fopenImage(slant_coherence, "rb");
  
  /*Use least-squares fit to determine the optimal seed_phase and seed_height.*/
  {
    double x,xSum=0,xSqrSum=0,hSum=0,hxSum=0,pxSum=0,pxSqrSum=0;
    double a,b,c,d,e,f,det;
    int npts=0;
    float *phase_line;
    
    phase_line = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
    
    while (1)
      {
	float seed_x,seed_y,height,phase;
	int seek_x,seek_y;
	/*Read in each seed point*/
	if (3!=fscanf(fseed,"%f%f%f",&seed_x,&seed_y,&height))
	  break;/*Break out when no more points.*/
	seek_x=(int)((seed_x-ss)/xScale);
	seek_y=(int)((seed_y-sl)/yScale);
	get_float_line(fphase, meta, seek_y, phase_line);
	phase = phase_line[seek_y];
	if (phase==0)
	  continue;/*Escher couldn't unwrap this tie point.*/
	
	// Calculate that seed point's impact on fit.
	x         = meta_phase_rate(meta,base,seed_y,seed_x);
	xSum     += x;
	xSqrSum  += x * x;
	hSum     += height;
	hxSum    += height * x;
	pxSum    += phase * x;
	pxSqrSum += phase * x * x;
	npts++;
      }
    if (!quietflag)
      printf("   Read %d seed points\n",npts);
    /* The least-squares fit above leaves us with a matrix equation
     *	[ a  b ]   [ seed_phase  ]   [ e ]
     *	[      ] * [             ] = [   ]
     *	[ c  d ]   [ seed_height ]   [ f ]
     *
     *	which has the solution
     *
     *	[ d  -b ]   [ e ]    1    [ seed_phase  ]
     *	[       ] * [   ] * --- = [             ]
     *	[ -c  a ]   [ f ]   det   [ seed_height ]
     */
    a = -xSqrSum;
    b = xSum;
    c = -xSum;
    d = npts;
    e = hxSum-pxSqrSum;
    f = hSum-pxSum;
    det = a*d-b*c;
    seed_phase  = (e*d-f*b)/det;
    seed_height = (e*(-c)+f*a)/det;
  }
  
  if (!quietflag) 
    printf("   Seed Phase: %f\n   Elevation: %f\n",seed_phase,seed_height);
  
  /* calculate the sine of the incidence angle across cols*/
  sinFlat = (double *)MALLOC(sizeof(double)*ncols);
  cosFlat = (double *)MALLOC(sizeof(double)*ncols);
  phase2elevBase = (double *)MALLOC(sizeof(double)*ncols);
  for (x=0;x<ncols;x++)
    {
      int img_x = x*xScale + ss;
      double incid = meta_incid(meta, 0, img_x);
      double flat = meta_flat(meta, 0, img_x);
      sinFlat[x] = sin(flat);
      cosFlat[x] = cos(flat);
      phase2elevBase[x] = meta_get_slant(meta, 0, img_x)*sin(incid)/(2.0*k);
    }
  
  // Open intermediate output files
  felev = fopenImage(slant_elevation, "wb");
  feleverr = fopenImage(slant_elevation_error, "wb");
  
  /* loop through each row & calculate height*/
  /*Note:
    To make this faster, we don't call 
    delta_height=delta_phase * meta_phase_rate(ceos,base,y*yScale+sl,x*xScale+ss).
    Instead, we use the annoying temporary arrays
    allocated above to calculate the same thing, quicker.
  */
  for (y=0; y<nrows; y++) {
    double Bn_y,Bp_y;

    // Read in data
    FREAD(mask, sizeof(unsigned char), ncols, fmask);
    get_float_line(fphase, meta, y, uwp);
    get_float_line(fcoh, meta, y, coh);
    
    // Calculate baseline for this row
    meta_interp_baseline(meta, base, y*yScale+sl+1, &Bn_y, &Bp_y);
    
    // Step through each pixel in row
    for (x=0; x<ncols; x++) {
      // Calculate elevation
      if (uwp[x] != 0.0) {
	delta_phase = (double) uwp[x] - seed_phase;
	delta_height = delta_phase * phase2elevBase[x]/
	  (-Bp_y*sinFlat[x] - Bn_y*cosFlat[x]);
	elev[x] = delta_height + seed_height;
      }
      else 
	elev[x] = 0.0;
      // Calculate elevation error
      if (mask[x] == 0x10) {
	double coh_factor, base_height, sigma_height;
	coh_factor = (FLOAT_EQUALS_ZERO(coh[x])) ? 0.0 : sqrt((1-coh[x])/coh[x]);
	base_height = phase2elevBase[x]/(-Bp_y*sinFlat[x] - Bn_y*cosFlat[x]);
	sigma_height = base_height * coh_factor;
	eleverr[x] = (float) fabs(base_height*coh_factor);
      }
      else
	eleverr[x] = -1.0;
    }
    
    put_float_line(felev, meta, y, elev);
    put_float_line(feleverr, meta, y, eleverr);

    asfPercentMeter(y*100/nrows);
  }
  
  // Free memory and close files
  meta_free(meta);
  FREE(uwp);
  FREE(mask);
  FREE(coh);
  FREE(elev);
  FREE(eleverr);
  FREE(sinFlat);
  FREE(cosFlat);
  FREE(phase2elevBase);
  FCLOSE(felev);
  FCLOSE(feleverr);
  FCLOSE(fphase);
  FCLOSE(fseed);
  FCLOSE(fmask);

  // Transform all the slant range products into ground range
  printf("   Generating ground range elevation ...\n");
  deskew_dem(slant_elevation, ground_elevation, NULL, 0, NULL, NULL, TRUE);
  printf("   Generating ground range amplitude image ...\n");
  deskew_dem(slant_elevation, ground_amplitude, slant_amplitude, 1, NULL, NULL,
             TRUE);
  printf("   Generating ground range elevation error ...\n");
  deskew_dem(slant_elevation, ground_elevation_error, slant_elevation_error, 1,
             NULL, NULL, TRUE);
  printf("   Generating ground range coherence image ...\n\n");
  deskew_dem(slant_elevation, ground_coherence, slant_coherence, 0, NULL, NULL,
             TRUE);

}
