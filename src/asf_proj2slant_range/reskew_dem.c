/****************************************************************
 reskew_dem maps an input, ground range DEM into slant range, and
 creates a simulated SAR image.  The input DEM must already be lined up
 with the image, but need not be precisely co-registered. In fact, the
 amplitude image is generated only so the images can be co-registered.
******************************************************************************/
#include "asf_proj2slant_range.h"

#define VERSION 1.5

/* DEFINITIONS:
 * Ground Range: Distance measured along curve of earth, at sea level
 *                 (0 elevation).
 * Phi:          Smallest angle made between line connecting point on earth's
 *                 surface with earth's center and line connecting satellite and
 *              earth's center.
 * Slant Range:  Straight-line distance between satellite and point on earth's
 *                 surface.
 */

#define phi2grX(phi) (((phi)-minPhi)*phiMul)
#define grX2phi(gr) (minPhi+(gr)/phiMul)

extern float badDEMht;
extern int maxBreakLen;

extern double grPixelSize;
extern int gr_ns,sr_ns;

/*Array[gr_ns] indexed by ground range pixel.*/
extern double *slantGR;/*Slant range pixel #*/
extern double *heightShiftGR;

/*Array[sr_ns] indexed by slant range pixel.*/
extern double *heightShiftSR;
extern double *groundSR;/*Ground range pixel #*/
extern double *slantRangeSqr,*slantRange,*heightShift;
extern double *incidAng,*sinIncidAng,*cosIncidAng;

double calc_ranges(meta_parameters *meta);

void geo_compensate(float *srDEM,float *in,float *out,int ns);
void radio_compensate(float *grDEM, float *grDEMprev,float *inout,int ns);
void dem_sr2gr(float *inBuf,float *outBuf);
void dem_gr2sr(float *grDEM, float *srDEM,float *amp);
void dem_interp_col(float *buf,int ns,int nl);

float sr2gr(float srX,float height);
float gr2sr(float grX,float height);

double grPixelSize;
int gr_ns,sr_ns;

double *slantGR;/*Slant range pixel #*/
double *heightShiftGR;
double *heightShiftSR;
double *groundSR;/*Ground range pixel #*/
double *slantPixel;
double *groundRange;
double *slantRangeSqr,*slantRange,*heightShift;
double *incidAng,*sinIncidAng,*cosIncidAng;


void reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile, 
		char *outAmpFile)
{
  float *grDEMline,*srDEMline,*outAmpLine;
  register int line,nl;
  FILE *inDEM,*outDEM,*outAmp;
  meta_parameters *metaIn, *metaDem, *metaOut;
    
  /* Get metadata */
  metaIn = meta_read(inMetafile);
  metaDem = meta_read(inDEMfile);
  meta_write(metaIn, outDEMfile);
  metaOut = meta_read(outDEMfile);
  nl = metaDem->general->line_count;
  gr_ns = metaIn->general->sample_count;
  sr_ns = gr_ns;
  	
  /*Allocate vectors.*/
  slantGR       = (double *)MALLOC(sizeof(double)*gr_ns);
  groundSR      = (double *)MALLOC(sizeof(double)*sr_ns);
  heightShiftSR = (double *)MALLOC(sizeof(double)*sr_ns);
  heightShiftGR = (double *)MALLOC(sizeof(double)*gr_ns);
  slantRange    = (double *)MALLOC(sizeof(double)*sr_ns);
  slantRangeSqr = (double *)MALLOC(sizeof(double)*sr_ns);
  incidAng      = (double *)MALLOC(sizeof(double)*sr_ns);
  sinIncidAng   = (double *)MALLOC(sizeof(double)*sr_ns);
  cosIncidAng   = (double *)MALLOC(sizeof(double)*sr_ns);
  
  grPixelSize = calc_ranges(metaOut);
  
  /*Open files.*/
  inDEM  = fopenImage(inDEMfile,"rb");
  outDEM = fopenImage(outDEMfile,"wb");
  outAmp = fopenImage(outAmpFile,"wb");
  
  /*Allocate more memory (this time for data lines*/
  grDEMline  = (float *)MALLOC(sizeof(float)*gr_ns);
  srDEMline  = (float *)MALLOC(sizeof(float)*sr_ns);
  outAmpLine = (float *)MALLOC(sizeof(float)*sr_ns);
  
  /* Read deskewed data, write out reskewed data */
  for (line=0; line<nl; line++) {
    get_float_line(inDEM,metaIn,line,grDEMline);
    dem_gr2sr(grDEMline,srDEMline,outAmpLine);
    put_float_line(outDEM,metaOut,line,srDEMline);
    put_float_line(outAmp,metaOut,line,outAmpLine);
  }
  
  sprintf(logbuf,"   Converted %d lines from ground range to slant range.\n\n", nl);
  printf("%s", logbuf);
  if (logflag) { printLog(logbuf); }
  
  /* Write meta files */
  meta_write(metaOut, outDEMfile);
  meta_write(metaOut, outAmpFile);
  
  /* Free memory, close files, & exit */
  meta_free(metaIn);
  meta_free(metaDem);
  meta_free(metaOut);
  FREE(slantGR);
  FREE(groundSR);
  FREE(heightShiftSR);
  FREE(heightShiftGR);
  FREE(slantRange);
  FREE(slantRangeSqr);
  FREE(incidAng);
  FREE(sinIncidAng);
  FREE(cosIncidAng);
  FREE(grDEMline);
  FREE(srDEMline);
  FREE(outAmpLine);
  FCLOSE(inDEM);
  FCLOSE(outDEM);
  FCLOSE(outAmp);
  
}


#if 1
/*Use linearized arrays to do conversion.*/
float sr2gr(float srX,float height)
{
  double dx,srXSeaLevel=srX-height*heightShiftSR[(int)srX];
  int ix;
  if (srXSeaLevel<0) srXSeaLevel=0;
  if (srXSeaLevel>=sr_ns-1) srXSeaLevel=sr_ns-1;
  ix=(int)srXSeaLevel;
  dx=srXSeaLevel-ix;
  /*Linear interpolation on groundSR array*/
  return groundSR[ix]+dx*(groundSR[ix+1]-groundSR[ix]);
}
float gr2sr(float grX,float height)
{
  double dx,grXSeaLevel=grX-height*heightShiftGR[(int)grX];
  int ix;
  if (grXSeaLevel<0) grXSeaLevel=0;
  if (grXSeaLevel>=gr_ns-1) grXSeaLevel=gr_ns-1;
  ix=(int)grXSeaLevel;
  dx=grXSeaLevel-ix;
  /*Linear interpolation on slantGR array*/
  return slantGR[ix]+dx*(slantGR[ix+1]-slantGR[ix]);
}
#else
/*Use fundamental equations to do conversion.*/
float sr2gr(float srX,float height)
{
  double slant=slantFirst+srX*slantPer;
  double phi=acos((satHt*satHt+(er+height)*(er+height)-slant*slant)/
		  (2.0*satHt*(er+height)));
  return phi2grX(phi);
}
float gr2sr(float grX,float height)
{
  double phi=grX2phi(x);
  double slant=sqrt(satHt*satHt+(er+height)*(er+height)-cos(phi)*
		    (2.0*satHt*(er+height)));
  return (slant-slantFirst)/slantPer;
}
#endif


/* DEM ground range to slant range */

float badDEMht=0.0;
float unInitDEM=-1.0;
int maxBreakLen=20;

float unitRand(void)
{
  return (float)(0x0ffff&rand())/(0x0ffff);
}

float gaussRand(int num)
{
  int i;
  float ret=0;
  for (i=0;i<num;i++)
    ret+=unitRand();
  return 2*ret/num;
}

/*Note: it may seem stupid to add speckle to our simulated
  SAR images, but the speckle has an important statistical 
  contribution to the image.*/
float *createSpeckle(void)
{
  int i;
#define speckleLen 0x0fff
#define getSpeckle speckle[specklePtr=((specklePtr+7)&speckleLen)]
  float *speckle=(float *)MALLOC(sizeof(float)*(speckleLen+1));
  for (i=0;i<=speckleLen;i++)
    speckle[i]=gaussRand(4);
  return speckle;
}

/*dem_gr2sr: map one line of a ground range DEM into
  one line of a slant range DEM && one line of simulated amplitude image.*/
void dem_gr2sr(float *grDEM, float *srDEM,float *amp)
{
  int x,grX;
  double lastSrX=-1; /*Slant range pixels up to (and including) here 
		       have been filled.*/
  float lastOutValue=badDEMht;
  static float *speckle=NULL;
  int specklePtr=rand();
  if (speckle==NULL)
    speckle=createSpeckle();
  
  /*Initialize amplitude to zero, and DEM to -1.*/
  for (x=0;x<sr_ns;x++)
    {
      amp[x]=0;
      srDEM[x]=unInitDEM;
    }
  
  /*Step through the ground range line using grX.
    Convert each grX to an srX.  Update amplitude and height images.*/
  for (grX=1;grX<gr_ns;grX++)
    {
      double height=grDEM[grX];
      /*srX: float slant range pixel position.*/
      double srX=gr2sr(grX,height);
      int sriX=(int)srX;
      if ((height!=badDEMht)&&(srX>=0)&&(srX<sr_ns))
	{
	  double runLen=srX-lastSrX;
	  int intRun=(int)runLen;
	  if ((runLen<maxBreakLen)&&(lastOutValue!=badDEMht))
	    {
	      double currAmp;
	      /*Update the amplitude image.*/
	      if (runLen<0) 
		runLen=-runLen;
	      currAmp=50.0/(runLen*runLen*5+0.1);
	      for (x=lastSrX+1;x<=sriX;x++)
		amp[x]+=currAmp*getSpeckle;
	      /*Then, update the height image.*/
	      if (intRun!=0)
		{
		  float curr=lastOutValue;
		  float delt=(height-lastOutValue)/intRun;
		  curr+=delt;
		  for (x=lastSrX+1;x<=sriX;x++)
		    {
		      if (srDEM[x]==unInitDEM)
			srDEM[x]=curr;/*Only write on fresh pixels.*/
		      /*      else
			      srDEM[x]=badDEMht; Black out layover regions.*/
		      curr+=delt;
		    }
		} else 
		  if (srDEM[(int)lastSrX+1]==unInitDEM)
		    srDEM[(int)lastSrX+1]=height;
	    } else {
	      for (x=lastSrX+1;x<=sriX;x++)
		srDEM[x]=badDEMht;
	    }
	  lastOutValue=height;
	  lastSrX=srX;
	}
    }
  /*Fill to end of line with zeros.*/
  for (x=lastSrX+1;x<sr_ns;x++)
    srDEM[x]=badDEMht;
  /* Just plug all the holes and see what happens */
  /*Attempt to plug one-pixel holes, by interpolating over them.*/
  for (x=1;x<(sr_ns-1);x++)
    {
      if (srDEM[x]==badDEMht)
	/* &&
	   srDEM[x-1]!=badDEMht &&
	   srDEM[x+1]!=badDEMht)*/
	srDEM[x]=(srDEM[x-1]+srDEM[x+1])/2;
    }
}

/*
  Diffuse (lambertian) reflection:
  reflPower=cosIncidAng[sriX]+
  sinIncidAng[sriX]*(grDEM[grX]-grDEM[grX-1])/grPixelSize;
  if (reflPower<0) reflPower=0;Radar Shadow.
  currAmp=reflPower/runLen;
*/

/* Calculates range */

double calc_ranges(meta_parameters *meta)
{
  int x;
  double slantFirst,slantPer;
  double er  = meta_get_earth_radius(meta, meta->general->line_count/2, 0);
  double satHt = meta_get_sat_height(meta, meta->general->line_count/2, 0);
  double saved_ER = er;
  double er2her2,phi,phiAtSeaLevel,slantRng;
  double minPhi,maxPhi,phiMul;

  meta_get_slants(meta,&slantFirst,&slantPer);
  slantFirst += slantPer*meta->general->start_sample;
  slantPer *= meta->sar->sample_increment;
  er2her2 = er*er-satHt*satHt;
  minPhi=acos((satHt*satHt+er*er-slantFirst*slantFirst)/(2.0*satHt*er));
  
  /*Compute arrays indexed by slant range pixel:*/
  for (x=0;x<sr_ns;x++)
    {
      /*Precompute slant range for SR pixel x.*/
      slantRange[x]=slantFirst+x*slantPer;
      slantRangeSqr[x]=slantRange[x]*slantRange[x];
      /*Compute incidence angle for SR pixel x.*/
      incidAng[x]=M_PI-acos((slantRangeSqr[x]+er2her2)/(2.0*er*slantRange[x]));
      sinIncidAng[x]=sin(incidAng[x]);
      cosIncidAng[x]=cos(incidAng[x]);
    }
  
  maxPhi=acos((satHt*satHt+er*er-slantRangeSqr[sr_ns-1])/(2.0*satHt*er));
  phiMul=(sr_ns-1)/(maxPhi-minPhi);
  
  /*Compute arrays indexed by ground range pixel: slantGR and heightShiftGR*/
  for (x=0;x<gr_ns;x++)
    {
      er=saved_ER;
      phiAtSeaLevel=grX2phi(x);
      slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
      slantGR[x]=(slantRng-slantFirst)/slantPer;
      er+=1000.0;
      phi=acos((satHt*satHt+er*er-slantRng*slantRng)/(2*satHt*er));
      
      heightShiftGR[x]=(phi2grX(phi)-x)/1000.0;
    }
  /*Compute arrays indexed by slant range pixel: groundSR and heightShiftSR*/
  for (x=0;x<sr_ns;x++)
    {
      er=saved_ER;
      phiAtSeaLevel=acos((satHt*satHt+er*er-slantRangeSqr[x])/(2*satHt*er));
      groundSR[x]=phi2grX(phiAtSeaLevel);
      er+=1000.0;
      slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
      
      heightShiftSR[x]=((slantRng-slantFirst)/slantPer-x)/1000.0;
    }
  er=saved_ER;
  return er/phiMul;
}
