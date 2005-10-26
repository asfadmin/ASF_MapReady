#include "asf.h"
#include "asf_meta.h"
#include "proj.h"
#include "asf_proj2slant_range.h"

#define VERSION 1.5
#define gridResX 30
#define gridResY 30

/* Create a grid over a DEM to be reprojected into the slant range geometry
   of a SAR image.
*/
void create_dem_grid(char *demName, char *sarName, char *outName)
{
  int iflg=0;
  int gridCount,sar_x,sar_y,line_count,sample_count;
  char *ceos;
  FILE *out;
  meta_parameters *metaSar, *metaDem;
  double elev = 0.0;
  
  out=FOPEN(outName,"w");
  metaSar = meta_read(sarName);
  metaDem = meta_read(demName);  

  /* Convert all angles in projection part of metadata into radians -
     latlon_to_proj needs that lateron */
  switch (metaDem->projection->type) 
    {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      metaDem->projection->param.utm.lat0 *= D2R;
      metaDem->projection->param.utm.lon0 *= D2R;
      break;
    case POLAR_STEREOGRAPHIC:
      metaDem->projection->param.ps.slat *= D2R;
      metaDem->projection->param.ps.slon *= D2R;
      break;
    case ALBERS_EQUAL_AREA:
      metaDem->projection->param.albers.std_parallel1 *= D2R;
      metaDem->projection->param.albers.std_parallel2 *= D2R;
      metaDem->projection->param.albers.center_meridian *= D2R;
      metaDem->projection->param.albers.orig_latitude *= D2R;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      metaDem->projection->param.lamcc.plat1 *= D2R;
      metaDem->projection->param.lamcc.plat2 *= D2R;
      metaDem->projection->param.lamcc.lat0 *= D2R;
      metaDem->projection->param.lamcc.lon0 *= D2R;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      metaDem->projection->param.lamaz.center_lon *= D2R;
      metaDem->projection->param.lamaz.center_lat *= D2R;
      break;
    }
    
  /*Create a grid on the SAR image, and for each grid point:*/
  for (gridCount=0;getNextSarPt(metaSar,gridCount,&sar_x,&sar_y);gridCount++)
    {
      double dem_x,dem_y; /*This is what we're seeking-- 
                            the location on the DEM corresponding to the SAR point.*/
      double lat,lon; /*This is how we go between SAR and DEM images.*/
      double demProj_x,demProj_y; /*These are the projection coordinates for the DEM.*/
      int orig_x,orig_y;
      
      /*Compute the latitude and longitude of this location on the ground.*/
      meta_get_original_line_sample(metaSar, sar_y, sar_x, &orig_y, &orig_x);
      meta_get_latLon(metaSar,(float)orig_y,(float)orig_x,elev,&lat,&lon);
      
      /*Compute the projection coordinates of this location in the DEM.*/
      latlon_to_proj(metaDem->projection, metaSar->sar->look_direction, 
                     lat*D2R, lon*D2R, &demProj_x, &demProj_y);
      
      /*Compute the line,sample coordinates of this location in the DEM.*/
      dem_x = (demProj_x - metaDem->projection->startX) / metaDem->projection->perX;
      dem_y = (demProj_y - metaDem->projection->startY) / metaDem->projection->perY;

      fprintf(out,"%6d %6d %8.5f %8.5f %4.2f\n",sar_x,sar_y,dem_x,dem_y,1.0);
    }
  printf("   Created a grid of %ix%i points\n",gridResX,gridResY);
  if (logflag) {
    sprintf(logbuf,"   Created a grid of %ix%i points\n",gridResX,gridResY);
    printLog(logbuf);
    FCLOSE(fLog);
  }
  FCLOSE(out);
}

/*Return a regular, 30x30 grid of points.*/
int getNextSarPt(meta_parameters *meta,int gridNo,int *x,int *y)
{
  int xtmp, ytmp;
  
  if (gridNo>=gridResX*gridResY)
    return 0;
  
  xtmp = gridNo % gridResX;
  ytmp = gridNo / gridResX;
  
  *x = 1 + (float) xtmp / (float) (gridResX-1) * (meta->general->sample_count);
  *y = 1 + (float) ytmp / (float) (gridResY-1) * (meta->general->line_count);
  
  return 1;
}
