#include "libasf_meta.h"

#define METADATA_VERSION 0.9

meta_parameters *asf_meta_read09(meta_parameter_t *meta_struct)
{
  meta_parameters *meta;
  char *meta_version, *str;
  int ii, num;

  /* Allocate memory */
  meta_version = (char *)MALLOC(10*sizeof(char));
  str = (char *)MALLOC(255*sizeof(char));

  /* Initialize the metadata structure for requested version */
  meta = asf_meta_init("0.9");

  /* Read metadata version as string for safe comparison */
  meta_version = metaString(meta_struct, "meta_version:"); 

  /* Geometry structure */
  meta->geo->type = metaChar(meta_struct, "geo.type:"); 
  
  /* Projection information */
  if (meta->geo->type == 'P') {
    meta->geo->proj = (meta_projection *) MALLOC(sizeof(meta_projection));
    meta->geo->proj->type = metaChar(meta_struct, "geo.proj.type:");
    meta->geo->proj->startX = metaDouble(meta_struct, "geo.proj.startX:");
    meta->geo->proj->startY = metaDouble(meta_struct, "geo.proj.startY:");
    meta->geo->proj->perX = metaDouble(meta_struct, "geo.proj.perX:");
    meta->geo->proj->perY = metaDouble(meta_struct, "geo.proj.perY:");
    meta->geo->proj->hem = metaChar(meta_struct, "geo.proj.hem:");
    meta->geo->proj->re_major = metaDouble(meta_struct, "geo.proj.re_major:");
    meta->geo->proj->re_minor = metaDouble(meta_struct, "geo.proj.re_minor:");
    if (meta->geo->proj->type == 'A') {
      meta->geo->proj->param.atct.rlocal = 
	metaDouble(meta_struct, "geo.proj.atct.rlocal:");
      meta->geo->proj->param.atct.alpha1 =
	metaDouble(meta_struct, "geo.proj.atct.alpha1:");
      meta->geo->proj->param.atct.alpha2 =
	metaDouble(meta_struct, "geo.proj.atct.alpha3:");
      meta->geo->proj->param.atct.alpha3 =
	metaDouble(meta_struct, "geo.proj.atct.alpha3:");
    }
    if (meta->geo->proj->type == 'P') {
      meta->geo->proj->param.ps.slat =
	metaDouble(meta_struct, "geo.proj.ps_lat:");
      meta->geo->proj->param.ps.slon =
	metaDouble(meta_struct, "geo.proj.ps_lon:");
    }
    if (meta->geo->proj->type == 'L') {
      meta->geo->proj->param.lambert.plat1 =
	metaDouble(meta_struct, "geo.proj.lam_plat1:");
      meta->geo->proj->param.lambert.plat2 =
	metaDouble(meta_struct, "geo.proj.lam_plat2:");
      meta->geo->proj->param.lambert.lat0 =
	metaDouble(meta_struct, "geo.proj.lam_lat:");
      meta->geo->proj->param.lambert.lon0 =
	metaDouble(meta_struct, "geo.proj.lam_lon:");
    }
    if (meta->geo->proj->type == 'U') {
      meta->geo->proj->param.utm.zone =
	metaInt(meta_struct, "geo.proj.utm_zone:");
    }
  }

  meta->geo->lookDir = metaChar(meta_struct, "geo.lookDir:");
  meta->geo->deskew = metaInt(meta_struct, "geo.deskew:");
  meta->geo->xPix = metaDouble(meta_struct, "geo.xPix:");
  meta->geo->yPix = metaDouble(meta_struct, "geo.yPix:");
  meta->geo->rngPixTime = metaDouble(meta_struct, "geo.rngPixTime:");
  meta->geo->azPixTime = metaDouble(meta_struct, "geo.azPixTime:");
  meta->geo->slantShift = metaDouble(meta_struct, "geo.slantShift:");
  meta->geo->timeShift = metaDouble(meta_struct, "geo.timeShift:");
  meta->geo->slantFirst = metaDouble(meta_struct, "geo.slantFirst:");
  meta->geo->wavelen = metaDouble(meta_struct, "geo.wavelength:");
  meta->geo->dopRange[0] = metaDouble(meta_struct, "geo.dopRangeCen:");
  meta->geo->dopRange[1] = metaDouble(meta_struct, "geo.dopRangeLin:");
  meta->geo->dopRange[2] = metaDouble(meta_struct, "geo.dopRangeQuad:");
  meta->geo->dopAz[0] = metaDouble(meta_struct, "geo.dopAzCen:");
  meta->geo->dopAz[1] = metaDouble(meta_struct, "geo.dopAzLin:");
  meta->geo->dopAz[2] = metaDouble(meta_struct, "geo.dopAzQuad:");
  
  /* Interferometry structure */
  meta->ifm->er = metaDouble(meta_struct, "ifm.er:");
  meta->ifm->ht = metaDouble(meta_struct, "ifm.ht:");
  meta->ifm->nLooks = metaInt(meta_struct, "ifm.nLooks:");
  meta->ifm->orig_nLines = metaInt(meta_struct, "ifm.orig_lines:");
  meta->ifm->orig_nSamples = metaInt(meta_struct, "ifm.orig_samples:");
  
  /* State vector structure */
  num = metaInt(meta_struct, "state.number:");
  meta->stVec = asf_meta_state_vectors_init(num);
  meta->stVec->year = metaInt(meta_struct, "state.year:");
  meta->stVec->julDay = metaInt(meta_struct, "state.day:");
  meta->stVec->second = metaDouble(meta_struct, "state.second:");
  meta->stVec->num = metaInt(meta_struct, "state.number:");
  for (ii=0; ii<meta->stVec->num; ii++) {
    sprintf(str, "state.vector[%i].time:", ii);
    meta->stVec->vecs[ii].time = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].x:", ii);
    meta->stVec->vecs[ii].vec.pos.x = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].y:", ii);
    meta->stVec->vecs[ii].vec.pos.y = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].z:", ii);
    meta->stVec->vecs[ii].vec.pos.z = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].vx:", ii);
    meta->stVec->vecs[ii].vec.vel.x = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].vy:", ii);
    meta->stVec->vecs[ii].vec.vel.y = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].vz:", ii);
    meta->stVec->vecs[ii].vec.vel.z = 
      metaDouble(meta_struct, str);
  }

  /* Extra info structure */
  str = metaString(meta_struct, "extra.sensor:");
  sprintf(meta->info->sensor, "%s", str);
  str = metaString(meta_struct, "extra.mode:");
  sprintf(meta->info->mode, "%s", str);
  str = metaString(meta_struct, "extra.processor:");
  sprintf(meta->info->processor, "%s", str);
  meta->info->orbit = metaInt(meta_struct, "extra.orbit:");
  meta->info->bitErrorRate = metaDouble(meta_struct, 
					"extra.bitErrorRate:");
  str = metaString(meta_struct, "extra.satBinTime:");
  sprintf(meta->info->satBinTime, "%s", str);
  str = metaString(meta_struct, "extra.satClkTime:");
  sprintf(meta->info->satClkTime, "%s", str);
  meta->info->prf = metaDouble(meta_struct, "extra.prf:");
  
  return meta;
 
}
