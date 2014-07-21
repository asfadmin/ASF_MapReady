#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_tfdr(struct trl_file_des_rec *fd)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  int ii;

  add(&ret, "\n******** begin of Trailer File Descriptor (JAXA) record *********\n");
  add(&ret, "\n Number of data set summary records: %d", fd->num_dssr);
  add(&ret, "\n Data set summary record length: %d", fd->dssr_len);
  add(&ret, "\n Number of map projection data records: %d", fd->num_mpdr);
  add(&ret, "\n Map projection record length: %d", fd->mpdr_len);
  add(&ret, "\n Number of platform pos. data records: %d", fd->num_ppdr);
  add(&ret, "\n Platform position record length: %d", fd->ppdr_len);
  add(&ret, "\n Number of attitude data records: %d", fd->num_atdr);
  add(&ret, "\n Attitude data record length: %d", fd->atdr_len);
  add(&ret, "\n Number of radiometric data records: %d", fd->num_radr);
  add(&ret, "\n Radiometric record length: %d", fd->radr_len);
  add(&ret, "\n Number of radiometric compensation records: %d", fd->num_rdcr);
  add(&ret, "\n Radiometric compensation rec. length: %d", fd->rdcr_len);
  add(&ret, "\n Number of data quality summary records: %d", fd->num_dqsr);
  add(&ret, "\n Data quality summary record length: %d", fd->dqsr_len);
  add(&ret, "\n Number of data histograms records: %d", fd->num_dhr);
  add(&ret, "\n Data histogram record length: %d", fd->dhr_len);
  add(&ret, "\n Number of range spectra records: %d", fd->num_rsr);
  add(&ret, "\n Range spectra record length: %d", fd->rsr_len);
  add(&ret, "\n Number of DEM descriptor records: %d", fd->num_dem_dr);
  add(&ret, "\n DEM descriptor record length: %d", fd->dem_dr_len);
  add(&ret, "\n Number of Radar par. update records: %d", fd->num_rpur);
  add(&ret, "\n Radar par. update record length: %d", fd->rpur_len);
  add(&ret, "\n Number of Annotation data records: %d", fd->num_adr);
  add(&ret, "\n Annotation data record length: %d", fd->adr_len);
  add(&ret, "\n Number of Det. processing records: %d", fd->num_dpr);
  add(&ret, "\n Det. processing record length: %d", fd->dpr_len);
  add(&ret, "\n Number of Calibration records: %d", fd->num_calr);
  add(&ret, "\n Calibration record length: %d", fd->calr_len);
  add(&ret, "\n Number of GCP records: %d", fd->num_gcpr);
  add(&ret, "\n GCP record length: %d", fd->gcpr_len);
  for (ii=0; ii<11; ii++) {
    add(&ret, "\n Number of facility data (%d) records: %d", 
	ii+1, fd->num_facdr[ii]);
    add(&ret, "\n Facility data (%d) record length: %d", 
	ii+1, fd->facdr_len[ii]);
  }
  add(&ret, "\n Number of low image data records: %d", fd->low_res_img_num);
  add(&ret, "\n Low resolution image data record length: %d", 
      fd->low_res_img_len);
  add(&ret, "\n Number of pixels of low res image data: %d", 
      fd->low_res_pixels);
  add(&ret, "\n Number of lines of low res image data: %d",
      fd->low_res_lines);
  add(&ret, "\n Number of bytes per one sample of low res image data: %d",
      fd->low_res_bytes);
  add(&ret, "\n\n******** end of Trailer File Descriptor (JAXA) record ***********\n");
  return ret;
}

void prn_tfdr(FILE *fp, struct trl_file_des_rec *fd)
{
    char *rec = sprn_tfdr(fd);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
