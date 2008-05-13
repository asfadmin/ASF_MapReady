#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_jaxa_facdr(struct JAXA_FACDR *fd, int length)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  int ii;

  add(&ret, "\n******** begin of Facility Related Data (JAXA) record *********\n");
  add(&ret, "\n FACDR sequence number\t\t%d", fd->seqence_number);
  for (ii=0; ii<10; ii++)
    add(&ret, "\n Map to line/sample coefficients: a[%d]\t\t%20.10lf", 
	ii, fd->a_map[ii]);
  for (ii=0; ii<10; ii++)
    add(&ret, "\n Map to line/sample coefficients: b[%d]\t\t%20.10lf", 
	ii, fd->b_map[ii]);
  add(&ret, "\n Calibration data indicator\t\t%d", fd->cal_data_indicator);
  add(&ret, "\n Start line of calibration at upper image\t\t%d", 
      fd->start_line_up);
  add(&ret, "\n Stop line of calibraiton at upper image\t\t%d", fd->stop_line_up);
  add(&ret, "\n Start line of calibration at bottom image\t\t%d", 
      fd->start_line_bottom);
  add(&ret, "\n Stop line of calibraiton at bottom image\t\t%d", 
      fd->stop_line_bottom);
  add(&ret, "\n PRF switching indicator\t\t%d", fd->prf_switching_indicator);
  add(&ret, "\n Line locator of PRF switching\t\t%d", fd->line_prf_switching);
  add(&ret, "\n SIGMA-SAR processing start line number\t\t%d", 
      fd->sigma_start_line);
  add(&ret, "\n Number of loss lines (level 1.0)\t\t%d", fd->number_loss_lines_L0);
  add(&ret, "\n Number of loss lines (level 1.1, 1.5)\t\t%d", 
      fd->number_loss_lines_L1);
  if (length == 5000) {
    for (ii=0; ii<25; ii++)
      add(&ret, "\n Line/sample to lat/lon coefficients: a[%d]\t\t%g",
	  ii, fd->a[ii]);
    for (ii=0; ii<25; ii++)
      add(&ret, "\n Line/sample to lat/lon coefficients: b[%d]\t\t%g", 
	  ii, fd->b[ii]);
    add(&ret, "\n Origin pixel (P0)\t\t%20.10lf", fd->origin_pixel);
    add(&ret, "\n Origin line (L0)\t\t%20.10lf", fd->origin_line);
    for (ii=0; ii<25; ii++)
      add(&ret, "\n Lat/lon to line/sample coefficients: c[%d]\t\t%g", 
	  ii, fd->c[ii]);
    for (ii=0; ii<25; ii++)
      add(&ret, "\n Lat/lon to line/sample coefficients: d[%d]\t\t%g", 
	  ii, fd->d[ii]);
    add(&ret, "\n Origin latitude [degrees]\t\t%20.10lf", fd->origin_lat);
    add(&ret, "\n Origin longitude [degrees]\t\t%20.10lf", fd->origin_lon);
  }

  add(&ret, "\n******** end of Facility Related Data (JAXA) record ***********\n");
  return ret;
}

void prn_jaxa_facdr(FILE *fp, struct JAXA_FACDR *fd, int length)
{
    char *rec = sprn_jaxa_facdr(fd, length);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
