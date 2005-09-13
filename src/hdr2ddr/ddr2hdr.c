#include "asf.h"
#include "ddr.h"
#include "hdr.h"
#include "asf_reporting.h"

void ddr2hdr(struct DDR *ddr, jpl_header *hdr)
{
  // Make sure we've got allocated stuctures
  if (ddr == NULL) {
    asfPrintError("%s: ddr structure has not been allocated!", __func__);
  }
  if (hdr == NULL) {
    asfPrintError("%s: hdr structure has not been allocated!", __func__);
  }

  // Initialize just for fun
  hdr->magnitude_bytes = 0;
  hdr->elevation_bytes = 0;
  strcpy(hdr->data_type, "0");
  hdr->elevation_scale = 0;
  hdr->elevation_shift = 0;
  hdr->line_increment = 0.0;
  hdr->sample_increment = 0.0;
  hdr->start_lat = 0.0;
  hdr->start_lon = 0.0;
  hdr->line_count = 0;
  hdr->sample_count = 0;

  // Okay lets put some believable stuff in there
  hdr->magnitude_bytes = 4;
  hdr->elevation_bytes = 4;
  strcpy(hdr->data_type, "EQA");
  hdr->elevation_scale = 1;
  hdr->elevation_shift = 0;
  if (ddr->valid[DDINCV] == VALID) {
    hdr->line_increment = ddr->line_inc / 1200;
    hdr->sample_increment = ddr->sample_inc / 1200;
  }
  else if (ddr->valid[DDPDV] == VALID) {
    hdr->line_increment = ddr->pdist_y / 3600;
    hdr->sample_increment = ddr->pdist_x / 3600;
  }
  if (ddr->valid[DDPUV] == VALID) {
    hdr->start_lat = ddr->upleft[0] / 3600;
    hdr->start_lon = ddr->upleft[1] / 3600;
  }
  hdr->line_count = ddr->nl;
  hdr->sample_count = ddr->ns;

}
