#include "asf.h"
#include "ddr.h"
#include "hdr.h"

void hdr2ddr(jpl_header *hdr, struct DDR *ddr)
{
  // Initialize ddr values
  c_intddr(ddr);

  // Image dimension
  ddr->nl = hdr->line_count;
  ddr->ns = hdr->sample_count;

  // Number of bands; int
  ddr->nbands = 1;

  // Data type; int
  ddr->dtype = DTYPE_FLOAT;

  // Worthless date & time fields; both char[12]
  strcpy (ddr->last_used_date,"");
  strcpy (ddr->last_used_time,"");

  // System byte ordering style; char[12]
  strcpy(ddr->system, "ieee-std");

  // Projection units; char[12]
  strcpy(ddr->proj_units, "seconds");
  ddr->valid[DDPUV] = VALID;

  // Increment per sample in x & y directions; both double
  ddr->line_inc   = fabs(hdr->line_increment) * 1200;
  ddr->sample_inc = fabs(hdr->sample_increment) * 1200;
  ddr->valid[DDINCV] = VALID;

  // Line/sample relative to master image; both int
  ddr->master_line   = 1;
  ddr->master_sample = 1;

  // Projection distance per pixel; both double (pixel size)*/
  ddr->pdist_y = fabs(hdr->line_increment) * 3600;
  ddr->pdist_x = fabs(hdr->sample_increment) * 3600;
  ddr->valid[DDPDV] = VALID;

  // Projection stuff
  ddr->proj_code = 0; // Things are in Geographic
  ddr->zone_code = 0;
  ddr->valid[DDZCV] = VALID;
  ddr->valid[DDPCV] = VALID;

  // Datum Code; int
  ddr->datum_code = 12; // Pulled out of thin air
  ddr->valid[DDDCV] = VALID;

  // Corner Coordinates; all double[2]
  ddr->upleft[0] = hdr->start_lat*3600;
  ddr->upleft[1] = hdr->start_lon*3600;
  ddr->loleft[0] = hdr->start_lat*3600 +
                   hdr->line_count * hdr->line_increment*3600;
  ddr->loleft[1] = hdr->start_lon*3600;
  ddr->upright[0] = hdr->start_lat*3600;
  ddr->upright[1] = hdr->start_lon*3600 +
                    hdr->sample_count * hdr->sample_increment*3600;
  ddr->loright[0] = hdr->start_lat*3600 +
                    hdr->line_count * hdr->line_increment*3600;
  ddr->loright[1] = hdr->start_lon*3600 +
                    hdr->sample_count * hdr->sample_increment*3600;
  ddr->valid[DDCCV] = VALID;

}
