#include "libasf_meta.h"

#define METADATA_VERSION 1.3

void asf_meta_write(meta_parameters *meta, char *requested_version, struct DDR *ddr,
		    char *outName)
{
  char ddrName[255], meta_version[10];
  meta_parameters *meta09=NULL, *meta1x=NULL;

  /* Assign DDR file name */
  create_name(ddrName, outName, ".ddr");

  /* Check whether version needs to be converted */
  sprintf(meta_version, "%.1f", meta->meta_version);
  if (strncmp(meta_version, requested_version, 2) != 0) {
    if (strncmp(meta_version, "1.", 2) == 0) {
      meta09 = asf_meta_init("0.9");
      ddr = (struct DDR *) MALLOC(sizeof(struct DDR));
      asf_meta1x_to_meta09(meta, meta_version, meta09, ddr);
      asf_meta_write09(meta09, outName);
      c_putddr(ddrName, ddr);
    }
    else if (strncmp(meta_version, "0.9", 3) == 0) {
      meta1x = asf_meta09_to_meta1x(meta, ddr, requested_version);
      asf_meta_write1x(meta1x, requested_version, outName);
    }
  }
  /* Write out the requested version */
  else {
    if (strcmp(requested_version, "0.9") == 0) {
      asf_meta_write09(meta, outName);
      c_putddr(ddrName, ddr);
    }
    else if (strcmp(requested_version, "1.1") == 0 || 
	     strcmp(requested_version, "1.2") == 0 ||
	     strcmp(requested_version, "1.3") == 0 ||
	     strcmp(requested_version, "1.4") == 0)
      asf_meta_write1x(meta, requested_version, outName);
  }  
  
  return;

}
