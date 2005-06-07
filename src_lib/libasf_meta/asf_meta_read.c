#include "libasf_meta.h"

#define METADATA_VERSION 1.4

/***************************************************************
 * asf_meta_read:
 * Reads a meta file (and DDR file).
 * Note that the appropriate extension is appended to the given
 * base name automagically if needed.  */
void asf_meta_read(char *inName, char *requested_version, 
		   meta_parameters **meta, struct DDR **ddr)
{
  char ddrName[255], *meta_version;
  meta_parameters *meta_new, *meta09, *meta1x;
  meta_parameter_t *meta_struct;
  struct DDR *ddr_new=NULL;

  /* Allocate memory */
  meta_version = (char *)MALLOC(5*sizeof(char));

  /* Assign DDR file name */
  create_name(ddrName, inName,".ddr");

  /* Read metadata file and fill it in generic parameter structure */
  meta_struct = extract_meta_parameters(inName);

  /* Read metadata version as string for safe comparison */
  meta_version = metaString(meta_struct, "meta_version:"); 

  /* Check whether version needs to be converted */
  if (strncmp(meta_version, requested_version, 3) != 0) {
    if (strncmp(meta_version, "1.", 2)==0) {
      meta1x = asf_meta_init(meta_version);
      meta1x = asf_meta_read1x(meta_struct);
      if (strncmp(requested_version, "0.9", 3) == 0) {
	meta_new = asf_meta_init("0.9");
	ddr_new = (struct DDR *) MALLOC(sizeof(struct DDR));
	asf_meta1x_to_meta09(meta1x, meta_version, meta_new, ddr_new);
      }
      else
	meta_new = meta1x;
    }
    else if (strncmp(meta_version, "0.9", 3) == 0) {
      if (fileExists(ddrName)) {
	ddr_new = (struct DDR *) MALLOC(sizeof(struct DDR));
	c_getddr(ddrName, ddr_new);
      }
      else 
	asfPrintError("Missing DDR file: '%s'", ddrName);
      meta09 = asf_meta_init("0.9");
      meta09 = asf_meta_read09(meta_struct);
      sprintf(meta_version, "%f", METADATA_VERSION);
      meta_new = asf_meta09_to_meta1x(meta09, ddr_new, meta_version);
    }
  }
  /* Read the requested version */
  else {
    if (strncmp(meta_version, "0.9", 3) == 0) {
      if (fileExists(ddrName)) {
	ddr_new = (struct DDR *) MALLOC(sizeof(struct DDR));
	c_getddr(ddrName, ddr_new);
      }
      else 
	asfPrintError("Missing DDR file: '%s'", ddrName);
      meta_new = asf_meta_init("0.9");
      meta_new = asf_meta_read09(meta_struct);
    }
    else if (strncmp(meta_version, "1.", 2)==0) {
      meta_new = asf_meta_init(meta_version);
      meta_new = asf_meta_read1x(meta_struct);
      ddr_new = NULL;
    }
  }
  
  *meta = meta_new;
  /* Pass back DDR structure when we are actually dealing with 0.9 */
  if (ddr_new)
    *ddr = ddr_new;
  return;

}
