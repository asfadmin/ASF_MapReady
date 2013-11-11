#include "asf.h"
#include "asf_meta.h"
#include "asf_import.h"
#include "asf_endian.h"
#include "hdf5.h"
#include "asf_iso_meta.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>

// This is hard coded for now
#define SEASAT_DATASET "/data/HH"

void import_seasat_h5(const char *inBaseName, const char *outBaseName)
{
  char *out_meta_filename = appendExt(outBaseName, ".meta");
  char *in_xml_filename = appendExt(inBaseName, ".xml");

  asfPrintStatus("\nReading %s\n", in_xml_filename);
  iso_meta *iso = iso_meta_read(in_xml_filename);
  meta_parameters *meta = iso2meta(iso);
  iso_meta_free(iso);

  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  meta->sar->original_line_count = nl;
  meta->sar->original_sample_count = ns;

  asfPrintStatus("Writing %s\n", out_meta_filename);
  meta_write(meta, outBaseName);

  FREE(in_xml_filename);
  FREE(out_meta_filename);

  char *h5_filename = appendExt(inBaseName, ".h5");
  char *img_filename = appendExt(outBaseName, ".img");

  hid_t file = H5Fopen(h5_filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  hid_t dset = H5Dopen(file, SEASAT_DATASET, H5P_DEFAULT);
  hid_t space = H5Dget_space(dset);

  int ndims = H5Sget_simple_extent_dims(space, NULL, NULL);
  if (ndims != 2)
    asfPrintError("Cannot ingest non-2D data (ndims=%d)\n", ndims);

  hsize_t dims[2];
  ndims = H5Sget_simple_extent_dims(space, dims, NULL);
  if (dims[0] != nl || dims[1] != ns)
    asfPrintError("Size mismatch between metadata and data:\n"
                  "  xml: %dx%d, h5: %dx%d\n", nl, ns, dims[0], dims[1]);

  asfPrintStatus("\nImage is %dx%d LxS\n\n", nl, ns);  
  float *dest = MALLOC(sizeof(float)*nl*ns);

  asfPrintStatus("Reading %s\n", h5_filename);
  H5Dread(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dest);

  H5Dclose(dset);
  H5Sclose(space);
  H5Fclose(file);

  asfPrintStatus("Writing %s\n", img_filename);
  FILE *fpOut = FOPEN(img_filename, "wb");
  put_float_lines(fpOut, meta, 0, nl, dest);
  fclose(fpOut);

  FREE(dest);
  FREE(h5_filename);
  FREE(img_filename);
  meta_free(meta);
}

