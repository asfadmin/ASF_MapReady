#include "asf_meta.h"

void meta_set_no_data(const char *metaFile, float no_data) 
{
  meta_parameters *meta;

  meta = meta_read(metaFile);
  meta->general->no_data = no_data;
  meta_write(meta, metaFile);
  meta_free(meta);

  return;
}
