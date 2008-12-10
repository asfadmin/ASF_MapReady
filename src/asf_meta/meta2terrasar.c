#include "asf_meta.h"
#include "dateUtil.h"
#include "terrasar.h"
#include "asf_nan.h"

meta_parameters* terrasar2meta(terrasar_meta *terrasar)
{
  meta_parameters *meta;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  meta->general->data_type = REAL32;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  strcpy(meta->general->system, "big_ieee");
  meta->general->band_count = terrasar->numberOfLayers;
  meta->general->line_count = terrasar->numberOfRows;
  meta->general->sample_count = terrasar->numberOfColumns;
  meta->general->x_pixel_size = terrasar->groundRangeResolution;
  meta->general->y_pixel_size = terrasar->azimuthResolution;
    
  return meta;
}
