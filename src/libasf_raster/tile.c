#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

void create_image_tiles(char *inFile, char *outBaseName, int tile_size)
{
  FILE *fpIn, *fpOut;
  meta_parameters *metaIn, *metaOut;
  char *outFile;
  int row, column;
  int start_line, start_sample, num_lines_to_get, num_samples_to_get;
  int line_count, sample_count, tiles_row, tiles_column;
  long long pixel_count;
  float *data_buffer;
  double min, max, mean, stdDev;

  create_clean_dir(outBaseName);
  outFile = (char *) MALLOC(sizeof(char)*512);

  // Calculate how many tiles we need
  metaIn = meta_read(inFile);
  metaOut = meta_read(inFile);
  line_count = metaIn->general->line_count;
  sample_count = metaIn->general->sample_count;
  tiles_row = line_count / tile_size + 1;
  tiles_column = sample_count / tile_size + 1;

  fpIn = FOPEN(inFile, "rb");

  // Generate tiles with the appropriate metadata
  for (row=0; row<tiles_row; row++)
    for (column=0; column<tiles_column; column++) {

      // Work the number for the tile
      sprintf(outFile, "%s/%s_%d_%d.img", outBaseName, outBaseName, row, column);
      start_line = row * tile_size;
      start_sample = column * tile_size;
      num_lines_to_get = ((start_line + tile_size) < line_count) ? 
	tile_size : (line_count - start_line);
      num_samples_to_get = ((start_sample + tile_size) < sample_count) ?
	tile_size : (sample_count - start_sample);
      pixel_count = num_lines_to_get * num_samples_to_get;

      // Take care of the metadata
      metaOut->general->start_line = start_line;
      metaOut->general->start_sample = start_sample;
      metaOut->general->line_count = num_lines_to_get;
      metaOut->general->sample_count = num_samples_to_get;
      meta_get_corner_coords(metaOut);

      // Read tile from input file
      data_buffer = (float *) MALLOC(sizeof(float)*pixel_count);
      get_partial_float_lines(fpIn, metaIn, start_line, num_lines_to_get,
			      start_sample, num_samples_to_get, data_buffer);

      // Do the statistics to detect zero fill tiles
      calc_stats(data_buffer, pixel_count, NAN, &min, &max, &mean, &stdDev);

      // Write tile to output file if not complete zero fill
      if (min > 0.0 && max > 0.0) {
        //printf("made it here!\n");
	meta_write(metaOut, outFile);
	fpOut = FOPEN(outFile, "wb");
	put_float_lines(fpOut, metaOut, 0, num_lines_to_get, data_buffer);
	FCLOSE(fpOut);
      }

      // Clean up
      FREE(data_buffer);
    }

  FCLOSE(fpIn);
  meta_free(metaIn);
  meta_free(metaOut);
}
