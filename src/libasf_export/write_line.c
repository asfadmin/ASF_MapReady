#include "asf.h"
#include "asf_export.h"

void write_tiff_byte2byte(TIFF *otif, unsigned char *byte_line,
                          channel_stats_t stats, scale_t sample_mapping,
                          int sample_count, int line)
{
  int jj;
  if (sample_mapping != NONE) {
    for (jj=0; jj<sample_count; jj++) {
      byte_line[jj] =
          pixel_float2byte((float)byte_line[jj], sample_mapping, stats.min, stats.max,
                           stats.hist, stats.hist_pdf, NAN);
    }
  }
  TIFFWriteScanline (otif, byte_line, line, 0);
}

void write_tiff_float2float(TIFF *otif, float *float_line, int line)
{
  TIFFWriteScanline (otif, float_line, line, 0);
}

void write_tiff_float2byte(TIFF *otif, float *float_line,
			   channel_stats_t stats, scale_t sample_mapping,
			   float no_data, int line, int sample_count)
{
  int jj;
  unsigned char *byte_line;

  byte_line = (unsigned char *) MALLOC(sizeof(unsigned char) * sample_count);

  for (jj=0; jj<sample_count; jj++) {
    byte_line[jj] =
      pixel_float2byte(float_line[jj], sample_mapping, stats.min, stats.max,
		       stats.hist, stats.hist_pdf, no_data);
  }
  TIFFWriteScanline (otif, byte_line, line, 0);
  FREE(byte_line);
}

void write_rgb_tiff_byte2byte(TIFF *otif,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
			      int line, int sample_count)
{
  int jj;
  unsigned char *rgb_byte_line;

  rgb_byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_byte_line[jj*3] = red_byte_line[jj];
    rgb_byte_line[(jj*3)+1] = green_byte_line[jj];
    rgb_byte_line[(jj*3)+2] = blue_byte_line[jj];
  }
  TIFFWriteScanline (otif, rgb_byte_line, line, 0);
  FREE(rgb_byte_line);
}

void write_tiff_byte2lut(TIFF *otif, unsigned char *byte_line,
			 int line, int sample_count, char *look_up_table_name)
{
  unsigned char *rgb_line;

  rgb_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  apply_look_up_table(look_up_table_name, byte_line, sample_count,
		      rgb_line);

  TIFFWriteScanline (otif, rgb_line, line, 0);
  FREE(rgb_line);
}

void write_rgb_tiff_float2float(TIFF *otif,
				float *red_float_line,
				float *green_float_line,
				float *blue_float_line,
				int line, int sample_count)
{
  int jj;
  float *rgb_float_line;

  rgb_float_line = (float *) MALLOC(sizeof(float) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_float_line[jj*3] = red_float_line[jj];
    rgb_float_line[(jj*3)+1] = green_float_line[jj];
    rgb_float_line[(jj*3)+2] = blue_float_line[jj];
  }
  TIFFWriteScanline (otif, rgb_float_line, line, 0);
  FREE(rgb_float_line);
}

void write_rgb_tiff_float2byte(TIFF *otif,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int line, int sample_count)
{
  int jj;
  unsigned char *rgb_byte_line;

  rgb_byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_byte_line[jj*3] =
      pixel_float2byte(red_float_line[jj], sample_mapping,
		       red_stats.min, red_stats.max, red_stats.hist,
		       red_stats.hist_pdf, no_data);
    rgb_byte_line[(jj*3)+1] =
      pixel_float2byte(green_float_line[jj], sample_mapping,
		       green_stats.min, green_stats.max, green_stats.hist,
		       green_stats.hist_pdf, no_data);
    rgb_byte_line[(jj*3)+2] =
      pixel_float2byte(blue_float_line[jj], sample_mapping,
		       blue_stats.min, blue_stats.max, blue_stats.hist,
		       blue_stats.hist_pdf, no_data);
  }
  TIFFWriteScanline (otif, rgb_byte_line, line, 0);
  FREE(rgb_byte_line);
}

void write_tiff_float2lut(TIFF *otif, float *float_line,
			  channel_stats_t stats, scale_t sample_mapping,
			  float no_data, int line, int sample_count,
			  char *look_up_table_name)
{
  int jj;
  unsigned char *byte_line, *rgb_line;

  byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count);
  rgb_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    byte_line[jj] =
      pixel_float2byte(float_line[jj], sample_mapping,
		       stats.min, stats.max, stats.hist,
		       stats.hist_pdf, no_data);
  }

  apply_look_up_table(look_up_table_name, byte_line, sample_count,
		      rgb_line);

  TIFFWriteScanline (otif, rgb_line, line, 0);
  FREE(byte_line);
  FREE(rgb_line);
}

void write_jpeg_byte2byte(FILE *ojpeg, unsigned char *byte_line,
                          channel_stats_t stats, scale_t sample_mapping,
			  struct jpeg_compress_struct *cinfo,
			  int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  if (sample_mapping != NONE) {
    for (jj=0; jj<sample_count; jj++) {
      byte_line[jj] =
          pixel_float2byte((float)byte_line[jj], sample_mapping, stats.min, stats.max,
                            stats.hist, stats.hist_pdf, NAN);
    }
  }

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj] = (JSAMPLE) byte_line[jj];
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_jpeg_float2byte(FILE *ojpeg, float *float_line,
			   struct jpeg_compress_struct *cinfo,
			   channel_stats_t stats,
			   scale_t sample_mapping,
			   float no_data, int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj] = (JSAMPLE)
      pixel_float2byte(float_line[jj], sample_mapping, stats.min, stats.max,
		       stats.hist, stats.hist_pdf, no_data);
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_rgb_jpeg_byte2byte(FILE *ojpeg,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
			      struct jpeg_compress_struct *cinfo,
			      int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count * 3);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj*3] = (JSAMPLE) red_byte_line[jj];
    jsample_row[(jj*3)+1] = (JSAMPLE) green_byte_line[jj];
    jsample_row[(jj*3)+2] = (JSAMPLE) blue_byte_line[jj];
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_jpeg_byte2lut(FILE *ojpeg, unsigned char *byte_line,
			 struct jpeg_compress_struct *cinfo,
			 int sample_count, char *look_up_table_name)
{
  int jj;
  unsigned char *rgb_line;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count * 3);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));
  rgb_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  apply_look_up_table(look_up_table_name, byte_line, sample_count,
		      rgb_line);

  for (jj=0; jj<sample_count*3; jj++)
    jsample_row[jj] = (JSAMPLE) rgb_line[jj];

  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_rgb_jpeg_float2byte(FILE *ojpeg,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
			       struct jpeg_compress_struct *cinfo,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count * 3);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj*3] = (JSAMPLE)
      pixel_float2byte(red_float_line[jj], sample_mapping,
		       red_stats.min, red_stats.max, red_stats.hist,
		       red_stats.hist_pdf, no_data);
    jsample_row[(jj*3)+1] = (JSAMPLE)
      pixel_float2byte(green_float_line[jj], sample_mapping,
		       green_stats.min, green_stats.max, green_stats.hist,
		       green_stats.hist_pdf, no_data);
    jsample_row[(jj*3)+2] = (JSAMPLE)
      pixel_float2byte(blue_float_line[jj], sample_mapping,
		       blue_stats.min, blue_stats.max, blue_stats.hist,
		       blue_stats.hist_pdf, no_data);
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_jpeg_float2lut(FILE *ojpeg, float *float_line,
			  struct jpeg_compress_struct *cinfo,
			  channel_stats_t stats, scale_t sample_mapping,
			  float no_data, int sample_count,
			  char *look_up_table_name)
{
  int jj;
  unsigned char *byte_line, *rgb_line;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count * 3);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));
  byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count);
  rgb_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    byte_line[jj] =
      pixel_float2byte(float_line[jj], sample_mapping,
		       stats.min, stats.max, stats.hist,
		       stats.hist_pdf, no_data);
  }

  apply_look_up_table(look_up_table_name, byte_line, sample_count,
		      rgb_line);

  for (jj=0; jj<sample_count*3; jj++)
    jsample_row[jj] = (JSAMPLE) rgb_line[jj];

  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_pgm_byte2byte(FILE *opgm, unsigned char *byte_line,
                         channel_stats_t stats, scale_t sample_mapping,
			 int sample_count)
{
  int jj;
  if (sample_mapping != NONE) {
    for (jj=0; jj<sample_count; jj++) {
      byte_line[jj] =
          pixel_float2byte((float)byte_line[jj], sample_mapping, stats.min, stats.max,
                            stats.hist, stats.hist_pdf, NAN);
    }
  }

  FWRITE(byte_line, sizeof(unsigned char), sample_count, opgm);
}

void write_pgm_float2byte(FILE *opgm, float *float_line,
			  channel_stats_t stats, scale_t sample_mapping,
			  float no_data, int sample_count)
{
  int jj;
  unsigned char *byte_line;

  byte_line = (unsigned char *) MALLOC(sizeof(unsigned char) * sample_count);

  for (jj=0; jj<sample_count; jj++)
    byte_line[jj] =
      pixel_float2byte(float_line[jj], sample_mapping, stats.min, stats.max,
		       stats.hist, stats.hist_pdf, no_data);

  FWRITE(byte_line, sizeof(unsigned char), sample_count, opgm);
  FREE(byte_line);
}
