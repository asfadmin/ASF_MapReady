// Implementation of the interface described in write_meta_and_img.h.

#include "write_meta_and_img.h"

int
write_meta_and_img (const char *outBaseName, meta_parameters *meta,
		    FloatImage *image)
{
  // Determine the name of the output metadata file to write.
  GString *out_meta_file = g_string_new (outBaseName);
  g_string_append (out_meta_file, ".meta");

  // Determine the name of the output data file to write.
  GString *out_data_file = g_string_new (outBaseName);
  g_string_append (out_data_file, ".img");

  // Write the metadata to a file.
  meta_write (meta, out_meta_file->str);
  // Done with the metadata and the file name we wanted to write it in.
  g_string_free (out_meta_file, TRUE);

  // Write the data file itself.
  int return_code = float_image_store (image, out_data_file->str,
				       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  return return_code;
}
