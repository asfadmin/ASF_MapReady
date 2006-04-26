// See function comment below.

#ifndef WRITE_META_AND_IMG_H
#define WRITE_META_AND_IMG_H

#include <asf_meta.h>
#include <float_image.h>

// Given meta and image, write ASF format '.meta', and '.img' files with
// outBaseName.  Return 0 on success, non-zero on error.
int
write_meta_and_img (const char *outBaseName, meta_parameters *meta,
		    FloatImage *image);

#endif // WRITE_META_AND_IMG_H
