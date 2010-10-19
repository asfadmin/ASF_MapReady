#include "asf_tiff.h"

static TIFFExtendProc _ParentExtender = NULL;
static TIFFFieldInfo xtiffFieldInfo[] = {
    { TIFFTAG_ASF_INSAR_METADATA, TIFF_VARIABLE, TIFF_VARIABLE, TIFF_ASCII, FIELD_CUSTOM, TRUE, FALSE, "ASF Metadata" } 
  };

static void
_XTIFFDefaultDirectory(TIFF *tif)
{
  /* Install the extended Tag field info */
  TIFFMergeFieldInfo(tif, xtiffFieldInfo, N(xtiffFieldInfo));

  /* Since an XTIFF client module may have overridden
   * the default directory method, we call it now to
   * allow it to set up the rest of its own methods.
   */

  if (_ParentExtender) 
    (*_ParentExtender)(tif);
}

void _XTIFFInitialize(void)
{
    static int first_time=1;
  
    if (! first_time) return; /* Been there. Done that. */
    first_time = 0;
  
    /* Grab the inherited method and install */
    _ParentExtender = TIFFSetTagExtender(_XTIFFDefaultDirectory);
}


