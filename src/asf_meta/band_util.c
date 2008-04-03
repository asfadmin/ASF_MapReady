#include "asf.h"
#include "asf_meta.h"

static void remove_file(const char * file)
{
  if (fileExists(file))
    unlink(file);
}

// attempt to remove "<file>.img" and "<file>.meta", etc files
static void cleanImgAndMeta(const char *file)
{
    if (file)
    {
        char * img_file = appendExt(file, ".img");
        char * meta_file = appendExt(file, ".meta");

        remove_file(img_file);
        remove_file(meta_file);
        remove_file(file);

        free(img_file);
        free(meta_file);
    }
}

void remove_band(const char *file, int band, int save_orig)
{
  if (!fileExists(file))
    asfPrintError("remove_band: file not found: %s\n", file);

  char *src = appendToBasename(file, "_orig");
  copyImgAndMeta(file, src);

  if (!save_orig)
    cleanImgAndMeta(file);

  meta_parameters *meta = meta_read(src);

  if (!meta)
    asfPrintError("remove_band: could not read metadata: %s\n", src);

  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int nb = meta->general->band_count;

  float *buf = MALLOC(sizeof(float)*ns);

  FILE *ifp = fopenImage(src, "rb");
  FILE *ofp = fopenImage(file, "wb");

  int ib;  // input band
  int ob;  // output band
  int l;   // line (within the band)

  for (ib=0,ob=0; ib<nb; ++ib) {
    if (ib != band) {
      for (l=0; l<nl; ++l) {
        get_band_float_line(ifp,meta,ib,l,buf);
        put_band_float_line(ofp,meta,ob,l,buf);
      }
      ++ob;
    }
  }

  FCLOSE(ifp);
  FCLOSE(ofp);
  FREE(buf);
}

