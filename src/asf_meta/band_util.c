#include "asf.h"
#include "asf_meta.h"

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
  meta_parameters *meta = meta_read(file);
  if (!meta)
    asfPrintError("remove_band: could not read metadata: %s\n", file);

  if (meta->general->band_count == 1) {
    asfPrintWarning("remove_band: Won't remove the only band!\n");
    return;
  }
  if (band < 0 || band > meta->general->band_count) {
    asfPrintWarning("remove_band: Tried to remove non-existent band.\n");
    return;
  }

  asfPrintStatus("Removing band %d from %s.\n", band, file);

  char *src = appendToBasename(file, "_orig");
  copyImgAndMeta(file, src);

  if (!save_orig)
    cleanImgAndMeta(file);

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

  // the only thing we need update in the metadata is the band info
  meta->general->band_count = nb-1;

  char *p = meta->general->bands;
  char *new_band_str = MALLOC(sizeof(char)*(strlen(p)+1));
  strcpy(new_band_str, "");

  int n = 0;
  do {
    char *q = strchr(p, ',');
    if (q) *q = '\0'; // don't care if we clobber existing string
    if (n++ != band) {
      strcat(new_band_str, p);
      strcat(new_band_str, ",");
    }
    p = q ? q+1 : NULL;
  } while (p);
  if (strlen(new_band_str)==0) // just in case... though this shouldn't happen
    strcpy(new_band_str, MAGIC_UNSET_STRING);
  if (new_band_str[strlen(new_band_str)-1] == ',') // strip trailing comma
    new_band_str[strlen(new_band_str)-1] = '\0';

  strcpy(meta->general->bands, new_band_str);
  meta_write(meta, file);

  FREE(new_band_str);
  meta_free(meta);

  FREE(src);

  asfPrintStatus("Done.\n\n");
}

