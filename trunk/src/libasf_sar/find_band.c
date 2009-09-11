#include "asf_sar.h"
#include "asf_raster.h"

int find_band(meta_parameters *meta, char *name, int *ok)
{
    const char *rad_name = get_cal_band_name(meta, name);
    int band_num = get_band_number(meta->general->bands,
                                   meta->general->band_count, rad_name);

    if (band_num < 0) {
        asfPrintStatus("Band '%s' not found.\n", name);
        *ok = FALSE;
    }

    return band_num;
}

const char *get_cal_band_name(meta_parameters *meta, char *base)
{
    static char rad_name[32];
    const char *rad;
    switch (meta->general->radiometry) {
      case r_AMP:
        rad = "";
        break;
      case r_SIGMA:
        rad = "SIGMA-";
        break;
      case r_BETA:
        rad = "BETA-";
        break;
      case r_GAMMA:
        rad = "GAMMA-";
        break;
      case r_SIGMA_DB:
        rad = "SIGMA_DB-";
        break;
      case r_BETA_DB:
        rad = "BETA_DB-";
        break;
      case r_GAMMA_DB:
        rad = "GAMMA_DB-";
        break;
      case r_POWER:
        rad = "POWER-";
        break;
      default:
        asfPrintWarning("Unexpected radiometry: %d\n",
                        meta->general->radiometry);
        rad = "";
        break;
    }

    sprintf(rad_name, "%s%s", rad, base);
    return rad_name;
}
