#include "ssv.h"
#include "asf_geocode.h"

void fill_meta_info()
{
    char s[1024];
    strcpy(s, "");

    if (meta) {
      if (meta->general) {
        sprintf(&s[strlen(s)],
          "Size: %d x %d (LxS)\n"
          "Sensor/Mode: %s/%s\n"
          "Acquisition Date: %s\n"
          "Orbit/Frame: %d/%d\n"
          "Direction: %s\n"
          "Bands: %s\n\n",
            meta->general->line_count, meta->general->sample_count,
            meta->general->sensor, meta->general->mode,
            meta->general->acquisition_date,
            meta->general->orbit, meta->general->frame,
            meta->general->orbit_direction == 'A' ? "Ascending" : "Descending",
            strlen(meta->general->bands) > 0 ? meta->general->bands : "-");
      }
      if (meta->sar) {
        // don't show the polarization for ALOS data, it is usually wrong...
        if (strcmp_case(meta->general->sensor, "ALOS") != 0) {
          sprintf(&s[strlen(s)], "Polarization: %s\n", meta->sar->polarization);
        }

        sprintf(&s[strlen(s)],
          "Look Direction: %c\n"
          "Look Count: %d\n"
          "Deskewed: %s\n"
          "Range Time/Pixel: %e s\n"
          "Azimuth Time/Pixel: %f s\n"
          "Slant to First Pixel: %.1f m\n"
          "PRF: %.1f Hz\n"
          "Satellite Height: %.0f m\n\n",
            meta->sar->look_direction,
            meta->sar->look_count,
            meta->sar->deskewed ? "Yes" : "No",
            meta->sar->range_time_per_pixel,
            meta->sar->azimuth_time_per_pixel,
            meta->sar->slant_range_first_pixel,
            meta->sar->prf,
            meta->sar->satellite_height);
      }
      if (meta->projection) {
          sprintf(&s[strlen(s)],
            "%s"
            "StartX: %.1f %s\n"
            "StartY: %.1f %s\n"
            "PerX: %.2f %s\n"
            "PerY: %.2f %s\n"
            "Spheriod: %s\n"
            "Datum: %s\n"
            "Height: %f m\n\n",
              proj_info_as_string(meta->projection->type, &meta->projection->param),
              meta->projection->startX, meta->projection->units,
              meta->projection->startY, meta->projection->units,
              meta->projection->perX, meta->projection->units,
              meta->projection->perY, meta->projection->units,
              spheroid_toString(meta->projection->spheroid),
              datum_toString(meta->projection->datum),
              meta->projection->height);
      }
    }

    put_string_to_label("lower_label", s);
}

