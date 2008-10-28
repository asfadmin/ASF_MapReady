#include "asf_view.h"
#include "asf_geocode.h"
#include <ctype.h>

// returns a pointer to static memory -- don't free or keep
// pointers to!
char *br(const char *s)
{
#define N 512
    static char out[N];

    const int MAX = 28;
    if (strlen(s) < MAX) {
        // no line breaking necessary
        strcpy(out, s);
    } else {
        char *str = STRDUP(s); // because we will need to modify
        char *curr = str;
        char *prev = curr;
        char *next = curr;
        strcpy(out, "");
        while (1) {
            next = strchr(prev, ',');
            if (!next) {
                // reached end of string -- put what we have in output
                if (strlen(out) + strlen(curr) < N-MAX) {
                    if (strlen(out) > 0)
                        strcat(out, ",\n   ");
                    strcat(out, curr);
                }
                break;
            }
            else if (next-curr > MAX) {
                // too many characters -- copy what we have
                if (strlen(out) + strlen(curr) < N-MAX) {
                    if (strlen(out) > 0)
                        strcat(out, ",\n   ");
                    *next = '\0';
                    strcat(out, curr);
                }
                else
                    break; // getting too long
                curr = next+1;
            }
            prev = next+1;
            if (strlen(out) > N-2*MAX) // prevent buffer overrun
                break;                 // plenty of safety margin
        }
        free(str);
    }
    return out;
#undef N
}

static const char *get_data_type_str(data_type_t data_type)
{
  switch (data_type) {
    case BYTE:
      return "Byte";
    case INTEGER16:
      return "Integer16";
    case INTEGER32:
      return "Integer32";
    case REAL32:
      return "32-bit Float";
    case REAL64:
      return "64-bit Float";
    case COMPLEX_BYTE:
      return "Complex Byte";
    case COMPLEX_INTEGER16:
      return "Complex Integer16";
    case COMPLEX_INTEGER32:
      return "Complex Integer32";
    case COMPLEX_REAL32:
      return "Complex 32-bit Float";
    case COMPLEX_REAL64:
      return "Complex 64-bit Float";
    default:
      return "???";
  }
}

static void fix_bad_chars(char *s)
{
  int i,n = strlen(s);
  for (i=0; i<n; ++i) {
    s[i] = s[i]&0x7F;
    if (!isprint(s[i]) && s[i]!='\n')
      s[i]=' ';
  }
}

void fill_meta_info()
{
    char s[1024];
    strcpy(s, "");

    meta_parameters *meta = curr->meta;
    if (meta) {
      if (meta->general) {
          sprintf(&s[strlen(s)],
            "Size: %d x %d (LxS)\n"
            "Data Type: %s\n"
            "Sensor: %s\n"
            "Mode: %s\n"
            "Acquisition Date: %s\n",
                meta->general->line_count, meta->general->sample_count,
                get_data_type_str(meta->general->data_type),
                meta->general->sensor, meta->general->mode,
                meta->general->acquisition_date);

          if (meta->general->orbit != MAGIC_UNSET_INT &&
              meta->general->frame != MAGIC_UNSET_INT)
          {
            sprintf(&s[strlen(s)], "Orbit/Frame: %d/%d\n",
                meta->general->orbit, meta->general->frame);
          }
          sprintf(&s[strlen(s)],
            "Direction: %s\n"
            "Bands: %s\n",
                meta->general->orbit_direction == 'A' ? "Ascending" : "Descending",
                strlen(meta->general->bands) > 0 ? br(meta->general->bands) : "-");
      }
      if (meta->colormap) {
        sprintf(&s[strlen(s)], "Colormap: %s (%d elements)\n",
                (!strncmp(meta->colormap->look_up_table, "UNKNOWN", 7)) ? "(Unnamed)" :
                    meta->colormap->look_up_table, meta->colormap->num_elements);
      }
      sprintf(&s[strlen(s)],"\n");
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
          "Earth Radius: %.0f m\n"
          "Satellite Height: %.0f m\n"
          "    (Above Earth: %.0f m)\n\n",
            meta->sar->look_direction,
            meta->sar->look_count,
            meta->sar->deskewed ? "Yes" : "No",
            meta->sar->range_time_per_pixel,
            meta->sar->azimuth_time_per_pixel,
            meta->sar->slant_range_first_pixel,
            meta->sar->prf,
            meta->sar->earth_radius,
            meta->sar->satellite_height,
            meta->sar->satellite_height-meta->sar->earth_radius);
      }
      if (meta->projection) {
          sprintf(&s[strlen(s)], "%s",
              proj_info_as_string(meta->projection->type,
                                  &meta->projection->param, NULL));
          // if degrees, supply more precision
          if (strcmp_case(meta->projection->units, "degrees") == 0) {
            sprintf(&s[strlen(s)],
                "StartX: %.5f %s\n"
                "StartY: %.5f %s\n"
                "PerX: %.6f %s\n"
                "PerY: %.6f %s\n",
                meta->projection->startX, meta->projection->units,
                meta->projection->startY, meta->projection->units,
                meta->projection->perX, meta->projection->units,
                meta->projection->perY, meta->projection->units);
          } else {
            sprintf(&s[strlen(s)],
                "StartX: %.1f %s\n"
                "StartY: %.1f %s\n"
                "PerX: %.2f %s\n"
                "PerY: %.2f %s\n",
                meta->projection->startX, meta->projection->units,
                meta->projection->startY, meta->projection->units,
                meta->projection->perX, meta->projection->units,
                meta->projection->perY, meta->projection->units);
          }
          sprintf(&s[strlen(s)],
              "Spheriod: %s\n"
              "Datum: %s\n"
              "Height: %f m\n\n",
              spheroid_toString(meta->projection->spheroid),
              datum_toString(meta->projection->datum),
              meta->projection->height);
      }
    }

    fix_bad_chars(s);
    put_string_to_label("meta_label", s);
}

char * escapify(const char * s)
{
    int i,j;
    char * ret = MALLOC(2*strlen(s)*sizeof(char));
    for (i = 0, j = 0; i <= strlen(s); ++i)
    {
        switch(s[i])
        {
            case '\\':
                ret[j] = ret[j+1] = s[i];
                ++j;
                break;
            default:
                ret[j] = s[i];
                break;
        }
        ++j;
    }

    return ret;
}

static char *
find_in_bin(const char * filename)
{
    char * ret = (char *) malloc(sizeof(char) *
        (strlen(get_asf_bin_dir_win()) + strlen(filename) + 5));
    sprintf(ret, "%s%c%s", get_asf_bin_dir_win(), DIR_SEPARATOR, filename);
    return ret;
}

static void mdv_thread (GString *file, gpointer user_data)
{
#ifdef win32
    gchar * mdv = find_in_bin("mdv.exe");
#else
    gchar * mdv = find_in_bin("mdv");
#endif

    char buf[1024];
    char *escaped_str = escapify(file->str);
    sprintf(buf, "\"%s\" \"%s\"", mdv, escaped_str);
    free(escaped_str);
    asfSystem(buf);
    g_string_free(file, TRUE);
}

void open_mdv()
{
    char *f=NULL;

    char *ceos_filename = appendExt(curr->filename, ".L");
    char *palsar_filename = NULL;

    if (fileExists(ceos_filename)) {
        f = STRDUP(ceos_filename);
    }
    else {
        char *dir = MALLOC(sizeof(char)*(strlen(curr->filename)+2));
        char *file_ext = MALLOC(sizeof(char)*(strlen(curr->filename)+2));
        split_dir_and_file(curr->filename, dir, file_ext);
        char *file = stripExt(file_ext);
        FREE(file_ext);

        if (strncmp_case(file, "LED-", 4) == 0) {
            if (fileExists(curr->filename)) {
                f = STRDUP(curr->filename);
            }
        } else if (strncmp_case(file, "IMG-", 4) == 0) {
            palsar_filename = MALLOC(sizeof(char)*(strlen(curr->filename)+10));
            char *p = strchr(file, '-');
            if (p) {
                p = strchr(p+1, '-');
                if (p) {
                    sprintf(palsar_filename, "%sLED-%s", dir, p+1);
                    if (fileExists(palsar_filename)) {
                        f = STRDUP(palsar_filename);
                    }
                }
            }
        } else {
            palsar_filename = MALLOC(sizeof(char)*(strlen(curr->filename)+10));
            sprintf(palsar_filename, "%sLED-%s", dir, file);
            if (fileExists(palsar_filename)) {
                f = STRDUP(palsar_filename);
            } else {
                // anything else we should try?
            }
        }

        FREE(dir);
        FREE(file);
    }

    if (!f) {
        char *msg = MALLOC(sizeof(char)*(strlen(curr->filename)*3+100));
        if (palsar_filename) {
          sprintf(msg,
                  " Couldn't find CEOS metadata file.\n\n"
                  " Was looking for:\n"
                  "    %s\n"
                  " or\n"
                  "    %s\n\n", ceos_filename, palsar_filename);
        }
        else {
          sprintf(msg,
                  " Couldn't find CEOS metadata file.\n\n"
                  " Was looking for:\n"
                  "    %s\n\n", ceos_filename);
        }
        message_box(msg);
        free(msg);
    }
    else {
        asfPrintStatus("Opening in MDV: %s\n", f);
        static GThreadPool *ttp = NULL;
        GError *err = NULL;

        if (!ttp)
        {
            if (!g_thread_supported ()) g_thread_init (NULL);
            ttp = g_thread_pool_new ((GFunc) mdv_thread, NULL, 4, TRUE, &err);
            g_assert(!err);
        }
        g_thread_pool_push (ttp, g_string_new (f), &err);
        g_assert(!err);

        free(f);
    }

    FREE(ceos_filename);
    FREE(palsar_filename);
}

SIGNAL_CALLBACK void on_mdv_button_clicked(GtkWidget *w)
{
    open_mdv();
}

void disable_meta_button_if_necessary()
{
#ifdef win32
    gchar * mdv = find_in_bin("mdv.exe");
#else
    gchar * mdv = find_in_bin("mdv");
#endif

    enable_widget("mdv_button", fileExists(mdv));
}
