#include "asf_convert_gui.h"

typedef struct
{
  /* import */
  int input_data_format;

  /* transformations */
  int data_type;
  int output_db;
  int latitude_checked;
  double latitude_low;
  double latitude_hi;

  /* export */
  int export_is_checked;
  int output_format;
  int apply_scaling;
  int longest_dimension;
  int output_bytes;
  int scaling_method;

  /* geocode */
  int geocode_is_checked;
  int projection;
  int zone;
  double plat1;
  double plat2;
  double lat0;
  double lon0;
  double false_easting;
  double false_northing;

  int specified_height;
  double height;
  int specified_pixel_size;
  double pixel_size;
  int datum;
  int resample_method;

  /* misc */
  int keep_files;
  int apply_metadata_fix;
}
Settings;

Settings * settings_get_from_gui();
void settings_apply_to_gui();
Settings * settings_copy(const Settings *);
int settings_equal(const Settings *, const Settings *);
const gchar * settings_get_size_argument(const Settings *);
const gchar * settings_get_latitude_argument(const Settings *);
const gchar * settings_get_apply_metadata_fix_argument(const Settings *);
const gchar * settings_get_output_bytes_argument(const Settings *s);
const gchar * settings_get_data_type_string(const Settings *);
const gchar * settings_get_data_type_arg_string(const Settings *);
const gchar * settings_get_input_data_format_string(const Settings *);
const gchar * settings_get_output_format_extension(const Settings *);
const gchar * settings_get_output_format_string(const Settings *);
const gchar * settings_get_geocode_options(const Settings *);
const gchar * settings_get_projection_abbrev(const Settings *);
int settings_get_run_import(const Settings *);
int settings_get_run_export(const Settings *);
int settings_get_run_geocode(const Settings *);
int  settings_get_output_format_can_be_thumbnailed(const Settings *s);
void settings_delete(Settings *);

