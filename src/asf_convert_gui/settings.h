
typedef struct
{
  /* import */
  int input_data_format;

  /* transformations */
  int data_type;
  int latitude_checked;
  float latitude_low;
  float latitude_hi;

  /* export */
  int output_format;
  int apply_scaling;
  int longest_dimension;
  int output_bytes;
  int scaling_method;

} Settings;

Settings * settings_get_from_gui();
void settings_apply_to_gui();
Settings * settings_copy(const Settings *);
int settings_equal(const Settings *, const Settings *);
const gchar * settings_get_size_argument(const Settings *);
const gchar * settings_get_latitude_argument(const Settings *);
const gchar * settings_get_output_bytes_argument(const Settings *s);
const gchar * settings_get_data_type_string(const Settings *);
const gchar * settings_get_data_type_arg_string(const Settings *);
const gchar * settings_get_input_data_format_string(const Settings *);
const gchar * settings_get_output_format_extension(const Settings *);
const gchar * settings_get_output_format_string(const Settings *);
int settings_get_run_import(const Settings *);
int settings_get_run_export(const Settings *);
void settings_delete(Settings *);

