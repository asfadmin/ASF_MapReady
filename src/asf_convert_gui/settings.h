
typedef struct
{
  /* import */
  int input_data_format;

  /* transformations */
  int data_type;
  float latitude_low;
  float latitude_hi;

  /* export */
  int output_format;
  int apply_scaling;
  int longest_dimension;

} Settings;

Settings * settings_get_from_gui();
void settings_apply_to_gui();
Settings * settings_copy(Settings *);
int settings_equal(Settings *, Settings *);
const char * settings_get_size_argument(Settings *);
const char * settings_get_latitude_argument(Settings *);
const char * settings_get_data_type_string(Settings *);
const char * settings_get_input_data_format_string(Settings *);
const char * settings_get_output_format_extension(Settings *);
const char * settings_get_output_format_string(Settings *);
int settings_get_run_import(Settings *);
int settings_get_run_export(Settings *);
