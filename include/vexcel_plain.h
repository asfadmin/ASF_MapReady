#ifndef __GAMMA_H__
#define __GAMMA_H__

#include "asf_meta.h"

// Structure of the Vexcel Plain format
typedef struct {
  double I_mean;
  double Q_mean;
  double I_std;
  double Q_std;
  double IQ_corr;
} vp_iq_statistics;

typedef struct {
  char *polarization;
  double polarization_amplitude;
  double polarization_phase;
  int stc_pattern_id;
  vp_iq_statistics iq_statistics;
} vp_polarization;

typedef struct {
  int nr_polarizations;
  vp_polarization *polarization;
} vp_polarization_block;

typedef struct {
  double reference_first_dimension;
  double reference_second_dimension;
  int number_of_coefficients_first_dimension;
  int number_of_coefficients_second_dimension;
  double *a;
} vp_coefficients;

typedef struct {
  vp_coefficients doppler_centroid_coefficients;
  double reference_range;
  char *reference_date;
  int ambiguity_number;
  int MLCC_ambiguity_number_occurence;
  int MLBF_ambiguity_number_occurence;
  double DAR_doppler;
  double Predict_doppler;
  double DAR_confidence;
  double doppler_fit_correlation;
  char *doppler_status;
} vp_doppler_centroid_parameters;

typedef struct {
  vp_coefficients effective_velocity_coefficients;
  double veff;
  double reference_range;
  char *reference_date;
  double autofocus_scale_factor;
  double autofocus_snr;
  double autofocus_suggested_ambiguity_number;
  char *autofocus_status;
} vp_doppler_rate_parameters;

typedef struct {
  char *beam_name;
  int nr_of_samples;
  double echo_delay;
  double carrier_freq;
  double sampling_freq;
  double prf;
  double chirp_rate;
  double pulse_length;
  double look_angle;
  double incidence_angle;
  double range_spectrum_snr;
  double replica_energy_ref_level;
  double cal1_cal2_diff_ref_level;
  double thermal_noise_ref_level;
  double gain_corctn_factor;
  double gain_scale;
  vp_polarization_block polarization_block;;
  vp_doppler_centroid_parameters doppler_centroid_parameters;
  vp_doppler_rate_parameters doppler_rate_parameters;
} vp_beam;

typedef struct {
  double x;
  double y;
  double z;
  double xv;
  double yv;
  double zv;
  char *date;
} vp_state_vector;

typedef struct {
  int nr_sv;
  vp_state_vector *state_vector;
} vp_sv_block;

typedef struct {
  double reference;
  int number_of_coefficients;
  double *a;
} vp_poly;

typedef struct {
  double yaw;
  double roll;
  double pitch;
  char *date;
  vp_poly yawpoly;
  vp_poly rollpoly;
  vp_poly pitchpoly;
} vp_attitude;

typedef struct {
  double angle;
  char *date;
} vp_gha;

typedef struct {
  vp_sv_block sv_block;
  vp_attitude attitude;
  int orbit_nr;
  char *orbit_nr_date;
  vp_gha gha;
  char *type;
} vp_ephemeris;

typedef struct {
  double lat;
  double lon;
  double height;
} vp_location;

typedef struct {
  char *name;
  char *ellipsoid_name;
  double major;
  double minor;
  double terrain_height;
  double mass;
  double delta_x;
  double delta_y;
  double delta_z;
  double g;
  double j2;
  double j3;
  double j4;
} vp_earth_model;

typedef struct {
  vp_earth_model earth_model;
  vp_location first_line_first_pixel;
  vp_location first_line_last_pixel;
  vp_location last_line_first_pixel;
  vp_location last_line_last_pixel;
  vp_location center_line_center_pixel;
} vp_coord;

typedef struct {
  char *reference_date;
  double reference_range;
  int number_of_coefficients;
  double *a;
} vp_gr2sr;

typedef struct {
  int nr_gr2sr;
  vp_gr2sr *gr2sr;
} vp_gr2sr_block;

typedef struct {
  char *facility;
  char *format;
  char *type;
  int bytes_per_pixel;
  char *title;
  double pixel_spacing;
  double pixel_resolution;
  double line_spacing;
  double line_resolution;
  int nr_pixels;
  int nr_lines;
  double min_value;
  double max_value;
  double mean_value;
  double sigma_value;
  double mean_intensity_value;
  double sigma_intensity_value;
  vp_coord coord;
} vp_image_desc;

typedef struct {
  char *sensor_name;
  char *instrument_name;
  char *instrument_type;
  double clock_angle;
  int nr_temperatures;
  int nr_beams;
  vp_beam *beam;
  vp_ephemeris ephemeris;
} vp_sensor;

typedef struct {
  vp_image_desc image_desc;
  char *processor_name;
  char *processor_version;
  char *first_line;
  char *first_line_txpol;
  double time_per_line;
} vp_raw_sar_image;

typedef struct {
  vp_image_desc image_desc;
  char *processor_name;
  char *processor_version;
  char *image_type;
  char *polarization;
  char *first_line;
  double time_per_line;
  double orbit_nr;
  char *orbit_nr_date;
  double track_angle;
  double near_range;
  double center_range;
  double far_range;
  double skew_flag;
  double kaiser_range;
  double kaiser_azimuth;
  int range_looks;
  int azimuth_looks;
  int range_block_average_factor;
  int azimuth_block_average_factor;
  int calibration_mode;
  int scan_id;
  vp_gr2sr_block gr2sr_block;
  int range_FFT_size;
  double azimuth_offset;
  double processor_bandwidth;
  double kaiser_multilook;
  double bandwidth_per_look;
  double multi_look_overlap;
  double output_range_pixel_spacing;
  double output_azimuth_pixel_spacing;
  double eff_azimuth_looks;
  double total_multilook_bw;
  int raw_start_line;
  int nr_raw_lines;
  int raw_start_pixel;
  int nr_raw_pixels;
} vp_gli_product;

typedef struct {
  vp_sensor sensor;
  char *flight_path_direction;
  vp_raw_sar_image raw_sar_image;
  vp_gli_product gli_product;
} vexcel_plain;

// Function prototypes
meta_parameters* vp2meta(vexcel_plain *vp);

#endif
