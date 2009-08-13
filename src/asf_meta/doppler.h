// Doppler header file.

#ifndef _doppler_h_
#define _doppler_h_

typedef enum {
  unknown_doppler=0,
  tsx_doppler,              // TerraSAR: polynominal for time stamps
  radarsat2_doppler         // Radarsat2: polynomial as function of slant range
} doppler_type;

typedef struct {
  double time;              // Time relative to first Doppler estimate time
  double first_range_time;  // Time first range pixel
  double reference_time;    // Reference point time of polynomial
  int poly_degree;          // Degree of polynomial
  double *coefficient;      // Polynomial coefficient
} tsx_doppler_t;

typedef struct {
  int year;                 // Year for first Doppler estimate
  int julDay;               // Julian day of year for first Doppler estimate
  double second;            // Seconds of day for first Doppler estimate
  int doppler_count;        // Number of Doppler estimates
  tsx_doppler_t *dop;       // Array sized at run-time
} tsx_doppler_params;

typedef struct {
  double ref_time_centroid; // Reference time for centroid (t0)
  double ref_time_rate;     // Reference time for rate (t0)
  int doppler_count;        // Number of Doppler estimates
  double *centroid;         // Polynomial coefficients for Doppler centroid
  double *rate;             // Polynomial coefficients for Doppler rate
  double time_first_sample; // Slant range time for first range sample
} radarsat2_doppler_params;

#endif
