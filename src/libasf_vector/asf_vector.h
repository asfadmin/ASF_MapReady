#ifndef _ASF_VECTOR_H_
#define _ASF_VECTOR_H_
/******************************************************************************
NAME:
 asf_vector.h

DESCRIPTION:
 C header with definitions & prototypes for libasf_vector library

******************************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "shapefil.h"
#include "asf_import.h"

// NOTE: The META format type applies to both ASF metadata files and all
// leader data files (.L, .par, LED- etcetera)
typedef enum {
  META=1,
  LEADER,
  KMLFILE,
  SHAPEFILE,
  TEXT,
  POINT,
  POLYGON,
  SMAP_BOUNDARY,
  RGPS,
  RGPS_GRID,
  RGPS_WEATHER,
  GRANULE_COUNT,
  GRANULE_LIST,
  GRANULE_DETAILS_A3,
  GRANULE_DETAILS,
  FOOT_PRINT,
  MULTIMATCH,
  URSA,
  DATAPOOL,
  GEOTIFF_META,
  AUIG,
  HAP,
  TERRASAR_META,
  STF_META,
  CSV,
  CUSTOM_FORMAT
} format_type_t;

typedef enum {
  CSV_UNKNOWN=0,
  CSV_STRING,
  CSV_DOUBLE,
  CSV_INTEGER,
  CSV_LOGICAL,
  CSV_DATE
} column_data_type_t;

typedef struct {
  char column_name[64];
  column_data_type_t data_type;
  int column_number;
} csv_meta_column_t;

typedef struct {
  char column_name[64];
  int column_number;
  int is_lat; // TRUE for lat, FALSE for lon
} csv_data_column_t;

// Database fields
typedef enum {
  DBF_STRING=1,
  DBF_DOUBLE,
  DBF_INTEGER
} dbf_format_t;

// Generic CSV structure
typedef struct {
  char header[255];
  dbf_format_t format;
  int visible;
} dbf_header_t;

// RGPS grid point definition
typedef struct {
  long cell_id;
  long grid_id;
  int ordering;
  double lat;
  double lon;
  int alive;
} grid_t;

typedef struct {
  long grid_id;
  char date[20];
  double day;
  double grid_x;
  double grid_y;
  double lat;
  double lon;
  char sourceImage[20];
  char targetImage[20];
  char stream[3];
  int quality;
  int alive;
  int done;
} grid_attr_t;

// RGPS cell definition
typedef struct {
  long cell_id;
  int nVertices;
  char date[20];
  double day;
  char sourceImage[20];
  char targetImage[20];
  char stream[3];
  double area;
  double multi_year_ice;
  double open_water;
  double incidence_angle;
  double cell_x;
  double cell_y;
  double dudx;
  double dudy;
  double dvdx;
  double dvdy;
  double dtp;
  double temperature;
  double u_wind;
  double v_wind;
  int alive;
} cell_t;


// Prototype from config.c
typedef struct
{
  char comment[255];             // first line for comments
  char directory[1024];          // default directory
  char input_file[512];          // input file name
  char output_file[512];         // output file name
  char input_format[25];         // input format
  char output_format[25];        // output format
  char overlay[512];             // overlay file name
  double north;                  // northern extent of the overlay
  double south;                  // southern extent of the overlay
  double east;                   // eastern extent of the overlay
  double west;                   // western extent of the overlay
  int transparency;              // transparency of the overlay
  int list;                      // list of files flag
  int time;                      // time series flag
  int stack;                     // stacking flag
  char boundary[25];             // polygon/line
  char altitude[25];             // ref: clampToGround/relativeToGround/absolute
  int height;                    // height
  int range;                     // range
  int width;                     // width of boundary line
  char color[25];                // color of boundary line
  int short_config;              // short configuration file flag
  char header_file[512];         // header file location - for testing only    
} c2v_config;

int init_c2v_config(char *configFile);
c2v_config *init_fill_c2v_config();
c2v_config *read_c2v_config(char *configFile);
int write_c2v_config(char *configFile, c2v_config *cfg);

// Prototypes from convert2vector.c
format_type_t str2format(const char *str);
char *format2str(format_type_t format);
int convert2vector(c2v_config *cfg);

// Prototypes from kml_utils.c
void kml_header(FILE *kml_file);
void kml_entry_with_overlay(FILE *kml_file, meta_parameters *meta,
                            char *name, char *ceos_fileame,
                            char *jpeg_dir);
void kml_entry(FILE *kml_file, meta_parameters *meta, char *name);
void kml_entry_ext(FILE *kml_file, meta_parameters *meta, char *name, 
		   c2v_config *cfg);
void kml_point_entry(FILE *kml_file, char *name, float lat, float lon);
void kml_polygon_entry(FILE *kml_file, char *name, char **id, float *lat,
               float *lon, int n);
void kml_footer(FILE *kml_file);
void write_kml_style_keys(FILE *kml_file);
const char *altitude_mode();

// Prototypes from shape_utils.c
void shape_generic_init(char *inFile, dbf_header_t *dbf, int nColumns,
			format_type_t format);
void shape_init(char *inFile, format_type_t format);
int read_shape(char *inFile, char *outFile, format_type_t format, int list);
int write_shape(char *inFile, char *outFile, format_type_t format, int list);
void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape);
void close_shape(DBFHandle dbase, SHPHandle shape);
void write_esri_proj_file(char *inFile);
void write_asf2esri_proj(meta_parameters *meta, char *projFile, char *outFile);

// Prototypes from utils.c
void meta2text(char *inFile, FILE *outFP);
void geotiff2text(char *inFile, FILE *outFP);
int ismetadata(char *inFile);
int isleader(char *inFile);
int ispoint(char *inFile);
int ispolygon(char *inFile);
int isshape(char *inFile);
int isgeotiff(char *inFile);
int isrgps(char *inFile);
int isparfile(char *inFile);

// Prototypes from header.c
int isVisible(dbf_header_t *dbf, int nCols, char *header);
char *get_column(char *line, int column);
int get_number_columns(char *line);
int read_header_config(const char *format, dbf_header_t **dbf, int *nColumns);
int read_header_config_ext(const char *format, dbf_header_t **dbf, 
			   int *nColumns, char *header_file);

// Prototypes from meta.c
int meta2csv(char *inFile, char *outFile, int listFlag);
int meta2kml(char *inFile, char *outFile, format_type_t inFormat, 
	     c2v_config *cfg);
void shape_meta_init(char *inFile, meta_parameters *meta);
int meta2shape(char *inFile, char *outFile, int listFlag);
int leader2meta(char *inFile, char *outFile, int listFlag);

// Prototypes from terrasar.c
int terrasar2csv(char *inFile, char *outFile, int listFlag);
int terrasar2kml(char *inFile, char *outFile, int listFlag);
int terrasar2shape(char *inFile, char *outFile, int listFlag);
int write_terrasar2meta(char *inFile, char *outFile, int listFlag);


// Prototypes from point.c
int point2polygon(char *inFile, char *outFile, int listFlag);
int point2kml(char *inFile, char *outFile, int listFlag);
void shape_point_init(char *inFile);
int point2shape(char *inFile, char *outFile, int listFlag);

// Prototypes from polygon.c
int polygon2point(char *inFile, char *outFile, int listFlag);
int polygon2kml(char *inFile, char *outFile, int listFlag);
void shape_polygon_init(char *inFile, int vertices);
int polygon2shape(char *inFile, char *outFile, int listFlag);

// Prototypes from generic_csv.c
FILE *csv_open(const char *filename,
               int *num_meta_cols, csv_meta_column_t **meta_column_info,
               int *num_data_cols, csv_data_column_t **data_column_info);
void csv_info(int num_meta_cols, csv_meta_column_t *meta_column_info,
              int num_data_cols, csv_data_column_t *data_column_info);
int csv_line_parse(const char *line, int line_num,
                   int num_meta_cols, csv_meta_column_t *meta_column_info,
                   int num_data_cols, csv_data_column_t *data_column_info,
                   char ***column_data_o, double **lats_o, double **lons_o);
void csv_free(int num_meta_cols, char **column_data,
              double *lats, double *lons);
void csv_dump(const char *filename);
int csv2kml(const char *in_file, const char *out_file, int listFlag);
void shape_csv_init(char *inFile, csv_meta_column_t *meta_column_info,
                    int num_meta_cols, int num_data_cols);
int csv2shape(char *inFile, char *outFile, int listFlag);

// Prototypes from auig.c
char *lf(double value);
int auig2kml(char *in_file, char *out_file, int listFlag);
void shape_auig_init(char *inFile, char *header);
int auig2shape(char *in_file, char *out_file, int listFlag);

// Prototypes from geotiff.c
int geotiff2csv(char *inFile, char *outFile, int listFlag);
int geotiff2kml(char *inFile, char *outFile, int listFlag);
void shape_geotiff_init(char *inFile);
int geotiff2shape(char *inFile, char *outFile, int listFlag);

// Prototypes from kml.c
void strip_end_whitesp_inplace(char *s);
int kml2meta(char *in_file, char *out_file, int listFlag);
int kml2point(char *in_file, char *out_file, int listFlag);
int kml2polygon(char *in_file, char *out_file, int listFlag);
int kml2csv(char *in_file, char *out_file, int listFlag);
int kml2auig(char *in_file, char *out_file, int listFlag);
int kml2shape(char *in_file, char *out_file, int listFlag);
int kml2ursa(char *in_file, char *out_file, int listFlag);
void test_kml(const char *inFile);

// Prototypes from shape.c
int shape2meta(char *inFile, char *outfile, int listFlag);
int shape2point(char *inFile, char *outfile, int listFlag);
int shape2polygon(char *inFile, char *outfile, int listFlag);
int shape2csv(char *inFile, char *outfile, int listFlag);
int shape2auig(char *inFile, char *outfile, int listFlag);
int shape2kml(char *inFile, char *outfile, int listFlag);

// Prototypes from rgps.c
void rgps2kml(cell_t cell, double *lat, double *lon, FILE *fp);
void rgps_grid2kml(grid_attr_t grid, FILE *fp);
void rgps2shape(cell_t cell, double *lat, double *lon, int vertices,
                DBFHandle dbase, SHPHandle shape, int n);
void rgps_grid2shape(grid_attr_t grid, DBFHandle dbase, SHPHandle shape,
		     int n);
void rgps_weather2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);

// Prototypes from multimatch.c
int multimatch2shape(char *inFile, char *outFile, int listFlag);

// Prototypes from ursa.c
int ursa2shape(char *inFile, char *outFile, int listFlag, int stack);
int ursa2kml(char *in_file, char *out_file, int listFlag, int stack);

// Prototypes from datapool.c
int datapool2shape(char *inFile, char *outFile, int listFlag, int stack);
int datapool2kml(char *in_file, char *out_file, int listFlag, int stack);

// Prototypes from high_altitude_photography.c
int hap2kml(char *in_file, char *out_file, int listFlag);
int hap2shape(char *inFile, char *outFile, int listFlag);

// Prototypes from smap.c
int smap2shape(char *inFile, char *outFile);

// Prototypes from custom.c
int custom2shape(char *inFile, const char *format,
                 char *outFile, int listFlag);
int custom2kml(char *inFile, const char *format,
	       char *outFile, int listFlag);

// Prototypes from convert2vector.t.c
int test_c2v(char *inFile, const char *inFormat_str,
	     char *outFile, const char *outFormat_str);

#endif
