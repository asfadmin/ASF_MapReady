#ifndef _GEOMPAK_H_
#define _GEOMPAK_H_

#ifndef NULL
#define NULL 0
#endif

#define MAX_GRID_SIZE	127	/* Maximum grid dimension (each dimension) */
#define MAX_TIE_POINTS	3072	/* Maximum number of tie point records */
#define MAX_POLY_DEGREE	  4	/* Maximum degree of modeling polynomial */
#define MAX_COEF	 16	/* Maximum number of coefficients (4th order) */

#define DX	0.03125		/* 1/32 of a pixel */
#define TWOPI	(PI * 2)             /* PI * 2 */

#define GEO_NOTVALID   -1
#define GEO_VALID	1
#define BAD_ID	       -2 	/* Bad tie point id */

#define NOCORR		100
#define PRECORR		200

#define DBL_RECSIZE	4096	/* Must be divisable by sizeof(double) */

#define MAX_GEOM_INBUF		2048000
#define MAX_GEOM_WORKBUF	2048000

/* Misc macros
  -----------*/
#define SQUARE(x)	x * x	/* x**2 */
#define	CUBE(x)     x * x * x	/* x**3 */
#define	QUAD(x) x * x * x * x	/* x**4 */

#define GMAX(A, B)	((A) > (B) ? (A) : (B))	/* assign maximum of a and b */
#define GMIN(A, B)	((A) < (B) ? (A) : (B))	/* assign minimum of a and b */

#define IMOD(A, B)	(A) - (((A) / (B)) * (B)) /* Integer mod function */

/* Error and return codes
  ----------------------*/
#define BAD_DEGREE	101	/* Polynomial degree not supported */
#define NOT_POLY	102	/* Model is other than a polynomial model */
#define ELIMINATE	103	/* Flag to eliminate grid element */
#define KEEP		104	/* Flag signaling need to keep grid element */
#define NO_SPACE	105	/* Not able to allocate dynamic memory */
#define DEGENERATE	106	/* Transformation not one-to-one */
#define NO_INGRID	107	/* Error opening grid file for input */
#define BAD_OUTGRID	108	/* Error opening grid file for output */
#define NO_REF_TPS	109	/* Reference tie pt selection file not found */
#define NO_SEA_TPS	110	/* Search tie point selection file not found */
#define BAD_OUT_MTP	111	/* Error opening MTP file for output */
#define BAD_OUT_TPL	112	/* Error opening TLP file for output */
#define NO_TPL		118	/* Tie point location file not found */
#define BAD_TPL		119	/* Error opening TPL file for output */
#define NO_FIT_X	120	/* Error modeling in X dimension */
#define NO_FIT_Y	121	/* Error modeling in Y dimension */
#define GEO_CLEAN	122	/* General E_FAIL needing file clean-up */

#define TPS	1	/* Flag for a Tie Point Selection file */
#define MTP	2	/* Flag for a Merged Tie Point file */
#define TPL	3	/* Flag for a Tie Point Location file */

#define TPSDAT	1	/* Flag for a Tie Point Selection data record */
#define MTPDAT	2	/* Flag for a Merged Tie Point data record */
#define TPLDAT	3	/* Flag for a Tie Point Location data record */
#define TPSHDR	4	/* Flag for a Tie Point Selection header record */
#define MTPHDR	5	/* Flag for a Merged Tie Point header record */
#define TPLHDR	6	/* Flag for a Tie Point Location header record */

/* The resampling weight table structure contains up to three N by 33
   matrices of separable interpolation weights to be used in brightness
   level resampling.  N is the resampling kernel dimension and 33
   identifies the 32 subdivisions between pixel values, including both
   endpoints.
  ----------------------------------------------------------------------*/
struct RWTAB
   {
   int dim[3];			/* Table dimensions */
   double table1[33][16];	/* First table */
   double table2[33][16];	/* Second table */
   double table3[33][16];	/* Third table */
   };

/* The geometric mapping grid structure contains a geometric mapping grid
   mapping output space coordinates (line,sample) to input space coordinates
   (line,sample), information needed to fill the output space image's DDR, 
   and miscellaneous mapping grid parameters and statistics.
 ------------------------------------------------------------------------*/
struct GEOGRID
  {
  int	proj_valid;	  /* Valid field for projection fields */
  int	fiterr_valid;	  /* Valid field for fit error fields */
  int	griderr_valid;	  /* Valid field for griding error fields */
  int	 ncoeff;	  /* Number of polynomial coefficients in X & Y.
			     This field is zero if other than a polynomial
			     model was used to generate the mapping grid. */
  int 	 code;	          /* Projection code for the output space image's
 			     DDR.  Values for this field are defined in the
			     "proj.h" include file. */
  int 	 zone;	   	  /* Projection zone code for UTM or State Plane
			     projections.  It has the same valid ranges and
			     defaults as the "zone_code" field in the DDR. */
  int 	 datum;	          /* Projection datum code.  It has the same valid
			     range and defaults as the "datum_code" field in
			     the DDR. */
  int	 nrows;	    	  /* Number of geometric mapping grid rows */
  int	 ncols;	    	  /* Number of geometric mapping grid columns */
  int	 lines;	    	  /* Number of lines in the output image */
  int	 samples;    	  /* Number of samples in the output image */
  int   *out_lines;      /* Pointer to the locations of the horizontal 
			     coordinate (line number) on each grid row, going
			     from top to bottom.  The output grid cells are of
			     consistent size and are rectangular, perpendicular
			     to the line, sample axis of the output image. 
			     Therefore, only intersections need be given.  */
  int   *out_samps; 	  /* Pointer to the locations of the vertical 
			     coordinate (pixel number) on each grid column,
			     going from left to right.  Again, only inter-
			     sections need be given.  */
  double corners[8];  	  /* Projection coordinates of the resulting output
			     image's four corners.  This field corresponds
			     to the DDR's "upleft", "loleft", "upright", and
			     "loright" fields.  */
  double pdist[2];    	  /* Projection distance per pixel in Y and in X.
			     This field corresponds to the "pdist_y" and
			     "pdist_x" field in the DDR.  */
  double projprms[15];    /* Array of 15 projection coefficients as required
			     by the projection transformation package.  Refer
			     to the projection package documentation for a
			     description of each field for a given projection.
			     This field corresponds to the "proj_coef" field
			     in the DDR.  */
  double *coeffs;	  /* Pointer to transformation coefficients.  The
			     first ncoeff values are the Y transformation
			     coefficients, followed by ncoeff X tranformation
			     coefficients.  */
  double max_grid_err[2]; /* Maximum horizontal and vertical grid interpolation
			     errors at an of 16 reference points.  Gridding 
			     routines give an estimate of the overall quality 
			     of the gridding process using a 4 x 4 matrix of 
			     reference points located at the intersections of
			     four vertical and four horizontal lines in the 
			     image region covered by the mapping grid.  The 
			     lines are located 3/126, 43/126, 83/126, and 
			     123/126 of the distance from one edge of the area 
			     to the other. */
  double ave_grid_err[2]; /* Average of the absolute values of the horizontal
			     and vertical grid interpolation errors of the
		    	     sixteen reference points. */
  double rms_grid_err;    /* RMS of the residual errors of the sixteen 
			     reference points.  */
  double act_grid_err[32];/* Actual fitting errors for each of the 16
			     reference points; each pair of values is given
			     in (line, sample) order, going left to right
			     in each row starting with the top row */
  double grid_tol;	  /* Tolerance used in reducing grid */
  double max_fit_err[2];  /* Maximum horizontal and vertical residual errors
			     of the tie points from the modeling process, if
			     tie points were used and model = "POLYNOMIAL". */
  double ave_fit_err[2];  /* Average of the absolute values of the horizontal
			     and vertical residual errors of the tie points
			     from the modeling process, if tie points were used
			     and model = "POLYNOMIAL."  */
  double rms_fit_err;     /* RMS of the residual errors from the modeling 
			     process, if tie points were used and model =
   			     "POLYNIOMIAL."  */
  double *in_lines;       /* Pointer to a buffer of input image line 
			     coordinates, one for each grid intersection.  
			     This buffer has as size of nrows * ncols * 
			     sizeof(double) bytes. */
  double *in_samps;       /* Pointer to a buffer of input image sample 
			     coordintaes, one for each grid intersection.  
			     This buffer has a size of nrows * ncols * 
			     sizeof(double) bytes.  */
  char	 model[15];       /* Type of model used to generate the geometric
			     mapping grid.  Standard values are "POLYNOMIAL",
			     "SPACECRAFT", and "FINITE ELEMENT".  Other
			     values are allowed, if need be. */
  char 	 units[12];       /* Projection units.  This value corresponds to
			     the proj_units field in the DDR. */
  };


/*  The Tie Point Location file records the results of automatic or manual
    correlation for subsequent use in generating geometric mapping grids for 
    the rectification process.  Each tie point location file consists of a 
    single header record followed by one record for each tie point.  The first 
    (header) record is stored in the TPLHDR structure.  The tie point records
    are stored in the TPL structure.
  ---------------------------------*/
struct TPLHEAD
  {
  double corners[8];  	  /* Projection coordinates of the resulting output
			     image's four corners.  This field corresponds
			     to the DDR's "upleft", "loleft", "upright", and
			     "loright" fields.  */
  double pdist[2];    	  /* Projection distance per pixel in Y and in X.
			     This field corresponds to the "pdist_y" and
			     "pdist_x" field in the DDR.  */
  double projprms[15];    /* Array of 15 projection coefficients as required
			     by the projection transformation package.  Refer
			     to the projection package documentation for a
			     description of each field for a given projection.
			     This field corresponds to the "proj_coef" field
			     in the DDR.  */
  double edge_thres;	  /* Faction of pixels to be classified as edges.
			     Refer to the edge correlation routine for a
			     description of this field */
  double min_str;    	  /* Minimum acceptable correlation strength */
  double max_dis;   	  /* Maximum acceptable difference between the 
			     nominal tie point location and that given 
			     by correlation */
  int	proj_valid;	  /* Valid field for projection fields */
  int  code;	  	  /* Projection code for the output space image's
 			     DDR.  Values for this field are defined in the
			     "proj.h" include file. */
  int 	zone;	  	  /* Projection zone code for UTM or State Plane
			     projections.  It has the same valid ranges and
			     defaults as the "zone_code" field in the DDR. */
  int	datum;	  	  /* Projection datum code.  It has the same valid
			     range and defaults as the "datum_code" field in
			     the DDR. */
  char   ref_mode[9];     /* Reference mode flag.  Valid values are:
				"IMAGE"	     Reference scene is a full image
				"MAP"	     Reference area is a map
				"CHIP-TEM"   Reference chips used to establish
					     temporal registration
				"CHIP-FUL"   Reference chips used to establish
					     geographic registration--extract
					     from full input image
				"CHIP-PRE"   Reference chips used to establish
					     geographic registration--search
					     sub-images prescaled or rotated */
  char 	 units[12];       /* Projection units.  This value corresponds to
			     the proj_units field in the DDR. */
  char   peak_method[25]; /* Correlation peak fitting method.  Valid values are:
				"RECIPROCAL" Elliptic paraboloid fit to
					     reciprocal of correlation values
				"PARABOLOID" Elliptic paraboloid fit
				"GAUSSIAN"   Elliptic gaussian fit
				"NEAR_INT"   No surface fit.  Tie point assigned
					     the integer coordinate which gave
					     the largest correlation value 
                                "NO_CORR"    Auto correlation not preformed */
  char   corr_type[7];    /* Type of correlation used.  Valid types are:
				"EDGE"	    Edge correlation
				"GREY"	    Grey level correlation
				"PHASE"	    Phase correlation
			   	"MANUAL"    Manually correlated--automatic
						correlation not performed */
  char   sea_name[CMLEN]; /* LAS name of the search image in which points
			     were obtained from */
  char   ref_name[CMLEN]; /* LAS name of the reference image in which points
			     were obtained from.  If points were obtained
			     from a source other than an image, this field
			     will be blank. */
  };

struct TPLDATA
  {
  double geo_coord[2];    /* Geographic coordinates of the tie point.  This
                             field may or may not contain data, depending 
	 		     upon the type of registration performed */
  double sea_coord[2];    /* Refined search image location of the tie point
                             in line, sample in the full search image */
  double ref_coord[2];    /* Image location of the tie point in line, sample
   			     in the reference image or subimage.  This defines
                             the center of the reference chip extracted during 
			     the correlation process */
  double rms_error[2];	  /* Estimated horizontal and vertical RMS errors.
 			     This field is not valid if correlation was not
			     attempted or was not successful */
  double strength;	  /* Strength of correlation; set to zero if automatic
			     correlation not attempted.  If points were manually
			     selected, the zoom_factor may be used as a rough
			     indication of "correlation" strength--however
			     this is not a very analogus indicator and the
			     units of measurement are different! */
  double displacement;	  /* Displacement, measured diagonally, from nominal
			     input image location of tie point to location
			     obtained by correlation; zero if automatic
			     correlation was not attempted */
  double nominal[2];	  /* Nominal search image line and sample coordinates
			     of the tie point; not valid if automatic
			     correlation was not performed */
  double elevation;	  /* Elevation of the tie point in meters */
  double zoom_factor;	  /* The enlargement factor used when selecting the tie
			     point.  This gives an indication as to the possible
			     sub-pixel accuracy of the tie point */
  int ref_size[2];       /* Actual width and height of the reference subimage.
                             This reflects adjustments made by the correlation
			     process */
  int offsets[2];        /* Requested horizontal and vertical search offsets */
  int chip_loc[2];       /* The line, sample coordinate relative to the full
   			     image of the upper left corner of the input 
			     subimage */
  int sea_size[2];       /* Actual width and height of the input search
			     subimage.  This reflects adjustments made by the
			     correlation process */
  int chip_coord[2];     /* The line, sample coordinates of the tie point
		  	     relative to the reference chip image.  This field 
			     is not valid if chips were not used */
  int req_size[2];	  /* Requested width and height of the reference
			     subimage; not valid if automatic correleation
			     was not performed */
  int location[2];	  /* Line and sample coordinates of the upper left
			     corner of the reference subimage relative to
			     the reference image or reference chip */
  int active;		  /* Tie point active flag containing accept/reject
			     codes.  Valid values are:
				0 --> Accept manually extracted tie point
				1 --> Accept automatically registerd tie pt
				2 --> Reject; correlation peak too near edge
					of search area.
				3 --> Reject; subsidiary peak comparable in
					strength to main peak
				4 --> Reject; strength of peak below minimum
					specified by user
				5 --> Reject; diagonal displacement from
					nominal location exceeds maximum
					specified by user
				6 --> Correlation not attempted; error in
					correlation parameters
				7 --> Reject; manually by user
				8 --> Reject; automatically by the modeling
					process
				9 --> Reject; manually by user during the
					modeling process
			       1X --> Previously rejected tie point re-accepted
					manually by user.  X indicates original
					active code of tie point (0 - 9) before
					being re-accepted */

  char pt_id[20];	  /* A user entered field describing each point.  It
                             must be unique */
  char ref_chip[CMLEN];   /* The LAS image name of the file containing the
                             subimage reference chip.  This field is blank
                             if a subimage chip file does not exist for this
                             tie point */
  };


/*  The Merged Tie Point file records the approximage coordinates of tie
    points and specifies options to be used during automatic, precision
    correlation of each tie point.  Each merged tie point file consists
    of a single header record followed by one record for each tie point.
    The first (header) record is stored the MTPHDR structure.  The tie 
    point records are stored in the MTP structure.
  -----------------------------------------------*/
struct MTPHEAD
  {
  double corners[8];  	  /* Projection coordinates of the resulting output
			     image's four corners.  This field corresponds
			     to the DDR's "upleft", "loleft", "upright", and
			     "loright" fields.  */
  double pdist[2];    	  /* Projection distance per pixel in Y and in X.
			     This field corresponds to the "pdist_y" and
			     "pdist_x" field in the DDR.  */
  double projprms[15];    /* Array of 15 projection coefficients as required
			     by the projection transformation package.  Refer
			     to the projection package documentation for a
			     description of each field for a given projection.
			     This field corresponds to the "proj_coef" field
			     in the DDR.  */
  int proj_valid;	  /* Valid field for projection fields */
  int code;		  /* Projection code for the output space image's
 			     DDR.  Values for this field are defined in the
			     "proj.h" include file. */
  int zone;		  /* Projection zone code for UTM or State Plane
			     projections.  It has the same valid ranges and
			     defaults as the "zone_code" field in the DDR. */
  int datum;		  /* Projection datum code.  It has the same valid
			     range and defaults as the "datum_code" field in
			     the DDR. */
  char   chip_mode[9];    /* Chip mode flag.  Valid values are:
				"NO-CHIPS"   Reference chip images not used
				"CHIP-TEM"   Reference chips used to establish
					     temporal registration
				"CHIP-FUL"   Reference chips used to establish
					     geographic registration--extract
					     from full input image
				"CHIP-PRE"   Reference chips used to establish
					     geographic registration--search
					     sub-images prescaled or rotated */
  char 	 units[12];       /* Projection units.  This value corresponds to
			     the proj_units field in the DDR. */
  char   sea_name[CMLEN]; /* LAS name of the search image in which points
			     were obtained from */
  char   ref_name[CMLEN]; /* LAS name of the reference image in which points
			     were obtained from.  If points were obtained
			     from a source other than an image, this field
			     will be blank. */
  };

struct MTPDATA
  {
  double geo_coord[2];    /* Geographic coordinates of the tie point.  This
                             field may or may not contain data, depending 
	 		     upon the type of registration performed */
  double nominal[2];      /* Nominal search image location of the tie point
                             in line, sample in the full search image */
  double ref_coord[2];    /* Image location of the tie point in line, sample
   			     in the reference image or subimage.  This defines
                             the center of the reference chip to be extracted 
			     during the correlation process */
  double elevation;	  /* Elevation of the tie point in meters */
  double zoom_factor;	  /* The enlargement factor used when selecting the tie
			     point.  This gives an indication as to the possible
			     sub-pixel accuracy of the tie point */
  int chip_size[2];      /* Desired sub-image width and height */ 
  int offsets[2];        /* Desired horizontal and vertical search offsets */
  char pt_id[20];	  /* A user entered field describing each point.  It
                             must be unique */
  char ref_chip[CMLEN];   /* The LAS image name of the file containing the
                             subimage reference chip.  This field is blank
                             if a subimage chip file does not exist for this
                             tie point */
  char sea_chip[CMLEN];   /* The LAS image name of the file containing the
 			     prescaled or rotated subimage search chip.  This
			     field is blank if a subimage chip file does not
 			     exist for this tie point */
  };


/*  The Tie Point Selection contains coordinates relative to a single image.
    Two tie point selection files, one for the search image and one for the
    reference image or geographic data, must be merged to specify temporal
    or geographic registration.

    Although merging the files requires an extra processing step, the inital
    generation of two separate data sets permits measurement of map coordinates
    in a separate step from tie point selection, and also facilitates reusing
    a set of reference image tie points for temporal registration or a series
    of images.

    The Tie Point Selection file consists of a single header record followed 
    by one record for each tie point.  The first (header) record is stored in 
    the TPSHDR structure.  The tie point records are stored in the TPS 
    structure.
  -----------*/
struct TPSHEAD
  {
  double corners[8];  	  /* Projection coordinates of the resulting output
			     image's four corners.  This field corresponds
			     to the DDR's "upleft", "loleft", "upright", and
			     "loright" fields.  */
  double pdist[2];    	  /* Projection distance per pixel in Y and in X.
			     This field corresponds to the "pdist_y" and
			     "pdist_x" field in the DDR.  */
  double projprms[15];    /* Array of 15 projection coefficients as required
			     by the projection transformation package.  Refer
			     to the projection package documentation for a
			     description of each field for a given projection.
			     This field corresponds to the "proj_coef" field
			     in the DDR.  */
  int proj_valid;	  /* Valid field for projection fields */
  int code;		  /* Projection code for the output space image's
 			     DDR.  Values for this field are defined in the
			     "proj.h" include file. */
  int zone;		  /* Projection zone code for UTM or State Plane
			     projections.  It has the same valid ranges and
			     defaults as the "zone_code" field in the DDR. */
  int datum;		  /* Projection datum code.  It has the same valid
			     range and defaults as the "datum_code" field in
			     the DDR. */
  char  mode[4];	  /* Tie Point mode.  Valid values are:
				"GEO"  Geographic Coordinates
				"REF"  Reference Image Coordinates
				"SEA"  Search Image Coordinates
				"PRO"  Projection Coordinates
				"USR"  User Defined Coordinates */

  char 	 units[12];       /* Projection units.  This value corresponds to
			     the proj_units field in the DDR. */
  char   name[CMLEN];     /* LAS name of the image in which points were 
                             obtained from.  If points were obtained from a
     		  	     source other than an image, this field will be 
			     blank */
  };

struct TPSDATA
  {
  double coord[2];        /* Coordinates of the tie point.  These are either
			     geographic coordinates, projection coordinates, or
			     user defined coordinates.  This field is valid when
			     MODE is set to "GEO", "PRO", or "USR" */
  double image_coord[2];  /* Image location of the tie point in line, sample
			     in the full image.  This field is used when mode
			     is set to "REF" or "SEA", but it may also contain
			     data with other types of registrations */
  double elevation;	  /* Elevation of the tie point in meters */
  double zoom_factor;	  /* The enlargement factor used when selecting the tie
			     point.  This gives an indication as to the possible
			     sub-pixel accuracy of the tie point */
  int chip_size[2];      /* Desired sub-imgae width and height.  This field is
			     valid only with MODE = "REF". */
  int offsets[2];        /* Desired horizontal and vertical search offsets.
			     This field is valid only with MODE = "SEA"  */
  char pt_id[20];	  /* A user entered field describing each point.  It
                             must be unique among points in this file.  When
			     tie point selection files are merged, this field
			     is used as the key for the merge.  */
  char chip_name[CMLEN];  /* The LAS image name of the file containing the
                             subimage chip.  This field is blank if a subimage 
			     chip file does not exist for this tie point */
  };

#ifndef FILE
	#include "asf.h"
#endif

#ifndef lasErr
	#define lasErr int
#endif

#ifndef FUNCTION
	#define FUNCTION
#endif

 void c_grderr (struct GEOGRID *grid, double *tol); 
 void grid_eval (struct GEOGRID *grid, double outx, double outy, double *grid_x, double *grid_y); 
double FUNCTION c_eval(
	int *degree,		/* Degree of polynomial */
	double *a,		/* Array of polynomial coefficients */
	double *x,double *y);

 int c_getrwt (char *host_name, struct RWTAB *table); 
 int c_getgrd (char *host_name, struct GEOGRID *grid); 
 int c_grdred (struct GEOGRID *grid, double *tolerance); 
 int elim_col (struct GEOGRID *grid, double tol, int *next, int *inc); 
 int elim_row (struct GEOGRID *grid, double tol, int *next, int *inc); 
 int check_col (int start, int end, double tol, int size, struct GEOGRID *grid); 
 int check_row (int start, int end, double tol, int size, struct GEOGRID *grid); 
 int c_putgrd (char *host_name, struct GEOGRID *grid); 
 
lasErr FUNCTION read_tps(FILE *fp, struct TPSDATA *pt_data);
lasErr FUNCTION read_mtp(FILE *fp, struct MTPDATA *pt_data);
lasErr FUNCTION read_tpl(FILE *fp, struct TPLDATA *pt_data);
lasErr FUNCTION write_tps(FILE *fp, struct TPSDATA *pt_data);
lasErr FUNCTION write_mtp(FILE *fp, struct MTPDATA *pt_data);
lasErr FUNCTION write_tpl(FILE *fp, struct TPLDATA *pt_data);

lasErr FUNCTION get_tps_header(FILE *fp, struct TPSHEAD *header);
lasErr FUNCTION get_mtp_header(FILE *fp, struct MTPHEAD *header);
lasErr FUNCTION get_tpl_header(FILE *fp, struct TPLHEAD *header);
lasErr FUNCTION put_tps_header(FILE *fp, struct TPSHEAD *header);
lasErr FUNCTION put_mtp_header(FILE *fp, struct MTPHEAD *header);
lasErr FUNCTION put_tpl_header(FILE *fp, struct TPLHEAD *header);


#endif
