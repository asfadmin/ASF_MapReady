/*============================================================================
 |  @(#)chkangl.c	1.3 96/07/29 00:06:02
 |
 |  Calculate the look angles for 3 points in range.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "scene_file.h"
#include "odl.h"
#include "Object.h"
#include "version.h"

static char sccsid_chkangl_c[] =
        "@(#)chkangl.c	1.3 96/07/29 00:06:02";

static	char usage[] = "scene_file | {scan_results_file [-f frame_id ]}";

#define	PI			((double) 3.141592653589793)
#define	DEG_PER_RAD		((double) 180.0 / PI)
#define	RAD_PER_DEG		(PI / (double) 180.0)
#define	EMF_F			((double) 298.257)
#define	EMF_R_E			((double) 6378.144)
#define	dist(a,b)		sqrt(((a)->x - (b)->x)*((a)->x - (b)->x) + \
			     	     ((a)->y - (b)->y)*((a)->y - (b)->y) + \
			     	     ((a)->z - (b)->z)*((a)->z - (b)->z))
typedef struct xyz {    	/* For position and velocity */
    double x;
    double y;
    double z;
} XYZ;

typedef struct {
    SCENE_FILE	sf;
    int		frame_id;
    double	angl_near;	/* In degrees */
    double	angl_mid;   	/* In degrees */
    double	angl_far;   	/* In degrees */
    double	r;
    double	altitude;
    double	gclat_sc;	/* In degrees */
    double	gdlat_sc;	/* "	"     */
    XYZ		a,b,c,d,e;
} Scene_t;

static Scene_t
    *load_scene_file(char* filename, int *scene_cnt, char *err),
    *load_scan_file(char* filename, int *scene_cnt, char *err);

/*-----------------------------*
 |  Calculate the look angle.
 *-----------------------------*/
static
double calc_lookangle(
    double Rslant, double re_nadir, XYZ *mid,
    double *r, double *altitude, double *gclat_sc, double *gdlat_sc)
{
    double xx,yy,zz,ff, dlat, delta, lk_angle;
    ff = 1.0 - 1.0/EMF_F;
    xx = mid->x * mid->x;
    yy = mid->y * mid->y;
    zz = mid->z * mid->z;
    *r  = sqrt(xx + yy + zz);

    *gclat_sc = acos(sqrt(xx + yy) / (*r));
    *gdlat_sc = atan(tan(*gclat_sc) / (ff*ff));
    dlat      = (*gdlat_sc) - (*gclat_sc);
    *altitude = ((*r) - re_nadir) * cos(dlat);

    *gclat_sc *= DEG_PER_RAD;
    *gdlat_sc *= DEG_PER_RAD;

    /* Calculate the antenna look angle */
    delta = (Rslant*Rslant - (*altitude)*(*altitude) ) / (2.0 * (*r));
    lk_angle = DEG_PER_RAD * (acos(((*altitude)+delta) / Rslant));

    return (lk_angle);
}

/*---------------------------------------------------------------*
 |  Calculates global x,y,z cartesian coordinates given lat/lon.
 *---------------------------------------------------------------*/
static
void calc_xyz(double lat, double lon, XYZ *pos)
{
    double glat, re, ff, f;
    ff = 1.0 - (f = 1.0 / EMF_F);
    glat = atan(tan(lat * RAD_PER_DEG) * ff * ff);
    re = (EMF_R_E*1000.0 * ff) / sqrt(1.0 - (2.0-f)*f*cos(glat)*cos(glat));
    pos->x = re * cos(glat) * cos(lon * RAD_PER_DEG);
    pos->y = re * cos(glat) * sin(lon * RAD_PER_DEG);
    pos->z = re * sin(glat);
}

/*----------------------------------------------------*
 |  Calculate the look angles for 3 points in range.
 *----------------------------------------------------*/
static
void calc_scene(Scene_t* sp)
{
    XYZ mid;
#ifdef	DEBUG
    if (sp->frame_id == -1)
	printf("SCENE\n");
    else
	printf("FRAME %d\n", sp->frame_id);

    printf("  Earth radius at nadir = %f\n", sp->sf.re_nadir);
    printf("  Mid %f, %f, %f\n", sp->sf.x_mid, sp->sf.y_mid, sp->sf.z_mid);
    printf("  a %f, %f\n", sp->sf.lat_a, sp->sf.lon_a);
    printf("  b %f, %f\n", sp->sf.lat_b, sp->sf.lon_b);
    printf("  c %f, %f\n", sp->sf.lat_scene_ctr, sp->sf.lon_scene_ctr);
    printf("  d %f, %f\n", sp->sf.lat_d, sp->sf.lon_d);
    printf("  e %f, %f\n", sp->sf.lat_e, sp->sf.lon_e);
    printf("  Near Slant range= %f\n", sp->sf.r_near);
    printf("  Mid Slant range= %f\n", sp->sf.r_mid);
    printf("  Near Slant range= %f\n", sp->sf.r_far);
    printf("\n");
#endif
    mid.x = sp->sf.x_mid;
    mid.y = sp->sf.y_mid;
    mid.z = sp->sf.z_mid;

    sp->angl_near = calc_lookangle(
	sp->sf.r_near,(double) sp->sf.re_nadir, &mid,
	&sp->r, &sp->altitude, &sp->gclat_sc, &sp->gdlat_sc
    );
    sp->angl_mid  = calc_lookangle(
	sp->sf.r_mid, (double) sp->sf.re_nadir, &mid,
	&sp->r, &sp->altitude, &sp->gclat_sc, &sp->gdlat_sc
    );
    sp->angl_far  = calc_lookangle(
	sp->sf.r_far, (double) sp->sf.re_nadir, &mid,
	&sp->r, &sp->altitude, &sp->gclat_sc, &sp->gdlat_sc
    );
    calc_xyz(sp->sf.lat_a, sp->sf.lon_a, &sp->a);
    calc_xyz(sp->sf.lat_b, sp->sf.lon_b, &sp->b);
    calc_xyz(sp->sf.lat_d, sp->sf.lon_d, &sp->d);
    calc_xyz(sp->sf.lat_e, sp->sf.lon_e, &sp->e);
    calc_xyz(sp->sf.lat_scene_ctr, sp->sf.lon_scene_ctr, &sp->c);
}

/*----------------------------------------------------*
 |  Print out the look angles for 3 points in range.
 *----------------------------------------------------*/
static
void print_scene(Scene_t* sp)
{
    if (sp->frame_id == -1)
	printf("Scene File:\n");
    else
	printf("Frame #%d:\n", sp->frame_id);

    printf("  Earth radius at nadir = %f\n", sp->sf.re_nadir);
    printf("  gclat_sc = %f deg., gdlat_sc = %f deg.\n",
	    sp->gclat_sc, sp->gdlat_sc);
    printf("  Spacecraft Radius = %f, Altitude = %f\n", 
	    sp->r, sp->altitude);
    printf("  Near Look Angle = %f, Slant Range= %f\n", 
	    sp->angl_near, sp->sf.r_near);
    printf("  Mid Look Angle  = %f, Slant Range= %f\n", 
	    sp->angl_mid, sp->sf.r_mid);
    printf("  Near Look Angle = %f, Slant Range= %f\n", 
	    sp->angl_far, sp->sf.r_far);
    printf("  Distance a to b = %g\n", dist(&sp->a, &sp->b)/1000.0);
    printf("  Distance d to e = %g\n", dist(&sp->d, &sp->e)/1000.0);
    printf("  Distance a to d = %g\n", dist(&sp->a, &sp->d)/1000.0);
    printf("  Distance b to e = %g\n", dist(&sp->b, &sp->e)/1000.0);
    printf("  Distance a to c = %g\n", dist(&sp->a, &sp->c)/1000.0);
    printf("  Distance b to c = %g\n", dist(&sp->b, &sp->c)/1000.0);
    printf("  Distance d to c = %g\n", dist(&sp->d, &sp->c)/1000.0);
    printf("  Distance e to c = %g\n", dist(&sp->e, &sp->c)/1000.0);
    printf("  Distance a to e = %g\n", dist(&sp->a, &sp->e)/1000.0);
    printf("  Distance b to d = %g\n", dist(&sp->b, &sp->d)/1000.0);
    printf("\n");
}

main(int argc, char* argv[])
{
    register Scene_t* sp = NULL;
    int i, ns, frame_id = -1;
    char *id, error[256], *input_file = NULL;
    id = (id = strrchr(argv[0],'/')) ? id+1 : argv[0];

    for (i = 1; argv[i] != NULL; ++i) {
	if (strcmp(argv[i], "-f") == 0) {
	    if (argv[++i] == NULL) {
	 	printf("%s: -f option needs a Frame ID value\n", id);
		printf("usage: %s %s\n", id, usage);
		exit(-1);
	    }
	    if (sscanf(argv[i], "%d", &frame_id) != 1 || frame_id < 0) {
	 	printf("%s: %s not a valid Frame ID value\n", id);
		exit(-1);
	    }
	}
	else if (argv[i][0] == '-') {
	    printf("%s: %s not a valid option\n", id, argv[i]);
	    printf("usage: %s %s\n", id, usage);
	    exit(-1);
	}
	else if (! input_file)
	    input_file = argv[i];
	else {
	    printf("%s: Too many input files\n", id);
	    printf("usage: %s %s\n", id, usage);
	    exit(-1);
	}
    }
    if (input_file == NULL) {
	printf("%s: input file required\n", id);
	printf("usage: %s %s\n", id, usage);
	exit(-1);
    }
    if (frame_id >= 0) {
	register int i;
	if (!(sp = load_scan_file(input_file, &ns, error))) {
	    printf("%s: %s\n", id, error);
	    exit(-1);
	}
	for (i = 0; i < ns; ++i)
	    if (sp[i].frame_id == frame_id)
		break;
	if (i < ns) {
	    calc_scene(&sp[i]);
	    print_scene(&sp[i]);
	}
	else {
	    printf("%s: can't find frame %d in scan results file\n", frame_id);
	    exit(-1);
	}
    }
    else if ((sp = load_scan_file(input_file, &ns, error)) ||
	     (sp = load_scene_file(input_file, &ns, error))) {
	register int i;
	for (i = 0; i < ns; ++i) {
	    calc_scene(&sp[i]);
	    print_scene(&sp[i]);
	}
    }
    if (sp) free(sp);
}

/*----------------------------------------------------------*
 |  Load in only needed parameters from scan results file.
 *----------------------------------------------------------*/
static
Scene_t *load_scan_file(char* filename, int *scene_cnt, char *err)
{
    register Scene_t* sp;
    register int i, j = 0;
    register ODL scan_results, *body_contents;
    *scene_cnt = 0;

    if (! (scan_results = ODLparse(filename, 0, err)))
	return NULL;

    body_contents = Val(Lookup(scan_results, "SCAN_RESULTS_FILE.BODY"));
    if (body_contents == NULL) {
	ODLFree(scan_results);
	strcpy(err, "not a valid scan results file");
	return NULL;
    }
    for (i = 0; body_contents[i] != NULL; ++i) {
	int ierr;
	ODL segmt = body_contents[i];
	if (strcasecmp(Name(segmt), "SEGMENT") ||
	    Type(segmt) != (Method_t) Object)
	    continue;
	*scene_cnt += ODLGetInt(segmt, "FRAME_COUNT", &ierr);
    }
    if (*scene_cnt == 0) {
	ODLFree(scan_results);
	sprintf(err, "no frames found");
	return NULL;
    }
    if ((sp = (Scene_t*) malloc(*scene_cnt * sizeof(Scene_t))) == NULL) {
	ODLFree(scan_results);
	sprintf(err, "no memory for %d Scene_t", *scene_cnt);
	*scene_cnt = 0;
	return NULL;
    }
    for (i = 0; body_contents[i] != NULL; ++i) {
	register int k;
	ODL *segmt_contents, segmt = body_contents[i];

	if (strcasecmp(Name(segmt), "SEGMENT") ||
	    Type(segmt) != (Method_t) Object)
	    continue;

	segmt_contents = Val(segmt);
	for (k = 0; segmt_contents[k] != NULL; ++k) {
	    double dd,ff,xx,yy,zz;
	    static char
	    X_MID[] = "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
	    Y_MID[] = "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
	    Z_MID[] = "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
	    A_LAT[] = "NEAR_START_LAT",
	    A_LON[] = "NEAR_START_LON",
	    B_LAT[] = "FAR_START_LAT",
	    B_LON[] = "FAR_START_LON",
	    C_LAT[] = "CENTER_LAT",
	    C_LON[] = "CENTER_LON",
	    D_LAT[] = "NEAR_END_LAT",
	    D_LON[] = "NEAR_END_LON",
	    E_LAT[] = "FAR_END_LAT",
	    E_LON[] = "FAR_END_LON",
	    R_NEAR[]= "SL_RNG_1ST_PIX",
	    R_MID[] = "SL_RNG_MID_PIX",
	    R_FAR[] = "SL_RNG_LAST_PIX";
	    int ierr;
	    ODL frame = segmt_contents[k];
	    if (strcasecmp(Name(frame), "FRAME") ||
		Type(frame) != (Method_t) Object)
		continue;
	    
	    sp[j].frame_id = ODLGetInt(frame, "FRAME_ID", &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.x_mid = ODLGetDouble(frame, X_MID, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.y_mid = ODLGetDouble(frame, Y_MID, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.z_mid = ODLGetDouble(frame, Z_MID, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lat_a = ODLGetDouble(frame, A_LAT, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lon_a = ODLGetDouble(frame, A_LON, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lat_b = ODLGetDouble(frame, B_LAT, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lon_b = ODLGetDouble(frame, B_LON, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lat_d = ODLGetDouble(frame, D_LAT, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lon_d = ODLGetDouble(frame, D_LON, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lat_e = ODLGetDouble(frame, E_LAT, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lon_e = ODLGetDouble(frame, E_LON, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lat_scene_ctr = ODLGetDouble(frame, C_LAT, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.lon_scene_ctr = ODLGetDouble(frame, C_LON, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.r_near = ODLGetDouble(frame, R_NEAR, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.r_mid = ODLGetDouble(frame, R_MID, &ierr);
	    if (ierr == -1) continue;

	    sp[j].sf.r_far = ODLGetDouble(frame, R_FAR, &ierr);
	    if (ierr == -1) continue;

	    ff = 1.0 / EMF_F;
	    xx = sp[j].sf.x_mid * sp[j].sf.x_mid;
	    yy = sp[j].sf.y_mid * sp[j].sf.y_mid;
	    zz = sp[j].sf.z_mid * sp[j].sf.z_mid;
	    dd = (xx + yy) / (xx + yy + zz);
	    sp[j].sf.re_nadir = EMF_R_E * (1-ff) / sqrt(1-(ff*(2-ff))*dd);

	    if (++j == *scene_cnt) {
		ODLFree(scan_results);
		return (sp);
	    }
	} /* Next ODL in segment */
    } /* Next ODL in BODY */

    ODLFree(scan_results);
    if ((*scene_cnt = j) == 0) {
	free(sp);
	return NULL;
    }
    return (sp);
}

/*-------------------------------------------*
 |  Load in all parameters from scene file.
 *-------------------------------------------*/
static
int load_XYZ(FILE* fp, double *bp)
{
    return (fscanf(fp, "%lf %lf %lf", bp, bp+1, bp+2) == 3);
}
static
int load_rate(FILE* fp, float *bp)
{
    return (fscanf(fp, "%f %f %f", bp, bp+1, bp+2) == 3);
}
static
int load_latlon(FILE* fp, double *bp)
{
    return (fscanf(fp, "%lf %lf", bp, bp+1) == 2);
}
static
int load_string(FILE* fp, char *bp)
{
    return (fscanf(fp, "%s", bp) == 1);
}
static
int load_double(FILE* fp, double *bp)
{
    return (fscanf(fp, "%lf", bp) == 1);
}
static
int load_float(FILE* fp, float *bp)
{
    return (fscanf(fp, "%f", bp) == 1);
}
static
int load_int(FILE* fp, int* bp)
{
    return (fscanf(fp, "%d", bp) == 1);
}
typedef struct {
    char* 	name;
    int		(*load)();
    size_t	offset;

} SceneToken_t;

static
int name_cmp(const void* t1, const void* t2)
{
    return strcmp(((SceneToken_t*)t1)->name, ((SceneToken_t*)t2)->name);
}

static
Scene_t *load_scene_file(char* filename, int *scene_cnt, char *err)
{
    static SceneToken_t table[] =
    {
#define SceneOffset(x)	((char*) &((SCENE_FILE*)0)->x - (char*)0)
	{"ALTITUDE", load_float, SceneOffset(altitude)},
	{"ASP_VERSION", load_string, SceneOffset(asp_ver[0])},
	{"ATT_RATE", load_rate, SceneOffset(pitch_rate)},
	{"BEGIN_POS", load_XYZ, SceneOffset(x_begin)},
	{"BEGIN_VEL", load_XYZ, SceneOffset(vx_begin)},
	{"COMMENT", load_string, SceneOffset(comment[0])},
	{"DCRS_END", load_int, SceneOffset(dcrs_end)},
	{"DCRS_START", load_int, SceneOffset(dcrs_start)},
	{"END_POS", load_XYZ, SceneOffset(x_end)},
	{"END_VEL", load_XYZ, SceneOffset(vx_end)},
	{"FDOT_G_A", load_double, SceneOffset(fdotga)},
	{"FDOT_G_B", load_double, SceneOffset(fdotgb)},
	{"FDOT_G_C", load_double, SceneOffset(fdotgc)},
	{"FD_G_A", load_double, SceneOffset(fdga)},
	{"FD_G_B", load_double, SceneOffset(fdgb)},
	{"FD_G_C", load_double, SceneOffset(fdgc)},
	{"FILE_NAME", load_string, SceneOffset(file_name[0])},
	{"IMAGE_ID", load_string, SceneOffset(image_id[0])},
	{"IQ_GAIN_IMB", load_float, SceneOffset(iq_gain_imb)},
	{"LNUM_SCENE_CTR", load_int, SceneOffset(lnum_scene_ctr)},
	{"LOC_A", load_latlon, SceneOffset(lat_a)},
	{"LOC_B", load_latlon, SceneOffset(lat_b)},
	{"LOC_D", load_latlon, SceneOffset(lat_d)},
	{"LOC_E", load_latlon, SceneOffset(lat_e)},
	{"LOC_SCENE_CTR", load_latlon, SceneOffset(lat_scene_ctr)},
	{"MEDIA_LABL", load_string, SceneOffset(media_labl[0])},
	{"MID_POS", load_XYZ, SceneOffset(x_mid)},
	{"MID_VEL", load_XYZ, SceneOffset(vx_mid)},
	{"NLINES", load_int, SceneOffset(nl)},
	{"NPIXELS", load_int, SceneOffset(np)},
	{"PNUM_SCENE_CTR", load_int, SceneOffset(pnum_scene_ctr)},
	{"PROC_TYPE", load_string, SceneOffset(proc_type[0])},
	{"RE_IMAGE_CTR", load_float, SceneOffset(re_image_ctr)},
	{"RE_NADIR", load_float, SceneOffset(re_nadir)},
	{"R_FAR", load_double, SceneOffset(r_far)},
	{"R_MID", load_double, SceneOffset(r_mid)},
	{"R_NEAR", load_double, SceneOffset(r_near)},
	{"SPEC_FLAG", load_string, SceneOffset(spec_flg[0])},
	{"SP_ID", load_string, SceneOffset(sp_id[0])},
	{"SW_ID", load_string, SceneOffset(sw_id[0])},
	{"TME_SCENE_CTR", load_string, SceneOffset(tme_scene_ctr[0])},
	{"WNDW_POS", load_float, SceneOffset(wndw_pos)}
#undef SceneOffset
    };
    FILE* fp;
    Scene_t* sp;
    SceneToken_t token;
    char name[256];
    token.name = name;
    *scene_cnt = 0;

    if ((sp = (Scene_t*) malloc(sizeof(Scene_t))) == NULL) {
	strcpy(err, "no memory for Scene_t");
	return NULL;
    }
    if (!(fp = fopen(filename, "r"))) {
	if (err) sprintf(err, "can't open %s: %s", filename, strerror(errno));
	free(sp);
	return NULL;
    }
    while (fscanf(fp, "%s", token.name) == 1) {
	int c;
	SceneToken_t* p;
	size_t n = strlen(token.name);
	if (token.name[n-1] == '=')
	    token.name[n-1] = 0;
	else
            while ((c = fgetc(fp)) != EOF && c != '=');

	p = bsearch(&token, table, sizeof(table)/sizeof(table[0]), 
		    sizeof(table[0]), name_cmp);
	if (p == NULL) {
	    sprintf(err, "%s: invalid keyword", token.name);
	    fclose(fp);
	    free(sp);
	    return NULL;
	}
	if (! (*p->load)(fp, (char*)&sp->sf + p->offset)) {
	    sprintf(err, "can't get %s", p->name);
	    fclose(fp);
	    free(sp);
	    return NULL;
	}
    }
    *scene_cnt = 1;
    sp->frame_id = -1;
    fclose(fp);
    return(sp);
}
