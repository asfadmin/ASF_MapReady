#ifndef DATUM_H
#define DATUM_H

struct DATUMDEF
{
	long datumnum;		/* Datum number */
	long spherenum;		/* Spheroid number */
	char datumname[50];	/* Datum name */
	char spherename[50];	/* Spheroid name */
	char area[50];		/* Area or Countries where Datum is used,
                                   if multiple, they will be comma-delimited */
	char category[50];	/* Category Datum is associated with,
                                   if multiple,they will be plus-delimited "+"*/
	long xshift;		/* X shift to convert to WGS 84 */
	long yshift;		/* Y shift to convert to WGS 84 */
	long zshift;		/* Z shift to convert to WGS 84 */
	long xdelta;		/* X delta value for accuracy of conversion */
	long ydelta;		/* Y delta value for accuracy of conversion */
	long zdelta;		/* Z delta value for accuracy of conversion */
	double smajor;		/* Semi-major axis of the spheroid */
	double sminor;		/* Semi-minor axis of the spheroid. Derived
				   by recip_flat.  major*(1-flat) */
	double recip_flat;	/* Reciprical of flattening of the spheroid */
};

/* These are the values used by c_trans */
#define NO_TRANS 0		/* No datum transformation needed */
#define MOLODENSKY 1		/* No NAD27 datum, so do Molodensky trans. */
#define NAD27_TO_83 2		/* NAD 27 to NAD 83 transformation */
#define NAD83_TO_27 3		/* NAD 83 to NAD 27 transformation */
#define NAD27_TO_GEN 4		/* NAD27 to non NAD83 transformation */
#define GEN_TO_27 5		/* Non NAD83 to NAD27 transformation */

#endif
