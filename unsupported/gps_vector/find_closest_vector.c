#include "asf.h"

#include "gps_vector.h"

void find_closest_vector(input, head, tail, file)
LATLON *input;
LATLON *head;
LATLON *tail;
FILE *file;
{
    struct gps_rec *rec;
    double inputx, inputy;
    double sourcex, sourcey;
    double diffx, diffy;
    double temp, min=HUGE_VAL;
    LATLON source, target;
    char buf[1024];

    /* skip the ceos header of the data file */
    fseek(file, 720L, 0);
    /* convert input lat/lon into SSMI x,y */
    ll_to_ssmi(input->lat,input->lon,&inputx,&inputy);

    /* read in data */
    while (fread(buf,sizeof(struct gps_rec),1,file) != 0) {
        rec = (struct gps_rec *) buf;
        source.lat = s_to_dbl(rec->a_lat,10);
        source.lon = s_to_dbl(rec->a_lon,10);
        target.lat = s_to_dbl(rec->b_lat,10);
	target.lon = s_to_dbl(rec->b_lon,10);

        if (source.lat > 90. || source.lat < -90. ||
            source.lon > 180. || source.lon < -180. ) {
             fprintf (stderr, 
                  "s_lat = %.4f\ts_lon = %.4f\te_lat = %.4f\te_lon = %.4f\n",
                   source.lat, source.lon, target.lat, target.lon);
             fprintf (stderr, "data out of range!\n");
             exit (2);
        }
        
	/* convert source to SSMI, and determine distance from input */
        ll_to_ssmi(source.lat, source.lon, &sourcex, &sourcey);
	diffx = sourcex-inputx;
	diffy = sourcey-inputy;
	temp = sqrt( (diffx*diffx) + (diffy*diffy) );

	/* if temp is less than min, then set min to temp and place 
	 * source and target lat/lon in head & tail.
	 */
        if (temp < min) {
	  min = temp;
	  *head = source;
	  *tail = target;
        }
	
    }
    
    /* return file to beginning & leave function */
    if (fseek(file,0L,0) != 0) fprintf(stderr,"Error in find...()\n");
    return;
}

