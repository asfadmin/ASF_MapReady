#include "asf.h"

#include "gps_vector.h"

void vector(oimage, fpdat, meta, gridspace, nr, nc, mag, 
	    pixel_size, vector_res, options, user)
unsigned char **oimage;
FILE *fpdat;
struct gps_meta *meta;
double gridspace;
int nr, nc;
int mag;
int pixel_size;
double vector_res;
int options;
LATLON user;
{
    struct gps_rec *rec;
    struct point imageA, imageB;
    struct point ref_pt;
    LATLON head;
    LATLON tail;
    int v, w;
    int start_row, end_row;
    int start_col, end_col;
    double rv, rw;
    double x, y;
    double min_xa,max_xa,min_ya,max_ya;
    double min_xb,max_xb,min_yb,max_yb;
    double SsmiAvgX, SsmiAvgY;
    int avgdispx, avgdispy,headx,heady,tailx,taily;
    char buf[1024];
    /* counter variables */
    int count;
    int draw; 

    /*
       Read in the Max and Min grid coords in both image A and B
       and the four corners of the overlapped area in image A and B.
    */
    min_xa = s_to_dbl(meta->min_x, 10);
    max_xa = s_to_dbl(meta->max_x, 10);
    min_ya = s_to_dbl(meta->min_y, 10);
    max_ya = s_to_dbl(meta->max_y, 10);
				    
    min_xb = s_to_dbl(meta->bmin_x, 10);
    max_xb = s_to_dbl(meta->bmax_x, 10);
    min_yb = s_to_dbl(meta->bmin_y, 10);
    max_yb = s_to_dbl(meta->bmax_y, 10);

    /* 
       Change SSMI max & min coords to gps i & j. Determine
       the number of rows & columns in our vector grid.
    */
    start_row = ssmi_gps(min_ya, min_ya, gridspace);
    end_row = ssmi_gps(max_ya, min_ya, gridspace);
    start_col = ssmi_gps(min_xa, min_xa, gridspace);
    end_col = ssmi_gps(max_xa, min_xa, gridspace);
    printf("Number of GPS Rows = %d\tNumber of GPS Cols = %d\n",
	   end_row - start_row, end_col - start_col);
    
    /* Determine # of rows and cols of avg. displacement */
    if (options & SUBTRACT_USER_AVG) {
        /*
	 * determine vector closest to give lat,lon in user
	 * convert lat,lon into SSMI
	 * find displacement in SSMI & place in avg. disp
	 */
        find_closest_vector(&user,&head,&tail,fpdat);
        
	ll_to_ssmi (head.lat, head.lon, &x, &y);
        headx = (int)((x - min_xb) * 1000) / pixel_size;
        heady = (int)((y - min_yb) * 1000) / pixel_size;  
	ll_to_ssmi(tail.lat,tail.lon,&x,&y);
        tailx = (int)((x - min_xb) * 1000) / pixel_size;
        taily = (int)((y - min_yb) * 1000) / pixel_size; 
        
	avgdispx = tailx - headx;
	avgdispy = taily - heady;
    } else {
        SsmiAvgX = s_to_dbl(meta->avgdispx, 10);
        SsmiAvgY = s_to_dbl(meta->avgdispy, 10);
        avgdispx = (int)(SsmiAvgX * 1000.0) / pixel_size;
        avgdispy = (int)(SsmiAvgY * 1000.0) / pixel_size;
    }

    /* skip the ceos header of the data file */
    fseek (fpdat, 720L, 0);
    /* first pass, read in data */
    count = draw = 0;
    while (fread(buf,sizeof(struct gps_rec),1,fpdat) != 0) {
        rec = (struct gps_rec *) buf;
        head.lat = s_to_dbl(rec->a_lat,10);
        head.lon = s_to_dbl(rec->a_lon,10);
	tail.lat = s_to_dbl(rec->b_lat,10);
	tail.lon = s_to_dbl(rec->b_lon,10);

        if (head.lat > 90. || head.lat < -90. ||
            head.lon > 180. || head.lon < -180. ||
            tail.lat > 90. || tail.lat < -90.  ||
            tail.lon > 180. || tail.lon < -180.) {

             fprintf (stderr, 
                  "s_lat = %.4f\ts_lon = %.4f\te_lat = %.4f\te_lon = %.4f\n",
                   head.lat, head.lon, tail.lat, tail.lon);
             fprintf (stderr, "data out of range!\n");
             exit (2);
        }
        
        /* convert head.lat and start.lon pair into i, j, pair */
        ll_to_ssmi (head.lat, head.lon, &x, &y);
        imageA.col = (int)((x - min_xb) * 10) / (int)(pixel_size/100);
        imageA.row = (int)((y - min_yb) * 10) / (int)(pixel_size/100);  
	imageA.gps_row = ssmi_gps(y,min_ya,gridspace) - start_row + 1;
	imageA.gps_col = ssmi_gps(x,min_xa,gridspace) - start_col + 1;
	
	ll_to_ssmi(tail.lat,tail.lon,&x,&y);
        imageB.col = (int)((x - min_xb) * 1000) / pixel_size;
        imageB.row = (int)((y - min_yb) * 1000) / pixel_size; 
	
	if (count == 0) {
	  ref_pt.row = imageA.row;
	  ref_pt.col = imageA.col;
	  ref_pt.gps_row = imageA.gps_row;
	  ref_pt.gps_col = imageA.gps_col;
        }
	
	/*
	   draw vector line from point A to point B, if it falls
	   within vector resolution
	*/	
	v = ref_pt.gps_col - imageA.gps_col;
	w = ref_pt.gps_row - imageA.gps_row;
	rv = fmod((double)v,vector_res);
	rw = fmod((double)w,vector_res);
	if (rv == 0.0 && rw == 0.0) {
	  if (options & SUBTRACT_META_AVG || options & SUBTRACT_USER_AVG) {
	     imageB.col -= avgdispx;
	     imageB.row -= avgdispy;
          }
	  draw_vector_arrow(oimage, imageA.col, imageA.row,
		             imageB.col, imageB.row, 255, mag, nr, nc);
          draw++;
	}
	count++;
    }
   
    /* Vector ouput information */
    printf("%d vectors drawn out of %d possible.\n",draw,count);

    return;
}
