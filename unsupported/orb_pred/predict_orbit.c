/****************************************************************
FUNCTION NAME:  predict_orbit

SYNTAX:   void predict_orbit(head_dat,orb_dat,dat_lines,filepos,asc_flag)


PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    head_dat	HEADER_DATA	header_data structure
    orb_dat	ORBIT_DATA[]	contains parameters of orbit
    dat_lines	int		number of lines of orbit info.
    filepos     char *		file of input points
    asc_flag    int		determine if orbit calculation is for
				ascending or descending orbits.

DESCRIPTION:
    Calculate orbits for phases A,B, & D for specified ground location.

RETURN VALUE:
    None

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    1.0         M. Shindle	Original Development

****************************************************************/
#include "asf.h"

#include "orb_pred.h"

void calc_orb(HEADER_DATA,int*,int*,int*,int*,int*,int,int,int);

/* 
  variable used to keep track pf years while printing.
  Must be set to zero before each point is calculated.
*/
int last_year;


void predict_orbit(head_dat, orb_dat, dat_lines, filepos, asc_flag)
HEADER_DATA head_dat;
ORBIT_DATA orb_dat[];
int dat_lines;
char *filepos;
int asc_flag;  /* are orbit values for ascending or descending mode? */
{
  int	i, j, k, m, n, diff;
  int	e_orb, e_orb1, w_orb, w_orb1, c_orb, n_path;
  double c_date, w_date, w_date1, e_date, e_date1;
  double hr[5], raw_day[5], day[5]; 
  int yr[5], min[5], rev[5];
  int orbit_mod;
  double dist1, det_diff;
  char strng[60];
  int r_days, r_orbs;
  double cycle, dist;
  double tmp1, tmp2, tmp3, tmp4;
  double pt_lat, pt_lon, longi, cf[3];
  double swcn_lat[4];
  double swcn_lon[4];
  double mid_lat[6], mid_lon[6];
  double pi, p1[2], p2[2], *midp;
  double a1[3][3];
  FILE *fp;
  
  /* set common variables */
  pi = 4. * atan(1.);
  n = 4;
  m = 3;
  for (i=0;i<3;i++) {
    swcn_lat[i] = 0.0;
    swcn_lon[i] = 0.0;
  }

  /* open input position file */
  if ((fp=fopen(filepos,"r")) == NULL) {
    fprintf(stderr,"Cannot open file: %s\nFatal Error.\n",filepos);
    exit(2);
  }

  while (fscanf(fp,"%le %le %s", &pt_lat, &pt_lon, strng) >= 1) {
      printf("\nFor given point at %s, Lat = %.5f\tlon = %.5f\n",
		strng, pt_lat, pt_lon);
      last_year = 0;
      
      /* determine coeff's for orbit values. Also check to see if
	 modifier is needed for ascending values. */
      assign_orbit_values(head_dat,&cycle,&r_days,&r_orbs,&n_path);
      if (asc_flag && (pt_lat >= 0.0))
	 orbit_mod = 1;
      else
	 orbit_mod = 0;
      swcn_lat[2] = orb_dat[0].alat;
      swcn_lon[2] = orb_dat[0].alon;
      swcn_lat[3] = orb_dat[0].blat;
      swcn_lon[3] = orb_dat[0].blon;
      k = 0;
      for (i=1;i<dat_lines;i++) {
         tmp1 = orb_dat[i].alat;
	 tmp2 = orb_dat[i].alon;
	 tmp3 = orb_dat[i].blat;
	 tmp4 = orb_dat[i].blon;
         swcn_lat[0] = swcn_lat[2];
         swcn_lon[0] = swcn_lon[2];
         swcn_lat[1] = swcn_lat[3];
         swcn_lon[1] = swcn_lon[3];
         swcn_lat[2] = tmp1;
         swcn_lon[2] = tmp2;
         swcn_lat[3] = tmp3;
         swcn_lon[3] = tmp4;
	 k++;
         tmp1 = 0.5 * (swcn_lat[0] + swcn_lat[1]);
	 tmp2 = 0.5 * (swcn_lat[2] + swcn_lat[3]);
	 if ( (tmp1 > pt_lat) ^ (tmp2 > pt_lat) )
           break;
      }

      /* calculate mid-points between corner points */
      for (i = 0; i < n; i++) {
	p1[0] = swcn_lat[i];
	p1[1] = swcn_lon[i];
	p2[0] = swcn_lat[(i+1)%n];
	p2[1] = swcn_lon[(i+1)%n];
	midp = midpoint(p1, p2);
	mid_lat[i] = midp[0];
	mid_lon[i] = midp[1];
      }
	
      /* calculate the central line from the side mid-points */
      dist = E_CIRCM / r_orbs * cos(pt_lat * pi / 180.);
      for (i = 0; i < n/2; i++) {
	p1[0] = mid_lat[i];
	p1[1] = mid_lon[i];
	p2[0] = mid_lat[i+2];
	p2[1] = mid_lon[i+2];
	midp = midpoint(p1, p2);
	mid_lat[i+n] = midp[0];
	mid_lon[i+n] = midp[1];
      }
      mid_lat[1] = 0.5 * (mid_lat[n] + mid_lat[n+1]);
      mid_lon[1] = 0.5 * (mid_lon[n] + mid_lon[n+1]);

      /* create matrix */
      for (i = 0; i < m; i++) {
	a1[i][0] = 1.;
	a1[i][1] = mid_lat[i];
	a1[i][2] = mid_lat[i] * mid_lat[i];
      }

      /*
	solve the linear equation system A X = B
	where 	A = a1
		X = cf
		B = mid_lon
      */
      solveAXB(a1,cf,mid_lon,m);

      longi = cf[0] + cf[1] * pt_lat + cf[2] * pt_lat * pt_lat;
      if (longi > pt_lon) {
	diff = ((int) ((longi - pt_lon) * r_orbs / 360 + 0.5)
		      * n_path + r_orbs * 1000) % r_orbs;
	det_diff = (int) ((longi - pt_lon) * r_orbs / 360 + 0.5)
		         - ((longi - pt_lon) * r_orbs / 360.);
      } else {
	diff = ((int) ((longi - pt_lon) * r_orbs / 360 - 0.5)
		      * n_path + r_orbs * 1000) % r_orbs;
	det_diff = (int) ((longi - pt_lon) * r_orbs / 360 - 0.5)
		         - ((longi - pt_lon) * r_orbs / 360.);
      }
     
      dist1 = det_diff * dist;
      if  (dist1 > 0.)
	printf ("The point is %.2f km to the east of %s\n",
		dist1, "the center of the central swath");
      else
	printf ("The point is %.2f km to the west of %s\n",
		(-1. * dist1), "the center of the central swath");
      printf("The distance between adjacent swaths is: %.2f km.\n", dist);
      printf("The available swaths are:\n");
      printf("West2_Orbits\tWest1_Orbits\tCenter-Orbits");
      printf("\tEast1_Orbits\tEast2_Orbits\n");
      printf("(Rev/Day/Time)\t(Rev/Day/Time)\t(Rev/Day/Time)");
      printf("\t(Rev/Day/Time)");
      printf("\t(Rev/Day/Time)\n");
      
      /* calculate orbits */
      calc_orb(head_dat,&w_orb1,&w_orb,&c_orb,&e_orb,&e_orb1,
	       diff,n_path,r_orbs);

      c_date = head_dat.o_date + diff/cycle + (k + 0.5) / (24. * 60.);
      w_date = c_date + (w_orb - c_orb) / cycle;
      w_date1 = c_date + (w_orb1 - c_orb) / cycle;
      e_date = c_date + (e_orb - c_orb) / cycle;
      e_date1 = c_date + (e_orb1 - c_orb) / cycle;

      for (j = (head_dat.s_rev - c_orb)/head_dat.r_rev;
	   j <= (head_dat.e_rev - c_orb)/head_dat.r_rev; 
	   j++) {
        
	/* Calculate orbit revolution */
	rev[0] = (w_orb1 + j * r_orbs) + orbit_mod;
	rev[1] = (w_orb + j * r_orbs) + orbit_mod;
	rev[2] = (c_orb + j * r_orbs) + orbit_mod;
	rev[3] = (e_orb + j * r_orbs) + orbit_mod;
	rev[4] = (e_orb1 + j * r_orbs) + orbit_mod;

        /* calculate day based on known orbit */
	raw_day[0] = (w_date1 + j * r_days); 
        raw_day[1] = (w_date + j * r_days);
	raw_day[2] = (c_date + j * r_days);
	raw_day[3] = (e_date + j * r_days);
	raw_day[4] = (e_date1 + j * r_days);
        /* format raw_day into appropriate year,day,hour,minute */ 
	for (i=0;i<5;i++) {
	  format_date(raw_day[i],head_dat.o_year,&day[i],&yr[i]);
	  hr[i] = (day[i] - (int)day[i]) * 24.0;
          min[i] = (hr[i] - (int)hr[i]) * 60;
        }

       print_table(rev,day,hr,min,yr,-1);
      }
  }

  fclose(fp);
  return; 
}

/* Calculate orbit numbers */
void calc_orb(hd,w1,w,c,e,e1,diff,n_path,r_orb)
HEADER_DATA hd;
int *w1;  /* far western orbit */
int *w;   /* near western orbit */
int *c;   /* center orbit */
int *e;   /* near eastern orbit */
int *e1;  /* far eastern orbit */
int diff; /* distance between orbits */
int n_path;
int r_orb;
{
      *c = hd.cur_orb + diff;
      *w = hd.cur_orb + (diff + n_path + r_orb * 1000)%r_orb;
      *w1 = hd.cur_orb + (diff +n_path + n_path + r_orb * 1000)%r_orb;
      *e = hd.cur_orb - r_orb + (diff - n_path + r_orb * 1000)%r_orb;
      *e1 = hd.cur_orb - r_orb +(diff - n_path - n_path + r_orb*1000)%r_orb;

      /* make modifications based on repeat of orbit */
      if (hd.r_rev == E168_ORBS || hd.r_rev == E35_ORBS) {
	while (*c < (double)hd.s_rev)
	   *c += r_orb;
        while (*c > (double)hd.e_rev)
	   *c -= r_orb;
        while (*w > (double)hd.e_rev)
	   *w -= r_orb;
        while (*w1 > (double)hd.e_rev)
	   *w1 -= r_orb;
        while (*e < (double)hd.s_rev)
	   *e += r_orb;
        while (*e1 < (double)hd.s_rev)
	   *e1 += r_orb;
      } else {
        while (*e < 0.) 
           *e += r_orb;
        while (*e1 < 0.) 
  	   *e1 += r_orb;
      }
      
      return;
}
