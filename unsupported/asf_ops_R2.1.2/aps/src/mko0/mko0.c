#include <stdio.h>
#include <string.h>

#define E1

#define ST 0
#define ND 1

#define NR 0
#define FR 1
#define CT 2
struct eos_entry
{
   int day;
   int hr;
   int min;
   float sec;
   int rev;
   char ad;
   float lat[3];
   float lon[3];
   float float_tm;  /* number to make sorting easier */
} eos_table[10000];

struct o0_entry
{
   double time[3];
   float lat[5];
   float lon[5];
   int    ascdec;
}orbit0;

float frm_lat( 
   int frm_num,
   struct eos_entry *table,
   int tbl_siz,
   float orbit_tm
);

float tm_lat( 
   float in_tm,
   struct eos_entry *table,
   int tbl_siz,
   float orbit_tm,
   int item
);
float lat_tm( 
   float lat,
   struct eos_entry *table,
   int tbl_siz
);

float tm_lon( 
   float in_tm,
   struct eos_entry *table,
   int tbl_siz,
   float orbit_tm,
   int item
);

int read_eos_file(
   char *fname, 
   struct eos_entry *table
);

float lat_360(
   float lat_in,
   int ad
);

float lat_pm90(
   float lat_in,
   char *ad	/* rtn value */
);
float interp (
   float in, 
   float pre, 
   float post, 
   float out_pre, 
   float out_post
);

static float frm_tm(
   int frame, 
   struct eos_entry *eos_table, 
   int eos_entries, 
   float orbit_tm
);

void main(int argc, char **argv)
{

int i, eos_entries, frm_num, seg, stat;
int dhr, dmin, dsec;
char f1;
float orbit_tm;
float vel, frm_siz, frm_time;
float tim[3];
float st_lat[2], st_lon[2], nd_lat[2], nd_lon[2], ct_lat, ct_lon;
char ifnam[100], *fnp;


   if (argc < 2)
   {
      fprintf (stderr,"Usage: %s <eos_menu file>\n",argv[0]);
      exit(1);
   }

   strcpy(ifnam,argv[1]);
   fnp = strrchr(ifnam,'/');
   if (fnp == NULL) 
      fnp = ifnam; 
   else
      fnp = &fnp[1];
   frm_siz = 102.0;   /* KM for E1, E2, J1, R1STD */

   if (strncmp(fnp,"J1",2) == 0)
      vel = 6.90;      /* KM/S for J1 */

   if ((strncmp(fnp,"E1",2) == 0) ||
       (strncmp(fnp,"E2",2) == 0))
      vel = 6.67;      /* KM/S for E1 & E2 */

   if (strncmp(fnp,"R1",2) == 0)
      vel = 6.62;      /* KM/S for R1 */

   if (strncmp(&fnp[4],"SN",2) == 0)
     frm_siz = 308.0;   /* KM for ScanSAR Narrow */

   if (strncmp(&fnp[4],"SW",2) == 0)
      frm_siz = 512.0;   /* KM for ScanSAR Wide */

   frm_time = frm_siz/vel;
   fprintf (stderr,"vel = %.2f along track length %.2f frm_time = %.2f \n",
      vel, frm_siz, frm_time);

   /* read input file  (entire orbital info - sorted by time) */

   eos_entries = read_eos_file(argv[1], eos_table);
   fprintf (stderr, "read %d entries from eos file\n",eos_entries);
   
   orbit_tm = lat_tm(360.0, eos_table, eos_entries); 
   orbit_tm -= eos_table[0].float_tm;
   orbit_tm *= (360.0/(360.0 - eos_table[0].lat[CT]));

   fprintf (stderr,"time/orbit = %.2f S\n",orbit_tm);

   for (frm_num = 1; frm_num<=900; frm_num++)
   {
      seg = (frm_num % 900)/150;
      switch (seg)
      {
      case 0:
      case 2:
      case 3:
      case 5:
         /* find ctr lat for frame */
         ct_lat = frm_num * 0.4;

         /* center time for frame */
         tim[CT] = lat_tm(ct_lat, eos_table, eos_entries);  
         break;
      case 1:   /* above 60 N */
      case 4:   /* below 60 S */
         tim[CT] = frm_tm(frm_num, eos_table, eos_entries, orbit_tm);
         ct_lat = tm_lat(tim[CT], eos_table, eos_entries, orbit_tm, CT);
         break;
      }      

      ct_lat = lat_pm90(ct_lat, &f1);
      ct_lon = tm_lon(tim[CT], eos_table, eos_entries, orbit_tm, CT);

      /* start & end times for frame */
      tim[ST] = tim[CT] - (frm_time/2.0);
      tim[ND] = tim[CT] + (frm_time/2.0);


      /* near/far st/nd lat/lon */
      for (i=0; i<2; i++)
      {
         st_lat[i] = tm_lat(tim[ST], eos_table, eos_entries, orbit_tm,i);
         st_lat[i] = lat_pm90(st_lat[i], &f1);
         st_lon[i] = tm_lon(tim[ST], eos_table, eos_entries, orbit_tm,i);

         nd_lat[i] = tm_lat(tim[ND], eos_table, eos_entries, orbit_tm,i);
         nd_lat[i] = lat_pm90(nd_lat[i], &f1);
         nd_lon[i] = tm_lon(tim[ND], eos_table, eos_entries, orbit_tm,i);
      }

      /* printf ("frm %d - lat %.2f tm %.2f\n", frm_num, ct_lat, tim[CT]); */

      for (i=0; i<3; i++) 
      {
         dsec = (int)tim[i];
         dhr = dsec/3600;
         dsec %= 3600;
         dmin = dsec/60;
         dsec %= 60;
         printf ("%02d:%02d:%02d ",dhr,dmin,dsec);
      }
      for (i=0; i<2; i++) 
      {
        printf ("%.2f %.2f ",st_lat[i], st_lon[i]);
        printf ("%.2f %.2f ",nd_lat[i], nd_lon[i]);
      }
      printf ("%.2f %.2f\n", ct_lat, ct_lon);
   }
}


int read_eos_file(
   char *fname, 
   struct eos_entry table[]
)
{
FILE *ifp;
int count;
char linein[133], f1;
float lat;


   if ((ifp = fopen(fname,"r")) == NULL)
   {
      fprintf (stderr, "error opening input file %s - exiting\n",fname);
      return(-1);
   }

fprintf (stderr,"opened file %s\n",fname); 

   count = 0;
   while (fgets(linein, 132, ifp) != NULL)
   {
      sscanf(linein,"%d/%d:%d:%f %d %c %f %f %f %f",
         &table[count].day,
         &table[count].hr,
         &table[count].min,
         &table[count].sec,
         &table[count].rev,
         &table[count].ad,
         &table[count].lat[NR],
         &table[count].lon[NR],
         &table[count].lat[FR],
         &table[count].lon[FR]);

         /* normalize lats to 0-360 degrees */
         table[count].lat[NR] = lat_360(table[count].lat[NR], table[count].ad);
         table[count].lat[FR] = lat_360(table[count].lat[FR], table[count].ad);

       
         table[count].lat[CT] = table[count].lat[NR] + table[count].lat[FR];

         /* if NR/FR points stradle equator */
         if ((table[count].lat[NR] - table[count].lat[FR]) > 180)
            table[count].lat[CT] += 360;

         if ((table[count].lat[FR] - table[count].lat[NR]) > 180)
            table[count].lat[CT] += 360;

         table[count].lat[CT] /= (float) 2.0;

         table[count].float_tm = 
           table[count].sec +
           (float) table[count].min * 60.0 +
           (float) table[count].hr * 3600.0 +
           (float) table[count].day* (24.0 * 3600.0);

         /* rolled over > 360 in one file */
         if ((count > 10) && (table[count].lat[CT] < table[(count-1)].lat[CT]))
              table[count].lat[CT] += 360.0;

         if (table[count].lon[NR] < 0.0)  table[count].lon[NR]  += 360.0;
         if (table[count].lon[FR] < 0.0)  table[count].lon[FR]  += 360.0;

         if ((table[count].lon[NR] - table[count].lon[FR]) > 180.0) 
            table[count].lon[FR] += 360.0;
         if ((table[count].lon[FR] - table[count].lon[NR]) > 180.0) 
             table[count].lon[NR] += 360.0;

         table[count].lon[CT] = 
            (table[count].lon[NR] + table[count].lon[FR])/2.0;

#if 0
fprintf (stderr,"DBG[%d] TM %f LAT %f %f %f LON %f\n",
   count, table[count].float_tm, 
   table[count].lat[NR], table[count].lat[CT], table[count].lat[FR], 
   table[count].lon[CT]);
#endif


    if ((count > 0) || (table[count].lat[CT] < 10.0))
       count++;
   }
fprintf (stderr,"EOS TBL Start TM %f, ND tm %f #entries %d\n",
table[0].float_tm, table[count-1].float_tm, count);
   return(count);
}


static float frm_tm(
   int frame, 
   struct eos_entry *table, 
   int tbl_siz, 
   float orbit_tm
)
{
float tm1, tm2, tm_frm, r_time;
float lat1, lat2;
int offset;

      if (frame < 150) return(-1);
      if (frame > 750) return(-1);
      if ((frame > 300) && (frame < 600)) return(-1);

      if (frame <= 300) 
      {
         lat1 = 60.0;
         lat2 = 120.0;
         offset = 150;
      } else {
         lat1 = 240.0;
         lat2 = 300.0;
         offset = 600;
      }

      tm1 = lat_tm(lat1, table, tbl_siz);
      tm2 = lat_tm( lat2, table, tbl_siz);
      tm_frm = (tm2 - tm1)/150.0;

      /* time of frame N */
      r_time = tm1 + (tm_frm * (frame - offset));
      return(r_time);
}

/* calc lat from time*/
float tm_lon( 
   float in_tm,
   struct eos_entry *table,
   int tbl_siz,
   float orbit_tm,
   int item
)
{
#define SEC_DAY (24*3600)
int ndx, ndx1, ndx2;
float t1, l1, lon1, lon2, offset, lat_orbit;

   lat_orbit = 360.0 * (orbit_tm/SEC_DAY); /* 360.0 * (35/501); */

   ndx = 0;
   while ((table[ndx].float_tm <= in_tm) && (ndx < tbl_siz)) ndx++;

   if (ndx == tbl_siz) 
   {
      /* fprintf (stderr,"Extrapolating (lon) for time = %f\n",in_tm); */
      ndx = tbl_siz - 1;
   }

   if (ndx == 0) 
   {
      /* fprintf (stderr,"Extrapolating (lon) for time = %f\n",in_tm); */
      ndx =  1;
   }

   ndx2 = ndx; ndx1 = ndx -1;
   lon1 = table[ndx1].lon[item];
   lon2 = table[ndx2].lon[item];

   if ((lon1 - lon2) > 180.0) lon2 += 360;
   if ((lon2 - lon1) > 180.0) lon1 += 360;

   l1 = interp (in_tm, table[ndx1].float_tm, table[ndx2].float_tm,
      lon1, lon2);

   while (l1 > 180.0) l1 -= 360.0;
   while (l1 < -180.0) l1 += 360.0;

   return(l1);

}
float tm_lat( 
   float in_tm,
   struct eos_entry *table,
   int tbl_siz,
   float orbit_tm,
   int item
)
{
int ndx, ndx1, ndx2;
float t1, l1, lat1, lat2;
char f1;

   if (in_tm <= table[0].float_tm ) in_tm += orbit_tm;

   ndx = 0;
   while ((table[ndx].float_tm <= in_tm) && (ndx < tbl_siz)) ndx++;

   if (ndx == tbl_siz) 
   {
      /* fprintf (stderr,"Extrapolating (lat) for time = %f\n",in_tm); */
      ndx = tbl_siz - 1;

   }

   ndx2 = ndx; ndx1 = ndx -1;

   lat1 = lat_pm90(table[ndx1].lat[item], &f1);
   lat2 = lat_pm90(table[ndx2].lat[item], &f1);

   l1 = interp (in_tm, table[ndx1].float_tm, table[ndx2].float_tm,
      lat1, lat2);

   l1 = lat_360(l1, table[ndx1].ad);

   return(l1);
}

/* calc lat from time */
float lat_tm( 
   float lat,
   struct eos_entry *table,
   int tbl_siz
)
{
int ndx1, ndx2, ndx;
float f1, t1, orbit_tm;

   /* find table entry immediately prior to lat */
   ndx = 0;
   while ((table[ndx].lat[CT] <= lat) && (ndx < tbl_siz)) ndx++;
   if (ndx == tbl_siz) 
   {
      fprintf (stderr,"Extrapolating (tm-hi) for lat = %f\n",lat);
      ndx = tbl_siz - 1;
   }
   if (ndx == 0)
   {
      fprintf (stderr,"Extrapolating (tm-lo) for lat = %f\n",lat);
      ndx = 1;
   }
   
   ndx2 = ndx; ndx1 = ndx -1;

   t1 = interp (lat, table[ndx1].lat[CT], table[ndx2].lat[CT],
           table[ndx1].float_tm, table[ndx2].float_tm);

   return(t1);

}

float lat_pm90(
   float lat_in,
   char *ad
)
{
   if (lat_in <= 90.0)  { *ad = 'A'; return(lat_in);}
   if (lat_in <= 270.0) { *ad = 'D'; return(180.0 - lat_in);}
   while (lat_in > 270.0)  {lat_in -= 360; } 
   *ad = 'A'; 
   return(lat_in);
}

float lat_360(
   float lat_in,
   int ad
)
{
   while (lat_in > 360.0) lat_in -= (float)360.0;
   if ((lat_in >= 0.0) && (ad == 'A')) return(lat_in);
   if ((lat_in >= 0.0) && (ad == 'D')) return (180.0 - lat_in);
   if ((lat_in < 0.0) && (ad == 'D')) return(180.0 - lat_in);
   if ((lat_in < 0.0) && (ad == 'A')) return(360.0 + lat_in);
}

float interp (
   float in, 
   float pre, 
   float post, 
   float out_pre, 
   float out_post
)
{
float pc, rv;
   pc =  (in - pre)/(post - pre);
   return (out_pre + (pc * (out_post - out_pre)));
}
