/* rcal_ceos.c is a collection of ceos interpretation routines.
 *
 * For extensive comments, please see the end of this file 
 *
 */

#include LIBINC

/* Leader file: */
/* volume descriptor record (ldr and tlr file record 1)        VDR */
/* data set summary record (ldr file record 2)                 DSS */
/* map projection data record (ldr file 2.5)                   MAP */
/* platform position record (ldr file record 3)                PPR */
/* attitude data record (ldr file record 4)                    ADR */
/* radiometric data record (ldr file record 5)                 RDR */
/* radiometric compensation record (ldr file record 6)         RCR */
/* data quality summary record (ldr file record 7)             DQS */
/* data histogram record (ldr file record 8)                   DHR */
/* range spectra record (ldr file record 9)                    RSR */

/* G E T   F U N C T I O N S */

/* the following is arguably the most general of all these functions: */
void rcal_get_test_string_from_arbitrary_record (s, fnm, record, a, b)
double *s;
char   *fnm;
int    record;
int    a, b;
{
  int i;

  i = rcal_ceos_get_value (s, STRING, fnm, record, a, b);
  if (i == FALSE) Exit ("rcal_get_test_string_from_arbitrary_record: test string failed");
  return;
}


void rcal_get_test_double_FRR(x, fnm, a, b)
double  *x;
char    *fnm;
int     a, b;
{
  int i;

  i = rcal_ceos_get_value (x, DOUBLE, fnm, FRR, a, b);
  if (i == FALSE) Exit ("rcal_get_test_double_FRR: test integer failed");
  return;
}

void rcal_get_test_integer_FRR(n, fnm, a, b)
int  *n;
char *fnm;
int   a, b;
{
  int i;

  i = rcal_ceos_get_value (n, INTEGER, fnm, FRR, a, b);
  if (i == FALSE) Exit ("rcal_get_test_integer_FRR: test integer failed");
  return;
}

void rcal_get_test_string_FRR(s, fnm, a, b)
char *s;
char *fnm;
int   a, b;
{
  int i;

  i = rcal_ceos_get_value (s, STRING, fnm, FRR, a, b);
  if (i == FALSE) Exit ("rcal_get_test_string_FRR: test integer failed");
  return;
}

void rcal_get_test_double(x, fnm, a, b)
double *x;
char   *fnm;
int    a, b;
{
  int i;

  i = rcal_ceos_get_value (x, DOUBLE, fnm, NUL, a, b);
  if (i == FALSE) Exit ("rcal_get_test_double: test double failed");
  return;
}

void rcal_get_test_integer(n, fnm, a, b)
int  *n;
char *fnm;
int   a, b;
{
  int i;

  i = rcal_ceos_get_value (n, INTEGER, fnm, NUL, a, b);
  if (i == FALSE) Exit ("rcal_get_test_integer: test integer failed");
  return;
}

void rcal_get_test_string(s, fnm, a, b)
char   *s;
char   *fnm;
int    a, b;
{
  int i;

  i = rcal_ceos_get_value (s, STRING, fnm, NUL, a, b);
  if (i == FALSE) Exit ("rcal_get_test_string: test string failed");
  return;
}

void rcal_get_carrier_frequency (v, fnm)
double *v;
char   *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (v,  DOUBLE, tf, FRR, 17, 17));

  return;
}

/* Jason mod -- created this function to get processor version # */
void rcal_get_processor_version (s, fnm)
char *s;
char *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (s,  STRING, tf, VDR, 31, 11));

  return;
}

/* Jason mod -- created this function to get actual image id # */
void rcal_get_image_id (s, fnm)
char *s;
char *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (s,  STRING, tf, FRR, 34, 10));

  return;
}

/* Jason mod -- changed name from rcal_get_image_id to rcal_get_rev_id */
void rcal_get_rev_id (s, fnm)
char *s;
char *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (s,  STRING, tf, FRR, 20, 14));

  return;
}

/* Jason mod -- created this function.  It reads in the time that the SAR
 *              image was actually processed by the SPS.
 */
void rcal_get_correlation_time (s, fnm)
char *s;
char *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (s,  STRING, tf, FRR, 45, 21));

  return;
}

void rcal_get_utc_time (s, fnm)
char *s;
char *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (s,  STRING, tf, FRR, 100, 22));

  return;
}

void  rcal_get_image_lat_lon(cnla, cnlo, ulla, ullo, llla, lllo, urla, urlo, lrla, lrlo, fnm)
double  *cnla, *cnlo, *ulla, *ullo, *llla, *lllo, *urla, *urlo, *lrla, *lrlo;
char    *fnm;
{
  int i, m, n;
  char tf[256];

  m = 122;
  n = 17;
  rcal_create_trailer_filename (fnm, tf);

  check (rcal_ceos_get_value (cnla, DOUBLE, tf, FRR, m,                   n));
  check (rcal_ceos_get_value (cnlo, DOUBLE, tf, FRR, m+n,                 n));
  check (rcal_ceos_get_value (ulla, DOUBLE, tf, FRR, m+n+n,               n));
  check (rcal_ceos_get_value (ullo, DOUBLE, tf, FRR, m+n+n+n,             n));
  check (rcal_ceos_get_value (llla, DOUBLE, tf, FRR, m+n+n+n+n,           n));
  check (rcal_ceos_get_value (lllo, DOUBLE, tf, FRR, m+n+n+n+n+n,         n));
  check (rcal_ceos_get_value (urla, DOUBLE, tf, FRR, m+n+n+n+n+n+n,       n));
  check (rcal_ceos_get_value (urlo, DOUBLE, tf, FRR, m+n+n+n+n+n+n+n,     n));
  check (rcal_ceos_get_value (lrla, DOUBLE, tf, FRR, m+n+n+n+n+n+n+n+n,   n));
  check (rcal_ceos_get_value (lrlo, DOUBLE, tf, FRR, m+n+n+n+n+n+n+n+n+n, n));

  return;
}

void rcal_get_rad_comp (a1, a2, a3, nr, fnm)
double *a1, *a2, *a3, *nr;
char *fnm;
{
  int i, m, n;
  char lf[256];
 
  rcal_create_leader_filename (fnm, lf);
  m = 84; n = 16;
  check (rcal_ceos_get_value (a1, DOUBLE, lf, RDR, m,     n));
  check (rcal_ceos_get_value (a2, DOUBLE, lf, RDR, m+n,   n));
  check (rcal_ceos_get_value (a3, DOUBLE, lf, RDR, m+n+n, n));

  m = 142; n = 16;
  for (i = 0; i < 256; i++) {
    check (rcal_ceos_get_value (nr+i, DOUBLE, lf, RDR, m + i*n, n));
  }

  return;
}

void rcal_get_img_pix_dims (nr, na, vr, va, fnm) 
int *nr;       /* number of range pixels   */
int *na;       /* number of azimuth pixels */
int *vr;       /* valid range pixels       */
int *va;       /* valid azimuth pixels     */
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 325;
  n = 9;

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (vr, INTEGER, tf, FRR, m,       n));
  check (rcal_ceos_get_value (va, INTEGER, tf, FRR, m+n,     n));
  check (rcal_ceos_get_value (nr, INTEGER, tf, FRR, m+n+n,   n));
  check (rcal_ceos_get_value (na, INTEGER, tf, FRR, m+n+n+n, n));
  return;
}

void  rcal_get_3_state_vectors(sv, fnm)
double *sv;
char *fnm;
{
  int i, m, n;
  double *dp;
  char lf[256];

  m = 386;
  n = 22;
  rcal_create_leader_filename (fnm, lf);

  for (i = 0; i < 18; i++) {
    dp = sv + i;
    check (rcal_ceos_get_value (dp,  DOUBLE, lf, PPR, m + i*n, n));
  }

  return;
}

void  rcal_get_state_vector(x, y, z, vx, vy, vz, fnm)
double *x, *y, *z, *vx, *vy, *vz;
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 574;
  n = 23;
  rcal_create_trailer_filename (fnm, tf);

  check (rcal_ceos_get_value (x,  DOUBLE, tf, FRR, m,           n));
  check (rcal_ceos_get_value (y,  DOUBLE, tf, FRR, m+n,         n));
  check (rcal_ceos_get_value (z,  DOUBLE, tf, FRR, m+n+n,       n));
  check (rcal_ceos_get_value (vx, DOUBLE, tf, FRR, m+n+n+n,     n));
  check (rcal_ceos_get_value (vy, DOUBLE, tf, FRR, m+n+n+n+n,   n));
  check (rcal_ceos_get_value (vz, DOUBLE, tf, FRR, m+n+n+n+n+n, n));

  return;
}

void rcal_get_platform_alt(alt, fnm)
double *alt;
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 557;
  n = 17;

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (alt, DOUBLE, tf, FRR, m,   n));
  return;
}

void rcal_get_boresight_angle(angle, fnm)
double *angle;
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 470;
  n = 22;

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (angle, DOUBLE, tf, FRR, m,   n));
  return;
}

void rcal_get_earth_radii(r1, r2, fnm)
double *r1, *r2;
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 832;
  n = 17;

  /* r1 = nadir radius, r2 = image center radius */
  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (r1, DOUBLE, tf, FRR, m,   n));
  check (rcal_ceos_get_value (r2, DOUBLE, tf, FRR, m+n, n));
  return;
}

void rcal_get_slant_ranges(s1, s2, fnm)
double *s1, *s2;
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 1085;
  n = 17;

  /* s1 = near slant, s2 = far slant */
  rcal_create_trailer_filename (fnm, tf);

  check (rcal_ceos_get_value (s1, DOUBLE, tf, FRR, m,   n));
  check (rcal_ceos_get_value (s2, DOUBLE, tf, FRR, m+n, n));
  return;
}

void rcal_get_proc_gain(g, fnm)
double *g;
char *fnm;
{
  int i;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (g, DOUBLE, tf, FRR, 1065, 9));
  return;
}

void  rcal_get_resolution_and_pixel_size (azres, rnres, azpix, rnpix, fnm)
double *azres, *rnres, *azpix, *rnpix;
char *fnm;
{
  int i, m, n;
  char tf[256];

  m = 1238;
  n = 17;

  rcal_create_trailer_filename (fnm, tf);
  check (rcal_ceos_get_value (azres, DOUBLE, tf, FRR, m,       n));
  check (rcal_ceos_get_value (rnres, DOUBLE, tf, FRR, m+n,     n));
  check (rcal_ceos_get_value (azpix, DOUBLE, tf, FRR, m+n+n,   n));
  check (rcal_ceos_get_value (rnpix, DOUBLE, tf, FRR, m+n+n+n, n));

  return;
}


/*
 *
 *  For Stubborn, Pesky, Archaic, Ancient Trailer Files...
 * 
 */

void  rcal_brute_get_proc_gain (g, fnm)
double *g;
char *fnm;
{
  FILE   *fp, *fopen();
  char   c[2000];
  int    i, j, count;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  fp = fopen(tf, "r");

  /* processor gain is 134'th item in trailer file, give or take */
  for (i = 0; i < 133; i++) j = fscanf(fp, "%s", c);
  i = fscanf(fp, "%s", c); *g  = (double)(atof(c));

  /* brute force shift stuff */
  if (strcmp (c, "YES") == 0) 
    Exit ("rcal_brute_get_proc_gain: went one too far");
  i = fscanf(fp, "%s", c); 
  while (strcmp (c, "YES") != 0) {
    *g  = (double)(atof(c));
    i = fscanf(fp, "%s", c); 
  }

  fclose (fp);
  return;
}

void  rcal_brute_get_slant_ranges (r1, r2, fnm)
double *r1, *r2;
char *fnm;
{
  FILE   *fp, *fopen();
  char   c[2000];
  int    i, j, count;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  fp = fopen(tf, "r");

  /* slant ranges are items 137 and 138 in trailer file, give or take */
  for (i = 0; i < 136; i++) j = fscanf(fp, "%s", c);
  i = fscanf(fp, "%s", c); *r1 = (double)(atof(c));
  i = fscanf(fp, "%s", c); *r2 = (double)(atof(c));

  /* brute force for case of shift of one or two */
  if (*r1 < 500.0 || *r1 > 1500.0) {

    /* case of both bad; shift down two, do it again */
    if (*r2 < 500.0 || *r2 > 1500.0) {
      i = fscanf(fp, "%s", c); *r1 = (double)(atof(c));
      i = fscanf(fp, "%s", c); *r2 = (double)(atof(c));
    }

    /* case of only r1 bad; shift down one, do it again */
    else {
      *r1 = *r2;
      i = fscanf(fp, "%s", c); 
      *r2 = (double)(atof(c));
    }
  }

  if (*r1 <  500.0 || *r2 <  500.0 ||
      *r1 > 1500.0 || *r2 > 1500.0 ||
      *r1 >= *r2   )
    Exit ("rcal_brute_get_slant_ranges:  bad get_brute_slant_range");


  fclose (fp);
  return;
}

void  rcal_brute_get_img_pix_dims (nr, na, vr, va, fnm)
int *nr, *na, *vr, *va;
char *fnm;
{
  FILE   *fp, *fopen();
  char   c[2000];
  int    i, j, count;
  char tf[256];

  rcal_create_trailer_filename (fnm, tf);
  fp = fopen(tf, "r");

  /* img pix dims are items 79-82 in trailer file, give or take */
  for (i = 0; i < 78; i++) j = fscanf(fp, "%s", c);

  *vr = 0;
  while (*vr < 500 || *vr > 8192) {
    i = fscanf(fp, "%s", c); *vr = atoi(c);
  }

  /* get the rest */
  i = fscanf(fp, "%s", c); *va = atoi(c);
  i = fscanf(fp, "%s", c); *nr = atoi(c);
  i = fscanf(fp, "%s", c); *na = atoi(c);

  fclose (fp);
  return;
}


/* G E N E R A L   R E T R I E V E R */

static int rcal_ceos_get_value(v, vt, fnm, rt, fb, nb)
void *v;       /* value                    */
int  vt;       /* value type               */
char *fnm;     /* ceos file name           */
int  rt;       /* record type              */
int  fb;       /* data field first byte    */
int  nb;       /* number of bytes in field */
{
  uchar buf[8192], hdr[12], *bp;
  int   fd, i, rcln, rcct, rctp;
  char  msg[256];

  /* simple test, make sure we are ok */
  if (fb < 0 || fb > 8192) 
    Exit ("rcal_ceos_get_value:  silly first byte value passed");

  /* fnm is passed as an argument */
  fd   = open (fnm, 0);
  sprintf(msg, "rcal_ceos_get_value():  open failed for file %s", fnm);
  if (fd < 3) Exit (msg);

  rcct = 0;

  do {

    /* read first 12 (header) bytes */
    i = read (fd, hdr, 12);
    if (i != 12) Exit ("rcal_ceos_get_value():  failed to read record header");
    rcct++;

    /* determine record length and type */
    rctp = rcal_get_record_type(hdr);

    /* read remainder of record */
    if (rctp == NUL) { /* test case of an unknown record */

      /* if (rt != NUL) Exit ("rcal_ceos_get_value():  found NUL record, unrequested");  */
      if (rt != NUL) Alert ("rcal_ceos_get_value():  found NUL record, unrequested");  

      rcln = fb + nb;
      if (rcln > 8192) Exit ("rcal_ceos_get_value(): record too long");
      i = (int)(lseek (fd, 0, 0));
      if (i != 0) Exit ("rcal_get_ceos_value():  failed to lseek to 0 in test case");
      i = read (fd, buf, rcln);
      if (i != rcln) Exit ("rcal_get_ceos_value():  failed to read record");
    }
    else { /* the usual case of a proper CEOS record or and OLD record */
      rcln = (hdr[8]<<24) + (hdr[9]<<16) + (hdr[10]<<8) + (hdr[11]);        
      if (rcln > 8192) Exit ("rcal_ceos_get_value: record too long");
      i = read (fd, buf, rcln-12);
      if (i != rcln - 12) Exit ("rcal_get_ceos_value():  failed to read record");
    }

  } while (rctp != rt);

  close (fd);

  /* at this point the function has either:
   *   a) messed up and HALTED, or...
   *   b) found an unrecognizable (test) record, and read sufficient bytes into 'buf', or...
   *   c) read the correct record and has its entire contents in 'buf', with the first
   *        twelve bytes in 'hdr'
   */

  /* case b) */
  if (rt == NUL) {
  
    bp = buf + fb;
    if      (vt == INTEGER) { rcal_get_integer_value (bp, nb, (int *)(v)); }
    else if (vt == DOUBLE)  { rcal_get_double_value  (bp, nb, (double *)(v)); }
    else if (vt == STRING)  { *(bp+nb) = '\0'; strncpy ((char *)(v), (char *)(bp), nb+1); }
    else                    { Exit ("rcal_ceos_get_value():  unfam. data type "); }
    return;

  }
 
  /* case c) */
  if (fb < 12) { /* requested header information */
    Exit ("rcal_ceos_get_value():  not set to return info from 12 byte header"); 
  }
 
  else { /* requested buffer information */

    bp = buf + fb - 12;
    
    if      (vt == INTEGER) { rcal_get_integer_value (bp, nb, (int *)(v)); }
    else if (vt == DOUBLE)  { rcal_get_double_value  (bp, nb, (double *)(v)); }
    else if (vt == STRING)  { *(bp+nb) = '\0'; strncpy ((char *)(v), (char *)(bp), nb+1); }
    else                    { Exit ("rcal_ceos_get_value():  unfam. data type "); }
  }
 
  return TRUE;
}

static int rcal_get_record_type(hdr)
uchar hdr[12];
{
    int rt;

    /* records common to both leader and trailer files */
    /* volume descriptor record (ldr and tlr file record 1) */
    if      ( hdr[4] ==  63 && hdr[5] == 192 && hdr[6] ==  31 && hdr[7] ==  18 ) { rt = VDR; }

    /* leader file records */
    /* Jason mod -- added the map projection record that Rob forgot... */
    else if ( hdr[4] ==  10 && hdr[5] ==  20 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = MAP; }
    /* data set summary record (ldr file record 2) */
    else if ( hdr[4] ==  10 && hdr[5] ==  10 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = DSS; }
    /* platform position record (ldr file record 3) */
    else if ( hdr[4] ==  10 && hdr[5] ==  30 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = PPR; }
    /* attitude data record (ldr file record 4) */
    else if ( hdr[4] ==  10 && hdr[5] ==  40 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = ADR; }
    /* radiometric data record (ldr file record 5) */
    else if ( hdr[4] ==  10 && hdr[5] ==  50 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = RDR; }
    /* radiometric compensation record (ldr file record 6) */
    else if ( hdr[4] ==  10 && hdr[5] ==  51 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = RCR; }
    /* data quality summary record (ldr file record 7) */
    else if ( hdr[4] ==  10 && hdr[5] ==  60 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = DQS; }
    /* data histogram record (ldr file record 8) */
    else if ( hdr[4] ==  10 && hdr[5] ==  70 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = DHR; }
    /* range spectra record (ldr file record 9) */
    else if ( hdr[4] ==  10 && hdr[5] ==  80 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = RSR; }

    /* trailer file records */
    /* detailed processing parameters record (tlr file record 2) */
    else if ( hdr[4] ==  90 && hdr[5] == 120 && hdr[6] ==  31 && hdr[7] ==  61 ) { rt = DPR; }
    /* calibration data record  (tlr file record 3) */
    else if ( hdr[4] ==  90 && hdr[5] == 130 && hdr[6] ==  31 && hdr[7] ==  20 ) { rt = CDR; }
    /* facility related record (tlr file record 4) */
    else if ( hdr[4] ==  90 && hdr[5] == 200 && hdr[6] ==  31 && hdr[7] ==  61 ) { rt = FRR; }

    /* ancient NUL record; type = OLD */
    else if ( hdr[4] ==  32 && hdr[5] == 32 && hdr[6] ==  32 && hdr[7] ==  32 ) { 
      Alert ("rcal_ceos.c: rcal_get_record_type(): OLD ceos record encountered"); 
      printf ("                               \n");
      printf ("hdr label 1 = %d\n", (int)(hdr[4]));
      printf ("hdr label 2 = %d\n", (int)(hdr[5]));
      printf ("hdr label 3 = %d\n", (int)(hdr[6]));
      printf ("hdr label 4 = %d\n", (int)(hdr[7]));
      printf ("                               \n");
      rt = OLD; 
    }

    /* None of the above */
    else { 
      Alert ("rcal_ceos.c: rcal_get_record_type(): unknown ceos record encountered"); 
      printf ("                               \n");
      printf ("hdr label 1 = %d\n", (int)(hdr[4]));
      printf ("hdr label 2 = %d\n", (int)(hdr[5]));
      printf ("hdr label 3 = %d\n", (int)(hdr[6]));
      printf ("hdr label 4 = %d\n", (int)(hdr[7]));
      printf ("                               \n");
      rt = NUL;
    }

    return rt;
}

/*
 * the following two functions are basically paranoid versions of 
 * atof() and atoi().  They use internal buffers to accomplish two
 * things:  
 *   1)  they make sure there is a null character at the end of the
 *       passed char array.
 *   2)  in writing that null character into a copy of the passed char array, they don't 
 *       write into that array itself.
 */

static void rcal_get_integer_value (b, n, v)
char *b;
int n;
int *v;
{
  char a[256];
  int i;
  
  /* copy n chars from passed buffer into a[] vector */
  for (i = 0; i < n; i++) a[i] = *(b+i);

  /* ensure next character is a null */
  a[n] = '\0';

  /* convert to integer type */
  *v = atoi(a);

  return;
}

static void rcal_get_double_value (b, n, v)
char *b;
int n;
double *v;
{
  char a[256];
  int i;
  
  /* copy n chars from passed buffer into a[] vector */
  for (i = 0; i < n; i++) a[i] = *(b+i);

  /* ensure next character is a null */
  a[n] = '\0';

  /* convert to double type */
  *v = atof(a);

  return;
}

static void check(i)
int i;
{
  if (i == FALSE) Exit ("check():  retrieval failed");
}

/*
 * rcal_ceos.c is a collection of ceos interpretation routines.
 *
 * The starting premise is that one has:
 *   (1) a leader file, 'xxx.ldr'
 *   (2) a data file, 'xxx.dat'
 *   (3) a trailer file, 'xxx.tlr'
 *   (4) calibration software
 *
 * Since (4) requires parameters from (1) and (3), the routines in
 *   this file are designed to retrieve them.  The first part of this
 *   file is a whole series of (void) functions, each of which is 
 *   intended to retrieve a particular parameter.  Following these
 *   functions is a more general function called rcal_ceos_get_value()
 *   which each of the above routines actually employs to acquire its
 *   particular parameter.  Following this are some basic 'slave' functions
 *   for rcal_ceos_get_value() and the like.
 * 
 * The reason for this approach is clear.  There is neither time nor inclination 
 * to write a sophisticated CEOS file interpreter.  Furthermore, calibration code
 * which requires a parameter value should have to know nothing more than the (type)
 * of the parameter, the name of the 'get' function to call, and the name of the 
 * file from which it is to be retrieved.  Sometimes an additional indexing value
 * will also be necessary.  Each of these four pieces of information are elaborated
 * in the following four paragraphs.
 *
 * (type) of data:  either INTEGER, DOUBLE, or STRING macro labels are recognized.
 * These are defined in the 'MACHINEFILE' macros file.  The data type is understood
 * to be passed to the get function as a pointer to the appropriate type.  In the 
 * case of a STRING data type, an array of appropriate length should be passed, and
 * the get function will append a null character '\0' to the string.
 *
 * 'get' function names are always of the type 'rcal_get_xxxxxx' where the x's 
 * represent the parameter to be got [sic].  See the documentation for a complete
 * list.
 *
 * This software uses only the part of the passed filename, that which precedes an
 * extension period.  e.g. passing '345100.ldr' to a get function will be interpreted
 * as '345100'.  Since that get function knows where its parameter is supposed to 
 * reside, it will look into the file of the proper extension.  If rcal_get_proc_gain
 * were called passing '345100.ldr', the function knows to look in the file 
 * '345100.tlr' for the processor gain.
 *
 * In some cases, notably vector enumerations, an index passed to the get function
 * will indicate a particular value among a set.  (This rather than writing 256
 * versions of 'rcal_get_radiometric_comp_vector_137()'!)  In other cases, two
 * or more values may be retrieved, as when getting lat-lon values.  Again, see 
 * the software documentation for precise usage.
 * 
 */
 
