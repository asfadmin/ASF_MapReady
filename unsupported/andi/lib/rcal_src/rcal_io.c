/* rcal_io.c:  simple i/o routines */

#include LIBINC

/* default input functions
 * These functions are passed a pointer to a type and a default value.  In response to the user entering
 *   a value from the keyboard or hitting return, the function returns the entered value or the passed 
 *   value respectively.
 */
void getInt (a, i) int *a, i;
{
  char buf[100], *bp;
  int c;

  /* fill buf with null characters */
  for (c = 0, bp = buf; c < 100; c++) *bp++ = '\0';

  /* read value into pointer */
  bp = buf;
  if ((c = getchar()) != '\n') {
    while (c != '\n'){
      *(bp++) = c;
      c = getchar();
    }
    *a = atoi(buf);
  }
  else { *a = i; }

  return;
}

void getFloat (a, f)
float *a, f;
{
  char buf[100], *bp;
  int c;

  /* fill buf with null characters */
  for (c = 0, bp = buf; c < 100; c++) *bp++ = '\0';

  /* read value into pointer */
  bp = buf;
  if ((c = getchar()) != '\n') {
    while (c != '\n'){
      *(bp++) = c;
      c = getchar();
    }
    *a = (float)(atof(buf));
  }
  else { *a = f; }

  return;
}

void getDouble(a, d)
double *a, d;
{
  char buf[100], *bp;
  int c;

  /* fill buf with null characters */
  for (c = 0, bp = buf; c < 100; c++) *bp++ = '\0';

  /* read value into pointer */
  bp = buf;
  if ((c = getchar()) != '\n') {
    while (c != '\n'){
      *(bp++) = c;
      c = getchar();
    }
    *a = atof(buf);
  }
  else { *a = d; }

  return;
}

void getString(a, s)
char *a, *s;
{
  char buf[100], *bp;
  int c;

  /* fill buf with null characters */
  for (c = 0, bp = buf; c < 100; c++) *bp++ = '\0';

  /* read value into pointer */
  bp = buf;
  if ((c = getchar()) != '\n') {
    while (c != '\n'){
      *(bp++) = c;
      c = getchar();
    }
    strcpy (a, buf);
  }
  else  { strcpy(a, s); }

  return;
}

/* diagnostic routine:  print an (m across by n down) matrix 'a' to stdout */
void printMatrix(a, m, n, type, cmnt)
void *a;
int    m; /* m columns */
int    n; /* n rows */
data_t type; /* specifies data type; see sarmacros.h and switch below */
char   *cmnt;
{
  int      i, j;
  int      iv, iv1;
  double   dv, dv1;
  Complex  cv, *cvp;
  DComplex dcv, *dcvp;
  SIComplex sicv, *sicvp;

  i = checkDataType (type);

  switch (type) {

    case CHAR:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          iv = (int)( *((char *)(a) + j*m + i) );
          printf ("%3d  ", iv);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case UCHAR:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          iv = (int)( *((uchar *)(a) + j*m + i) );
          printf ("%3d  ", iv);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case INT:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          iv = (int)( *((int *)(a) + j*m + i) );
          printf ("%5d  ", iv);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case SHORT_INT:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          iv = (int)( *((short int *)(a) + j*m + i) );
          printf ("%5d  ", iv);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case FLOAT:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          dv = (double)( *((float *)(a) + j*m + i) );
          printf ("%8.4lf   ", dv);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case DOUBLE:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          dv = (double)( *((double *)(a) + j*m + i) );
          printf ("%5.1lf   ", dv);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case COMPLEX:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      cvp = (Complex *)(a);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          cv  = *(cvp + j*m + i);
          dv  = (double)(cv.real);
          dv1 = (double)(cv.imag);
          printf ("(%5.1lf, %5.1lf)   ", dv, dv1);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case SHORT_INT_COMPLEX:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      sicvp = (SIComplex *)(a);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          sicv = *(sicvp + j*m + i);
          iv   = sicv.real;
          iv1  = sicv.imag;
          printf ("(%4d, %4d)   ", iv, iv1);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    case DOUBLE_COMPLEX:
      printf ("\n");
      printf ("diag mx printout: %s\n", cmnt);
      dcvp = (DComplex *)(a);
      for (j = 0; j < n; j++){
        printf ("\n");
        for (i = 0; i < m; i++) {
          dcv  = *(dcvp + j*m + i);
          dv   = dcv.real;
          dv1  = dcv.imag;
          printf ("(%5.1lf, %5.1lf)   ", dv, dv1);
        }
      }
      printf ("\n");
      printf ("\n");
      break;

    default:
      Exit ("rcal_io.c:  printMatrix():  bad passed data type");
      break;
  }

  return;

}


/* 
 * returns number of records of length rl in file; 
 * partial records not counted 
 */ 
int getFileLength (fd, rl)
int fd;  /* file descriptor; file assumed open */
int rl;  /* record length in bytes             */
{
  long int  count, i, sum;
  bool      ok;
  char      *b;

  count = sum = i = 0;
  ok              = FALSE;
  b = (char *)(malloc(rl));
  lseek (fd, 0L, 0);

  do {

    sum += (long)(rl);
    i   += read (fd, b, rl);

    /*  why doesn't this work:  i    = lseek (fd, rl, 1); ???  */

    ok   = (sum == i) ? TRUE : FALSE;
    if (ok) count++;

  } while (ok);

  free (b);
  lseek (fd, 0L, 0);
  return count;
}

/* i/o routine:  read an (m across by n down) matrix 'a' from a file to a buffer 
 * 
 * routine notes:
 *   all files assumed binary.
 *   all files assumed single value per sample or 2 values per sample
 *     in all *COMPLEX* cases.
 *   h0 is an initial number of 'skip' bytes (e.g. ACS/CEOS header bytes) at the
 *     top of the file.
 *   h1 is an initial number of 'skip' bytes at the beginning of each 
 *     record.
 *   Size is the size, in bytes, of a single sample.
 *     e.g.1:  a uchar array with 10 elements per line will have Size = 1
 *     e.g.2:  a DOUBLE_COMPLEX array (8 bytes per double) with 15 elements 
 *             per line will have Size = 16
 */
void readMatrix(fnm, a, type, m, n, c0, r0, M, N, h0, h1)
char   *fnm;      /* source filename                   */
void   *a;        /* destination buffer                */
data_t type;      /* data type; see switch below       */
int    m, n;      /* m = columns, n = rows             */
int    c0, r0;    /* c0 = first column, r0 = first row */ 
int    M, N;      /* M = columns, N = rows of source   */
int    h0, h1;    /* source header sizes (see below)   */
{
  long int  ls;
  int       fd;
  int       i, j, k;
  int       dsize;
  int       tail, ind, should_be_at;
  int       VPS, Size;
            /* VPS is values per sample; 1 for real, 2 for complex samples
             * Size is the length in bytes of 1 sample, as above
             */

  /* Jason mod -- diagnostic print routine */
  printf("readMatrix: Going to read from file %s.\n", fnm);
  printf("	type code : %d\n", type);
  printf("	columns to read : %d\n", m);
  printf("	rows to read : %d\n", n);
  printf("	1st column : %d\n", c0);
  printf("	1st row : %d\n", r0);
  printf("	image columns : %d\n", M);
  printf("	image rows : %d\n", N);
  printf("	TOF header size : %d (bytes)\n", h0);
  printf("	line header size : %d (bytes)\n", h1);

  /* bless all passed values */
  dsize = checkDataType(type);
  if (     m < 1) Exit ("rcal_io.c:  readMatrix():  number of matrix columns < 1");
  if (     n < 1) Exit ("rcal_io.c:  readMatrix():  number of matrix rows < 1");
  if (    c0 < 0) Exit ("rcal_io.c:  readMatrix():  first column < 0");
  if (    r0 < 0) Exit ("rcal_io.c:  readMatrix():  first row < 0");
  if (     M < 1) Exit ("rcal_io.c:  readMatrix():  number of source columns < 1");
  if (     N < 1) Exit ("rcal_io.c:  readMatrix():  number of source rows < 1");
  if (    h0 < 0) Exit ("rcal_io.c:  readMatrix():  top header length < 0");
  if (    h1 < 0) Exit ("rcal_io.c:  readMatrix():  record header length < 0");
  if (m + c0 > M) Exit ("rcal_io.c:  readMatrix():  columns exceed given file size");
  if (n + r0 > N) Exit ("rcal_io.c:  readMatrix():  rows exceed given file size");

  /* open file */
  fd = open (fnm, 0);
  if (fd < 3) Exit ("rcal_io.c:  readMatrix:  open(fnm, 0) ret < 3");

  /* lseek past 'h0' header bytes; file is now set to first line of data */
  ls = lseek (fd, (long)(h0), 0);


  switch (type) {

    /* signed bytes */
    case CHAR:              
      VPS  = 1;
      Size = VPS*sizeof(char);        
      break;

    /* unsigned bytes */
    case UCHAR:             
      VPS  = 1;
      Size = VPS*sizeof(uchar);       
      break;

    /* short integers */
    case SHORT_INT:         
      VPS  = 1;
      Size = VPS*sizeof(short int);   
      break;

    /* integers */
    case INT:               
      VPS  = 1;
      Size = VPS*sizeof(int);         
      break;

    /* floating point single precision */
    case FLOAT:             
      VPS  = 1;
      Size = VPS*sizeof(float);       
      break;

    /* double precision floating point */
    case DOUBLE:            
      VPS  = 1;
      Size = VPS*sizeof(double);      
      break;

    /* short int complex (ACS format) */
    case SHORT_INT_COMPLEX: 
      VPS  = 2;
      Size = VPS*sizeof(short int); 
      break;

    /* int complex */
    case INT_COMPLEX:       
      VPS  = 2;
      Size = VPS*sizeof(int);       
      break;

    /* same as float complex */
    case COMPLEX:           
      VPS  = 2;
      Size = VPS*sizeof(float);     
      break;

    /* double precision floating point complex */
    case DOUBLE_COMPLEX:    
      VPS  = 2;
      Size = VPS*sizeof(double);    
      break;

    default:
      Exit ("rcal_io.c:  readMatrix():  bad passed data type"); 
      break;

  }

  /* lseek to row r0 */
  j = r0*(h1 + M*Size);
  ls = lseek (fd, (long)(j), 1);

  /* calculate 'tail', number of bytes from end of matrix line to end of record */
  tail = Size*(M - (c0 + m));

  /* read in matrix to buffer */
  for (j = 0; j < n; j++){

    /* lseek over header h1 */
    ls = lseek (fd, (long)(h1), 1);

    /* lseek to column c0 */
    k  = c0*Size;
    ls = lseek (fd, (long)(k), 1);

    /* calculate start index for matrix */
    ind = j*m;

    /* read data directly to buffer *(a+ind) */
    i = read (fd, (char *)(a) + Size*ind, Size*m);

    /* lseek to end of line */
    ls = lseek (fd, (long)(tail), 1);

  }

  close (fd);
  return;

}

/* writeVector():  write an n element vector to a binary file */
void writeVector (v, fnm, type, n)
void   *v;        /* source vector          */
char   *fnm;      /* destination filename   */
data_t type;      /* data type              */
int    n;         /* n = number of elements */
{
  int       fd, i, Size;

  fd = creat (fnm, 0777);
  if (fd < 3) Exit ("rcal_io.c:  writeVector:  creat returned < 3");
  Size = n*checkDataType(type);
  i    = write (fd, (char *)(v), Size);
  close(fd);
  return;
}

/* appendVector():  append an n element vector to a binary file */
void appendVector (v, fnm, type, n)
void   *v;        /* source vector           */
char   *fnm;      /* destination filename    */
data_t type;      /* data type               */
int    n;         /* n = number of elements  */
{
  int       fd, i, Size;

  if ((fd = open (fnm, 1)) < 3) {
    close (fd);
    fd = creat(fnm, 0777);
  }
  if (fd < 3) Exit ("rcal_io.c:  appendVector:  open/creat returned < 3");
  i     = lseek(fd, 0, 2);
  Size  = n*checkDataType(type);
  i     = write(fd, (char *)(v), Size);
  close(fd);
  return;
}

int checkDataType(type)
data_t type;
{
  if (type == STRING            ) return sizeof(char);
  if (type == INT               ) return sizeof(int);
  if (type == INTEGER           ) return sizeof(int);
  if (type == SHORT_INT         ) return sizeof(short int);
  if (type == FLOAT             ) return sizeof(float);
  if (type == DOUBLE            ) return sizeof(double);
  if (type == CHAR              ) return sizeof(char);
  if (type == UCHAR             ) return sizeof(uchar);
  if (type == COMPLEX           ) return sizeof(Complex);
  if (type == FLOAT_COMPLEX     ) return sizeof(Complex);
  if (type == DOUBLE_COMPLEX    ) return sizeof(DComplex);
  if (type == SHORT_INT_COMPLEX ) return sizeof(SIComplex);
  if (type == INT_COMPLEX       ) return sizeof(IComplex);

  Exit ("rcal_io.c:  checkDataType():  unknown...");
}

/* Jason mod -- modified this file to only create/append existing log file.
 * This will provide a history for more than the last disa session.
 */
void rlogfileInit(f, a)
char *f, *a;
{
  FILE *fp;
  fp = fopen (f, "a");
  fclose (fp);
  rlogfileMsg(f, a);
  return;
}

void rlogfileMsg(f, a)
char *f, *a;
{
  FILE *fp;
  fp = fopen (f, "a");
  fseek(fp, 0, SEEK_END);
  fprintf (fp, "%s\n", a);
  fclose (fp);
  return;
}

void rlogfileBlankline(f)
char *f;
{
  FILE *fp;
  fp = fopen (f, "a");
  fseek(fp, 0, SEEK_END);
  fprintf (fp, "\n");
  fclose (fp);
  return;
}

int rIndex(i, j, n) int i, j, n; { return (i + j*n); }

/* wait n seconds */
void rWait(n)
int n;
{
  clock_t startTime, myTime, waitTime;
  bool    timeElapsed;

  if (n <= 0) return;
  waitTime    = (clock_t)(n);
  startTime   = clock()/MY_CLK_TCK;
  timeElapsed = FALSE;

  while (!timeElapsed) {
    myTime = clock()/MY_CLK_TCK;
    if (myTime - startTime > waitTime) timeElapsed = TRUE;
  }
  
  return;
}


void copyVector(v1, v2, type1, type2, n)
void   *v1;
void   *v2;
data_t type1;
data_t type2;
int    n;
{
  int       i;
  double    re, im;
  char      *c1, *c2, cval;
  uchar     *u1, *u2, uval;
  int       *i1, *i2, ival;
  short int *s1, *s2, sval;
  float     *f1, *f2, fval;
  double    *d1, *d2, dval;
  SIComplex *S1, *S2, Sval;
  IComplex  *I1, *I2, Ival;
  Complex   *C1, *C2, Cval;
  DComplex  *D1, *D2, Dval;

  /* Jason mod -- diag */
  printf("copyVector copying from type %d to type %d, length %d\n", type1, type2, n);
  i = checkDataType(type1);
  i = checkDataType(type2);

  switch (type1) {

    case STRING:
    
      c1 = (char *)(v1);

      switch (type2) {

        case STRING:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++)
            *c2++ = *c1++;
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++)
            *u2++ = (uchar)(*c1++);
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++)
            *c2++ = *c1++;
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = (short int)(*c1++);
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = (int)(*c1++);
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = (float)(*c1++);
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = (double)(*c1++);
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(*c1);
            S2->imag = 0;
            S2++;
            c1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*c1);
            I2->imag = 0;
            I2++;
            c1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(*c1);
            C2->imag = 0.0;
            C2++;
            c1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(*c1);
            D2->imag = 0.0;
            D2++;
            c1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case UCHAR:

      u1 = (uchar *)(v1);

      switch (type2) {

        case STRING:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++)
            *c2++ = *u1++;
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++)
            *u2++ = *u1++;
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++)
            *c2++ = (char)(*u1++);
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = (short int)(*u1++);
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = (int)(*u1++);
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = (float)(*u1++);
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = (double)(*u1++);
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(*u1);
            S2->imag = 0;
            S2++;
            u1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*u1);
            I2->imag = 0;
            I2++;
            u1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(*u1);
            C2->imag = 0.0;
            C2++;
            u1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(*u1);
            D2->imag = 0.0;
            D2++;
            u1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case CHAR:

      c1 = (char *)(v1);

      switch (type2) {

        case STRING:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++)
            *c2++ = *c1++;
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++)
            *u2++ = (uchar)(*c1++);
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++)
            *c2++ = *c1++;
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = (short int)(*c1++);
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = (int)(*c1++);
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = (float)(*c1++);
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = (double)(*c1++);
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(*c1);
            S2->imag = 0;
            S2++;
            c1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*c1);
            I2->imag = 0;
            I2++;
            c1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(*c1);
            C2->imag = 0.0;
            C2++;
            c1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(*c1);
            D2->imag = 0.0;
            D2++;
            c1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case SHORT_INT:

      s1 = (short int *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do short int -> string");
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++){
            sval = min(*s1, 255);
            sval = max(0, sval);
            *u2++ = (uchar)(sval);
            s1++;
          }
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++){
            sval = min(*s1, 127);
            sval = max(-128, sval);
            *c2++ = (char)(sval);
            s1++;
          }
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = *s1++;
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = (int)(*s1++);
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = (float)(*s1++);
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = (double)(*s1++);
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = *s1;
            S2->imag = 0;
            S2++;
            s1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*s1);
            I2->imag = 0;
            I2++;
            s1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(*s1);
            C2->imag = 0.0;
            C2++;
            s1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(*s1);
            D2->imag = 0.0;
            D2++;
            s1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case INTEGER:

      i1 = (int *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do int -> string");
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++){
            ival = min(*i1, 255);
            ival = max(0, ival);
            *u2++ = (uchar)(ival);
            i1++;
          }
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++){
            ival = min(*i1, 127);
            ival = max(-128, ival);
            *c2++ = (char)(ival);
            i1++;
          }
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = (short int)(*i1++);
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = *i1++;
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = (float)(*i1++);
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = (double)(*i1++);
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(*i1);
            S2->imag = 0;
            S2++;
            i1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*i1);
            I2->imag = 0;
            I2++;
            i1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(*i1);
            C2->imag = 0.0;
            C2++;
            i1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(*i1);
            D2->imag = 0.0;
            D2++;
            i1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case FLOAT:

      f1 = (float *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do float -> string");
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++){
            fval = min(*f1, 255.0);
            fval = max(0.0, fval);
            *u2++ = (uchar)(fval);
            f1++;
          }
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++){
            fval = min(*f1, 127.0);
            fval = max(-128.0, fval);
            *c2++ = (char)(fval);
            f1++;
          }
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = (short int)(*f1++);
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = (int)(*f1++);
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = *f1++;
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = (double)(*f1++);
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(*f1);
            S2->imag = 0;
            S2++;
            f1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*f1);
            I2->imag = 0;
            I2++;
            f1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = *f1;
            C2->imag = 0.0;
            C2++;
            f1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(*f1);
            D2->imag = 0.0;
            D2++;
            f1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case DOUBLE:

      d1 = (double *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do double -> string");
          break;

        case UCHAR:
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++){
            dval = min(*d1, 255.0);
            dval = max(0.0, dval);
            *u2++ = (uchar)(dval);
            d1++;
          }
          break;

        case CHAR:
          c2 = (char *)(v2);
          for (i = 0; i < n; i++){
            dval = min(*d1++, 127.0);
            dval = max(-128.0, dval);
            *c2++ = (char)(dval);
          }
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++)
            *s2++ = (short int)(*d1++);
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++)
            *i2++ = (int)(*d1++);
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++)
            *f2++ = (float)(*d1++);
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++)
            *d2++ = *d1++;
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(*d1);
            S2->imag = 0;
            S2++;
            d1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(*d1);
            I2->imag = 0;
            I2++;
            d1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(*d1);
            C2->imag = 0.0;
            C2++;
            d1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = *d1;
            D2->imag = 0.0;
            D2++;
            d1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case SHORT_INT_COMPLEX:

      S1 = (SIComplex *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do SIComplex -> string");
          break;

        case UCHAR:
	  /* Jason mod -- added code to do this for magnitude */
          u2 = (uchar *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(S1->real);
            im   = (double)(S1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            dval = min(dval,  32767.0);
            dval = max(dval,      0.0);
            *u2  = (uchar) (dval/128);
            u2++;
            S1++;
          }
          break;

        case CHAR:
          Exit ("copyVector():  can't do SIComplex -> char");
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(S1->real); 
            im   = (double)(S1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            dval = min(dval,  32767.0);
            dval = max(dval, -32768.0);
            *s2  = (short int)(dval);
            s2++;
            S1++;
          }
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(S1->real); 
            im   = (double)(S1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            *i2  = (int)(dval);
            i2++;
            S1++;
          }
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(S1->real); 
            im   = (double)(S1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            *f2  = (float)(dval);
            f2++;
            S1++;
          }
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(S1->real); 
            im   = (double)(S1->imag);
            dval = re*re + im*im;
            *d2  = (dval == 0.0) ? 0.0 : sqrt(dval);
            d2++;
            S1++;
          }
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++)
            *S2++ = *S1++;
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(S1->real);
            I2->imag = (int)(S1->imag);
            I2++;
            S1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(S1->real);
            C2->imag = (float)(S1->imag);
            C2++;
            S1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(S1->real);
            D2->imag = (double)(S1->imag);
            D2++;
            S1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case INT_COMPLEX:

      I1 = (IComplex *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do IComplex -> string");
          break;

        case UCHAR:
          Exit ("copyVector():  can't do IComplex -> uchar");
          break;

        case CHAR:
          Exit ("copyVector():  can't do IComplex -> char");
          break;

        case SHORT_INT:
          s2 = (short int *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(I1->real); 
            im   = (double)(I1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            dval = min(dval,  32767.0);
            dval = max(dval, -32768.0);
            *s2  = (short int)(dval);
            s2++;
            I1++;
          }
          break;

        case INTEGER:
          i2 = (int *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(I1->real); 
            im   = (double)(I1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            *i2  = (int)(dval);
            i2++;
            I1++;
          }
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(I1->real); 
            im   = (double)(I1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            *f2  = (float)(dval);
            f2++;
            I1++;
          }
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(I1->real); 
            im   = (double)(I1->imag);
            dval = re*re + im*im;
            *d2  = (dval == 0.0) ? 0.0 : sqrt(dval);
            d2++;
            I1++;
          }
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(I1->real);
            S2->imag = (short int)(I1->imag);
            S2++;
            I1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++)
            *I2++ = *I1++;
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(I1->real);
            C2->imag = (float)(I1->imag);
            C2++;
            I1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(I1->real);
            D2->imag = (double)(I1->imag);
            D2++;
            I1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case FLOAT_COMPLEX:

      C1 = (Complex *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do Complex -> string");
          break;

        case UCHAR:
          Exit ("copyVector():  can't do Complex -> uchar");
          break;

        case CHAR:
          Exit ("copyVector():  can't do Complex -> char");
          break;

        case SHORT_INT:
          Exit ("copyVector():  can't do Complex -> short int");
          break;

        case INTEGER:
          Exit ("copyVector():  can't do Complex -> int");
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(C1->real); 
            im   = (double)(C1->imag);
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            *f2  = (float)(dval);
            f2++;
            C1++;
          }
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++) {
            re   = (double)(C1->real); 
            im   = (double)(C1->imag);
            dval = re*re + im*im;
            *d2  = (dval == 0.0) ? 0.0 : sqrt(dval);
            d2++;
            C1++;
          }
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(C1->real);
            S2->imag = (short int)(C1->imag);
            S2++;
            C1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(C1->real);
            I2->imag = (int)(C1->imag);
            I2++;
            C1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++)
            *C2++ = *C1++;
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++) {
            D2->real = (double)(C1->real);
            D2->imag = (double)(C1->imag);
            D2++;
            C1++;
          }
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    case DOUBLE_COMPLEX:

      D1 = (DComplex *)(v1);

      switch (type2) {

        case STRING:
          Exit ("copyVector():  can't do DComplex -> string");
          break;

        case UCHAR:
          Exit ("copyVector():  can't do DComplex -> uchar");
          break;

        case CHAR:
          Exit ("copyVector():  can't do DComplex -> char");
          break;

        case SHORT_INT:
          Exit ("copyVector():  can't do DComplex -> short int");
          break;

        case INTEGER:
          Exit ("copyVector():  can't do DComplex -> int");
          break;

        case FLOAT:
          f2 = (float *)(v2);
          for (i = 0; i < n; i++) {
            re   = D1->real; 
            im   = D1->imag;
            dval = re*re + im*im;
            dval = (dval == 0.0) ? 0.0 : sqrt(dval);
            *f2  = (float)(dval);
            f2++;
            D1++;
          }
          break;

        case DOUBLE:
          d2 = (double *)(v2);
          for (i = 0; i < n; i++) {
            re   = D1->real; 
            im   = D1->imag;
            dval = re*re + im*im;
            *d2  = (dval == 0.0) ? 0.0 : sqrt(dval);
            d2++;
            D1++;
          }
          break;

        case SHORT_INT_COMPLEX:
          S2 = (SIComplex *)(v2);
          for (i = 0; i < n; i++) {
            S2->real = (short int)(D1->real);
            S2->imag = (short int)(D1->imag);
            S2++;
            D1++;
          }
          break;

        case INT_COMPLEX:
          I2 = (IComplex *)(v2);
          for (i = 0; i < n; i++) {
            I2->real = (int)(D1->real);
            I2->imag = (int)(D1->imag);
            I2++;
            D1++;
          }
          break;

        case FLOAT_COMPLEX:
          C2 = (Complex *)(v2);
          for (i = 0; i < n; i++) {
            C2->real = (float)(D1->real);
            C2->imag = (float)(D1->imag);
            C2++;
            D1++;
          }
          break;

        case DOUBLE_COMPLEX:
          D2 = (DComplex *)(v2);
          for (i = 0; i < n; i++)
            *D2++ = *D1++;
          break;

        default:
          Exit("copyVector:  type2 switch default fall-through");
          break;

      }

      break;

    default:
      Exit("copyVector:  type1 switch default fall-through");
      break;

  }
  
  printf("copyVector: returning now...\n");
  return;
}
