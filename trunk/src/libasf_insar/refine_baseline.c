#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"

// Local function declaration
static int get_matrix_rows(char *, char *);

static int bp(char *matfile, char *vecfile, char *oldbase, char *newbase)
{
  int i, j=0;
  int m, n = 4;   /* m and n are rows and columns of matrix A */
  float sum, rms;
  float **A, **Acp, **At;
  float **U, **Ut;
  float **V, **Vt;
  float **S, **St, **Si;
  float *Sv;
  float *b, *bT, *db, *x, *dx, *x0;
  float *Utb, *SiUtb, *VSiUtb;
  float **UUt, **VVt, **VtV, **US, **SVt, **USVt;
  float bperp, dbperp, bpar, dbpar, btemp;
  FILE *fp, *fpNew, *fpOld;

  /*
   * Part I:  Singular Value Decomposition of matrix A
   */
/*  printf("Beginning singular value decomposition of matrix A...\n");*/

  /* determine number of rows 'm' */
  m = get_matrix_rows(matfile,vecfile);

  /* establish matrix A and vector b */
  b = alloc_vector(1, m);
  db = alloc_vector(1, m);
  bT = alloc_vector(1, m);
  x = alloc_vector(1, n);
  dx = alloc_vector(1, n);
  x0 = alloc_vector(1, n);
  A = matrix(1, m, 1, n);
  Acp = matrix(1, m, 1, n);
  At = matrix(1, n, 1, m);

  /* establish decomposition matrices */
  U  = matrix(1, m, 1, n);
  Ut = matrix(1, n, 1, m);
  S  = matrix(1, n, 1, n);
  St = matrix(1, n, 1, n);
  Si = matrix(1, n, 1, n);
  V  = matrix(1, n, 1, n);
  Vt = matrix(1, n, 1, n);

  /* establish product matrices */
  UUt  = matrix(1, n, 1, n);
  VVt  = matrix(1, n, 1, n);
  VtV  = matrix(1, n, 1, n);
  US   = matrix(1, m, 1, n);
  SVt  = matrix(1, n, 1, n);

  /* establish SVD product matrices */
  USVt = matrix(1, m, 1, n);

  /* vector version of diagonal matrix S */
  Sv = alloc_vector(1, m);

  /* vector products */
  Utb = alloc_vector(1, n);
  SiUtb = alloc_vector(1, n);
  VSiUtb = alloc_vector(1, n);

  /* read matrix and vector from input files */
  fp = FOPEN(matfile,"r");
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      fscanf(fp, "%f", &A[i][j]);
    }
  }
  fclose(fp);

  fp = FOPEN(vecfile, "r");
  for (i = 1; i <= m; i++)
    fscanf(fp, "%f", &b[i]);
  fclose(fp);

  /* copy A into Acp */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      Acp[i][j] = A[i][j];
    }
  }

  /* transpose A into At */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      At[j][i] = A[i][j];
    }
  }

  /* NR fn to decompose A = U x S x Vt, where U is written into A */
  svdcmp(A, m, n, Sv, V);

  /* copy Sv into the diagonal of S and St */
  for (i = 1; i <= 4; i++)
    St[i][i] = S[i][i] = Sv[i];

  /* copy A into U where it belongs, copy Acp back into A */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      U[i][j] = A[i][j];
      A[i][j] = Acp[i][j];
    }
  }

  /* establish Ut and Vt */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      Ut[j][i] = U[i][j];
    }
  }

  for (i = 1; i <= n; i++) {
    for (j = 1; j <= n; j++) {
      Vt[j][i] = V[i][j];
    }
  }

  /* check that SVD of A == A */
  matrix_multiply(U, S, US, m, n, n);
  matrix_multiply(US, Vt, USVt, m, n, n);
  for (i = 1; i <= m; i++){
    for (j = 1; j <= n; j++){
      if (fabs(A[i][j] - USVt[i][j]) > 1e-12) {
        /* FIXME: This check needs to be examined and reintroduced */
        /* Exit("   reconstruction of A from SVD failed"); */
      }
    }
  }

  /* invert S into Si, automatically fixing small singular values */
  for (i = 1; i <= n; i++) {
    if (fabs(S[i][i]) < 0.0) {
      Exit("svdcmp() found a negative singular value");
    }
    if (S[i][i] < 1e-6) {
      printf("   singular value %d = %f; auto-set inverse to zero\n", i, S[i][i]);
      Si[i][i] = 0.0;
    }
    else {
      Si[i][i] = 1.0 / S[i][i];
    }
  }

  /* breathe sigh of relief having gotten through SVD */
/*  printf("\nSVD of A is ok\n\n");*/


  /*
   * Part II:  Solve for n-vector x0
   */

  /* multiply matrix Ut x vector b = vector Utb */
  for (i = 1; i <= n; i++) {
    for (j = 1, sum = 0.0; j <= m; j++) {
      sum += Ut[i][j] * b[j];
    }
    Utb[i] = sum;
  }

  /* multiply matrix Si x vector Utb = vector SiUtb */
  for (i = 1; i <= n; i++) {
    SiUtb[i] = Si[i][i] * Utb[i];
  }

  /* multiply matrix V x vector SiUtb = vector VSiUtb */
  for (i = 1; i <= n; i++) {
    for (j = 1, sum = 0.0; j <= n; j++) {
      sum += V[i][j] * SiUtb[j];
    }
    VSiUtb[i] = sum;
  }

  /* copy VSiUtb into x0 */
  for (i = 1; i <= n; i++) {
    x0[i] = VSiUtb[i];
  }

  /* calculate A x x0 */
  for (i = 1; i <= m; i++) {
    for (j = 1, sum = 0.0; j <= n; j++) {
      sum += A[i][j] * x0[j];
    }
    bT[i] = sum;
  }

  /*print_vector(bT, 1, m, "b check, compare with ...");
  print_vector(b, 1, m, "b");
  print_vector(x0, 1, n, "x0");*/

  for (i = 1, sum = 0.0; i <= m; i++) {
    sum += (bT[i] - b[i])*(bT[i] - b[i]);
  }
  rms = sqrt(sum/(float)(m));
  if (!quietflag) printf("   RMS of b-reconstructed and b = %f\n\n", rms);

  /* test for sign of deltas */
  fpOld = FOPEN(oldbase,"r");
  fscanf(fpOld, "%f %f %f %f %f", &bperp, &dbperp, &bpar, &dbpar, &btemp);
  fclose(fpOld);

  printf("   New Baseline:  Normal: %f, delta: %f\n"
         "                  Parallel: %f, delta: %f\n"
         "                  Temporal: %f days\n\n", x0[1], x0[2], x0[3], x0[4], btemp);
  if (logflag) {
    sprintf(logbuf,"   New Baseline:  Normal: %f, delta: %f\n"
                   "                  Parallel: %f, delta: %f\n"
                   "                  Temporal: %f days\n\n", x0[1], x0[2], x0[3], x0[4], btemp);
    printLog(logbuf);
  }

  fpNew = FOPEN(newbase,"w");
  fprintf(fpNew, "%14.7f  %14.7f  %14.7f  %14.7f %14.7f\n",
    x0[1], x0[2], x0[3], x0[4], btemp);
  fclose(fpNew);

  /* free memory */
  free_vector(b,1,m);
  free_vector(db,1,m);
  free_vector(bT,1,m);
  free_vector(x,1,m);
  free_vector(dx,1,m);
  free_vector(x0,1,m);
  free_matrix(A,1,n,1,m);
  free_matrix(Acp,1,n,1,m);
  free_matrix(At,1,n,1,m);
  free_matrix(U,1,m,1,n);
  free_matrix(Ut,1,n,1,m);
  free_matrix(S,1,n,1,n);
  free_matrix(St,1,n,1,n);
  free_matrix(Si,1,n,1,n);
  free_matrix(V,1,n,1,n);
  free_matrix(Vt,1,n,1,n);
  free_matrix(UUt,1,n,1,n);
  free_matrix(VVt,1,n,1,n);
  free_matrix(VtV,1,n,1,n);
  free_matrix(US,1,n,1,n);
  free_matrix(SVt,1,n,1,n);
  free_matrix(USVt,1,m,1,n);
  free_vector(Sv,1,m);
  free_vector(Utb,1,n);
  free_vector(SiUtb,1,n);
  free_vector(VSiUtb,1,n);
  return(0);
}

static int get_matrix_rows(char *f1, char *f2)
{
  int m1, m2;

  if (!fileExist(f1))
     Exit("get_matrix_rows(): %s does not exist",f1);
  if (!fileExist(f2))
     Exit("get_matrix_rows(): %s does not exist",f2);

  m1 = fileNumLines(f1);
  m2 = fileNumLines(f2);

  if (m1 != m2) Exit("get_matrix_rows():  A and b are not same row-dimension");
  if (m1 < 4) Exit("bp:  number of rows less than number of colums");

  return(m1);
}

/* for convenience in a diagnostic */
#define MAC1  (acos((re*re - H*H - Rnear*Rnear) / (-2.0*H*Rnear)))
#define MAC2  (asin(sin(MAC1)*Rnear/re))
#define MAC3  (acos((re*re - H*H - Rcenter*Rcenter) / (-2.0*H*Rcenter)))
#define MAC4  (asin(sin(MAC3)*Rcenter/re))
#define MAC5  (acos((re*re - H*H - Rfar*Rfar) / (-2.0*H*Rfar)))
#define MAC6  (asin(sin(MAC5)*Rfar/re))

static int genab(char *datafile, char *basefile, char *metaName, char *matfile,
		 char *vecfile)
{
  int i, j, m, n;
  double **A,*axB, *b;
  double *uwp, *x, *y, *z;
  baseline base;
  meta_parameters *meta;
  FILE *fp;
  
  /* Get input scene size and windowing info*/
  meta = meta_read(metaName);
  
  /* set program variables*/
  
  m  = fileNumLines(datafile); /* # of rows in matrix A*/
  n  = 4;  /* # of cols in matrix A*/
  
  /* allocate space for vectors and matricies*/
  A = dmatrix(1, m, 1, n);
  axB = dvector(1, m);
  b = dvector(1, m);
  uwp = dvector(1, m);
  x = dvector(1, m);
  y = dvector(1, m);
  z = dvector(1, m);
  
  /* read in seed points & phases from file
     printf("Reading in data from %s...\n",datafile);*/
  fp = FOPEN(datafile, "r");
  for (i = 1; i <= m; i++) {
    fscanf(fp,"%d", &j);
    fscanf(fp,"%lf", &x[i]);
    fscanf(fp,"%lf", &y[i]);
    fscanf(fp,"%lf", &z[i]);
    fscanf(fp,"%lf", &uwp[i]);
  }
  fclose(fp);
  
  /* read in baseline values*/
  base=read_baseline(basefile);
  
  if (!quietflag) 
    printf("   Top left Phase Rate=%f m/radian\n",
	   meta_phase_rate(meta,base,0,0));
  
  /*      printf("\nCalculating terms for matrix and vector...\n\n");*/
  /*Loop over seed points, calculating terms for matrix and vector.*/
  for (i = 1; i <= m; i++) {
    double flat,k;
    /*uwp[i] += meta_flat_phase(meta,base,y[i],x[i]);*/
    
    // Calculate the expected output element b[i].  This is the quantity of 
    // deramping  needed to move the satellite's phase back to the control 
    // point's phase-- that is, it's the satellite phase, compensated for 
    // the control point's height. It's measured in radians.
    b[i]=uwp[i]-z[i]/meta_phase_rate(meta,base,y[i],x[i]);

    /* calculate matrix columns A[i][1-4]
       These terms are multiplied, respectively, by
       the normal baseline, normal baseline change,
       parallel baseline, and parallel baseline change
       to generate the expected output, b[i].
       We use this matrix, later on, to generate a new
       baseline which will hopefully better fit the
       control points.*/
    
    k=meta_get_k(meta);
    flat=meta_flat(meta,y[i],x[i]);
    A[i][1] = -2.0*k*sin(flat);
    A[i][2] = -2.0*k*sin(flat)*meta_scene_frac(meta,y[i]);
    A[i][3] = 2.0*k*cos(flat);
    A[i][4] = 2.0*k*cos(flat)*meta_scene_frac(meta,y[i]);
    
    axB[i] = A[i][1]*base.Bn +
      A[i][2]*base.dBn +
      A[i][3]*base.Bp +
      A[i][4]*base.dBp;
  }
  // printf("\nMatrix calculated....\n");
  
  /* write out matrix & vector*/
  // printf("Writing output files...\n");
  fp = FOPEN(matfile,"w");
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      fprintf(fp, "%16.8f  ", A[i][j]);
    }
    fprintf (fp, "\n");
  }
  fclose(fp);
  
  {
    /*Before we output the b vector, we calculate
      and subtract off the average phase difference--
      a constant phase difference is meaningless, and
      will be corrected for in elev anyway.  The baseline
      should not concern itself with a constant phase shift.*/
    double avgDelta=0.0;
    double rmsDelta=0.0;
    for (i=1;i<=m;i++)
      avgDelta+=(axB[i]=axB[i]-b[i]);
    avgDelta/=(double)m;
    for (i=1;i<=m;i++)
      rmsDelta+=(axB[i]-avgDelta)*(axB[i]-avgDelta);
    rmsDelta/=(double)m;
    rmsDelta=sqrt(rmsDelta);
    if (!quietflag) {
      printf("   Average Difference per point = %f radians\n",avgDelta);
      printf("   RMS Difference per point = %f radians\n\n",rmsDelta);
    }
    fp = FOPEN(vecfile, "w");
    for (i = 1; i <= m; i++)
      fprintf (fp, "%16.8f \n", b[i]+avgDelta);
    fclose(fp);
  }
  
  /* free memory & scram*/
  meta_free(meta);
  free_dmatrix(A,1,m,1,n);
  free_dvector(b,1,m);
  free_dvector(uwp,1,m);
  free_dvector(x,1,m);
  free_dvector(y,1,m);
  free_dvector(z,1,m);
  return(0);
}

static int getphase(char *phasein, char *tiept, char *outfile)
{
  char metaName[255];
  FILE *fphase, *fout, *ftie;
  float phase, *phase_line;
  int ii=0;
  int wid, len;
  float x, y, ht;
  meta_parameters *meta;

  strcpy(metaName, phasein);

  /* Get input scene size*/
  meta = meta_read(metaName);
  wid = meta->general->sample_count;
  len = meta->general->line_count;

  /* open input files*/
  ftie = FOPEN(tiept,"r");
  fout = FOPEN(outfile,"w");
  fphase = fopenImage(phasein,"rb");

  /* Allocate memory for line buffer */
  phase_line = (float *) MALLOC(wid*sizeof(float));

  /* get phase for each tie pt. location*/
  while (fscanf(ftie,"%f %f %f\n",&x,&y,&ht) != EOF) {
     double orig_x = x;
     double orig_y = y;
     y -= meta->general->start_line;
     y /= meta->sar->line_increment;
     x -= meta->general->start_sample;
     x /= meta->sar->sample_increment;
     if ((x<0)||(y<0)||(x>=wid)||(y>=len)) {
       printf("   ERROR: Tie point not within image bounds. Discarding.\n");
       continue;
     }
     get_float_line(fphase, meta, y, phase_line);
     phase = phase_line[(int)x];
     if (fabs(phase)<0.0001) {
       printf("   ERROR: Phase equals zero. Discarding-- "
              "Escher couldn't unwrap this tie point..\n");
       continue;
     }
     fprintf(fout,"%4d  %9.2f  %9.2f  %10.3f  %.8f\n",
	     ii,orig_x,orig_y,ht,phase);
     ii++;
  }

  /* Free memory */
  meta_free(meta);

  /* close input files*/
  FCLOSE(fout);
  FCLOSE(ftie);
  FCLOSE(fphase);
  return(0);
}

static int test_base(char *basefile, char *matfile, char *vecfile)
{
  int i, j;
  int m, n = 4;   /* m and n are rows and columns of matrix A */
  double sum, rms, errsum;
  double **A;
  double *b, *x, *Ax;
  FILE *fp;

  /*
   * Part I:  Singular Value Decomposition of matrix A
   */

  /* determine number of rows 'm' */
  if (!fileExist(matfile)) 
    asfPrintError("test_base:  <A> does not exist\n");
  if (!fileExist(basefile)) 
    asfPrintError("test_base:  <x> does not exist\n");
  if (!fileExist(vecfile)) 
    asfPrintError("test_base:  <b> does not exist\n");
  m = fileNumLines(matfile);
  if (fileNumLines(vecfile) != m)
     asfPrintError("test_base: Number of rows in matrix A and vector B do "
		   "not match");

  /* establish matrix A and vector b */
  A   = dmatrix(1, m, 1, n);
  b   = dvector(1, m);
  x   = dvector(1, n);
  Ax  = dvector(1, m);

  /* read data */
  /*  printf("Reading MatrixA from file %s\n",matfile);*/
  fp = FOPEN(matfile, "r");
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      fscanf(fp, "%lf", &A[i][j]);
    }
  }
  fclose(fp);

  /*  printf("Reading baseline from file %s\n",basefile);*/
  fp = FOPEN(basefile, "r");
  for (i = 1; i <= n; i++) { fscanf(fp, "%lf", &x[i]); }
  fclose(fp);

  /*  printf("Reading vectorB from file %s\n",vecfile);*/
  fp = FOPEN(vecfile, "r");
  for (i = 1; i <= m; i++) { fscanf(fp, "%lf", &b[i]); }
  fclose(fp);

  /* multiply A * x = Ax */
  sum    = 0.0;
  errsum = 0.0;
  for (i = 1; i <= m; i++) {
    Ax[i] = 0.0;
    for (j = 1; j <= n; j++) {
      Ax[i] += A[i][j] * x[j];
    }
    //asfPrintStatus("   %3d  b = %9.3f   A * x = %9.3f   diff = %9.3f  \n", 
    //		   i, b[i], Ax[i], b[i] - Ax[i]);
    sum += (b[i] - Ax[i])*(b[i] - Ax[i]);
    errsum += (b[i] - Ax[i]);
  }
  rms = sqrt(sum/(double)(m));
  //asfPrintStatus("\n   rms diff(b, A*x) = %f, avg diff(b, A*x) = %f\n\n", 
  //  rms, errsum/(double)m);

  return(0);
}

int refine_baseline(char *phaseFile, char *seeds, char *oldBase, char *newBase)
{
  int iter=0;
  char cmd[255], ctrlpt_file[255], matrix_file[255], vec_file[255];

  sprintf(ctrlpt_file, "ctrlpts.%d", iter);
  sprintf(matrix_file, "matrix.%d", iter);
  sprintf(vec_file, "vecB.%d", iter);

  // Get phases from unwrapped phase file
  getphase(phaseFile, seeds, ctrlpt_file);

  // Generate matricies from phases & tie points
  genab(ctrlpt_file, oldBase, phaseFile, matrix_file, vec_file);

  // Create baseline and test it */
  bp(matrix_file, vec_file, oldBase, newBase);
  test_base(newBase, matrix_file, vec_file);

  // Clean up
  sprintf(cmd, "rm %s %s %s", ctrlpt_file, matrix_file, vec_file);
  system(cmd);

  return(0);
}
