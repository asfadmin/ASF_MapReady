#include LIBINC

/* 
 * this function uses idl to make a hardcopy plot of vector
 * 1 (x axis) versus vector 2 (y axis) 
 */
void idlXYPlotHard(n, v1, type1, v2, type2, gT, xT, yT)
int    n;
void   *v1, *v2;
data_t type1, type2;
char   *gT, *xT, *yT;
{
  int    i;
  char   idlCommandString[256], idlFunction[256]; 
  char   idlLog[256], idlPathString[256];
  float  *myV1, *myV2;
  FILE   *fp;

  checkDataType (type1);
  checkDataType (type2);
  if (n <= 0) return;

  myV1 = (float *)(malloc(n*sizeof(float)));
  myV2 = (float *)(malloc(n*sizeof(float)));

  fp = fopen ("xyplot.tmp.dat", "w");
  fprintf (fp, "%d\n", n);
  fprintf (fp, "%s\n", gT);
  fprintf (fp, "%s\n", xT);
  fprintf (fp, "%s\n", yT);

  copyVector(v1, myV1, type1, FLOAT, n);
  copyVector(v2, myV2, type2, FLOAT, n);

  for (i = 0; i < n; i++) 
    fprintf (fp, "%f      %f\n", *(myV1+i), *(myV2+i));

  fclose (fp);

  /* path to IDL functions and log files */
  strcpy (idlPathString, IDL_SOURCE_PATH);
  new_fnm_extension (idlPathString, idlFunction,  "xyPlotHard.idl");
  strcpy (idlLog, "Plot.Pvlog.tmp");
  new_fnm_extension ("@", idlCommandString, idlFunction);
  waveinit(idlLog);
  wavecmd (idlCommandString); 
  printf ("waiting 5 seconds, then stopping IDL\n");
  rWait (5);
  waveterm();
  printf ("IDL done\n");
  free (myV1);
  free (myV2);
  return;
}




/* fn uses IDL to display a 2-D scalar-valued (m x n) field */
void idlXYZSurface(m, n, a, type)
int    m, n;
void   *a;
data_t type;
{
  int    i, size;
  char   idlCommandString[256], idlFunction[256]; 
  char   idlLog[256], idlPathString[256];
  float  *myArr;
  FILE   *fp;

  i = checkDataType (type);
  if (n <= 0 || m <= 0) return;
  size = m*n;

  myArr = (float *)(malloc(size*sizeof(float)));

  fp = fopen ("xyzsurface.tmp.dat", "w");
  fprintf (fp, "%d\n", n);
  fprintf (fp, "%d\n", m);

  copyVector(a, myArr, type, FLOAT, size);

  for (i = 0; i < size; i++) 
    fprintf (fp, "%f\n", *(myArr+i));

  fclose (fp);

  /* path to idl functions and log files */
  strcpy (idlPathString, IDL_SOURCE_PATH);
  new_fnm_extension (idlPathString, idlFunction,  "xyzSurface.idl");
  strcpy (idlLog, "Surface.Pvlog.tmp");
  new_fnm_extension ("@", idlCommandString, idlFunction);
  waveinit(idlLog);
  wavecmd (idlCommandString); 
  printf ("waiting 5 seconds, then stopping IDL \n");
  rWait (5);
  waveterm();
  printf ("IDL done\n");
  free (myArr);
  return;
}

