#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "snaphu.h"


/* function: IsTrue()
 * ------------------
 * Returns TRUE if the string input is any of TRUE, true, 1, y, Y, yes, YES
 */
int IsTrue(char *str){

  if(!strcmp(str,"TRUE") || !strcmp(str,"true") 
     || !strcmp(str,"1") || !strcmp(str,"y") || !strcmp(str,"Y")
     || !strcmp(str,"yes") || !strcmp(str,"YES") || !strcmp(str,"Yes")){
    return(TRUE);
  }else{
    return(FALSE);
  }
}


/* function: IsFalse()
 * ------------------
 * Returns FALSE if the string input is any of FALSE, false, 0, n, N, no, NO
 */
int IsFalse(char *str){

  if(!strcmp(str,"FALSE") || !strcmp(str,"false") 
     || !strcmp(str,"0") || !strcmp(str,"n") || !strcmp(str,"N")
     || !strcmp(str,"no") || !strcmp(str,"NO") || !strcmp(str,"No")){
    return(TRUE);
  }else{
    return(FALSE);
  }
}


/* function: ModDiff()
 * -------------------
 * Computes floating point difference between two numbers.
 * f1 and f2 should be between [0,2pi).  The result is the 
 * modulo difference between (-pi,pi].  Assumes that
 * PI and TWOPI have been defined.  
 */  
float ModDiff(float f1, float f2){

  float f3;

  f3=f1-f2;
  if(f3>PI){
    f3-=TWOPI;
  }else if(f3<=-PI){
    f3+=TWOPI;
  }
  return(f3);
}

/* function: WrapPhase()
 * ---------------------
 * Makes sure the passed float array is properly wrapped into the [0,2pi)
 * interval.
 */
void WrapPhase(float **wrappedphase, long nrow, long ncol){

  long row, col;

  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      wrappedphase[row][col]-=TWOPI*floor(wrappedphase[row][col]/TWOPI);
    }
  }
}

/* function: CycleResidue()
 * ------------------------
 * Computes the cycle array of a phase 2D phase array. Input arrays 
 * should be type float ** and signed char ** with memory pre-allocated.  
 * Numbers of rows and columns in phase array should be passed.  
 * Residue array will then have size nrow-1 x ncol-1.  Residues will
 * always be -1, 0, or 1 if wrapped phase is passed in.
 */
void CycleResidue(float **phase, signed char **residue, 
		  int nrow, int ncol){

  int row, col;
  float **rowdiff, **coldiff;

  if((rowdiff=(float **)Get2DMem(nrow-1,ncol,sizeof(float *),sizeof(float)))==
     NULL){
    exit(ABNORMAL_EXIT);
  }
  if((coldiff=(float **)Get2DMem(nrow,ncol-1,sizeof(float *),sizeof(float)))==
     NULL){
    exit(ABNORMAL_EXIT);
  }

  for(row=0;row<nrow-1;row++){
    for(col=0;col<ncol;col++){
      rowdiff[row][col]=ModDiff(phase[row+1][col],phase[row][col]);
    }
  }
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol-1;col++){
      coldiff[row][col]=ModDiff(phase[row][col+1],phase[row][col]);
    }
  }

  for(row=0;row<nrow-1;row++){
    for(col=0;col<ncol-1;col++){
      residue[row][col]=(signed char)FRound((coldiff[row][col]
					     +rowdiff[row][col+1]
					     -coldiff[row+1][col]
					     -rowdiff[row][col])/TWOPI);
    }
  }

  Free2DArray((void **)rowdiff,nrow-1);
  Free2DArray((void **)coldiff,nrow);

}


/* function: CalcFlow()
 * --------------------
 * Calculates flow based on unwrapped phase data in a 2D array.  
 * Allocates memory for row and column flow arrays.
 */
void CalcFlow(float **phase, short ***flowsptr, long nrow, long ncol){

  long row, col;

  /* get memory for flow arrays */
  (*flowsptr)=(short **)Get2DRowColMem(nrow,ncol,
				       sizeof(short *),sizeof(short));

  /* get row flows (vertical phase differences) */
  for(row=0;row<nrow-1;row++){
    for(col=0;col<ncol;col++){
      (*flowsptr)[row][col]=(short)FRound((phase[row][col]-phase[row+1][col])
					 /TWOPI);
    }
  }
  
  /* get col flows (horizontal phase differences) */
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol-1;col++){
      (*flowsptr)[nrow-1+row][col]=(short)FRound((phase[row][col+1]
						  -phase[row][col])
						 /TWOPI);
    }
  }
}


/* function: IntegratePhase()
 * --------------------------
 * This function takes row and column flow information and integrates
 * wrapped phase to create an unwrapped phase field.  The unwrapped
 * phase field will be the same size as the wrapped field.  The array
 * rowflow should have size N-1xM and colflow size NxM-1 where the 
 * phase fields are NxM.  Output is saved to a file.
 */
void IntegratePhase(float **psi, float **phi, short **flows,
		    long nrow, long ncol){

  long row, col;
  short **rowflow, **colflow;
  rowflow=flows;
  colflow=&(flows[nrow-1]);
 
  /* set first element as seed */
  phi[0][0]=psi[0][0];

  if(COLWISEINT){
  
    /* integrate over first row */
    for(col=1;col<ncol;col++){
      phi[0][col]=phi[0][col-1]+ModDiff(psi[0][col],psi[0][col-1])
          +colflow[0][col-1]*TWOPI;
    }

    /* integrate over columns */
    for(row=1;row<nrow;row++){
      for(col=0;col<ncol;col++){
	phi[row][col]=phi[row-1][col]+ModDiff(psi[row][col],psi[row-1][col])
            -rowflow[row-1][col]*TWOPI;
      }
    }
  }else{
    for(row=1;row<nrow;row++){
      phi[row][0]=phi[row-1][0]+ModDiff(psi[row][0],psi[row-1][0])
          -rowflow[row-1][0]*TWOPI;
    }
    for(col=1;col<ncol;col++){
      for(row=0;row<nrow;row++){
	phi[row][col]=phi[row][col-1]+ModDiff(psi[row][col],psi[row][col-1])
            +colflow[row][col-1]*TWOPI;
      }
    }
  }/* end if(COLWISEINT) */
}/* end IntegratePhase() */


/* function: ExtractFlow()
 * -----------------------
 * Given an unwrapped phase array, parse the data and find the flows.
 * Assumes only integer numbers of cycles have been added to get the 
 * unwrapped phase from the wrapped pase.  Gets memory and writes 
 * wrapped phase to passed pointer.  Assumes flows fit into short ints.
 */
float **ExtractFlow(float **unwrappedphase, short ***flowsptr, 
		    long nrow, long ncol){    

  long row, col;
  float **wrappedphase;
  
  /* get memory for wrapped phase array */
  if((wrappedphase
      =(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))==NULL){
    fprintf(sp0,"Out of Memory. abort\n");
    exit(ABNORMAL_EXIT);
  }

  /* calculate wrapped phase */
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      /* fmod() gives wrong results here (maybe because of float argument?) */
      /* wrappedphase[row][col]=fmod(unwrappedphase[row][col],TWOPI); */
      wrappedphase[row][col]=unwrappedphase[row][col]
	-TWOPI*floor(unwrappedphase[row][col]/TWOPI);
    }
  }

  /* calculate flows */
  CalcFlow(unwrappedphase,flowsptr,nrow,ncol);

  /* return pointer to wrapped phase array */
  return(wrappedphase);

}


/* function: FlipPhaseArraySign()
 * ------------------------------
 * Flips the sign of all values in a passed array if the passed baseline
 * value is greater than zero.  Otherwise, does nothing.
 */
void FlipPhaseArraySign(float **arr, double bperp, 
			long nrow, long ncol){

  long row, col;

  if(bperp>0){
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	arr[row][col]*=-1;
      }
    }
  }

}


/* function: FlipFlowArraySign()
 * ------------------------------
 * Flips the sign of all values in a row-by-column array if the based baseline
 * value is greater than zero.  Otherwise, does nothing.
 */
void FlipFlowArraySign(short **arr, double bperp, long nrow, long ncol){

  long row, col, maxcol;

  if(bperp>0){
    for(row=0;row<2*nrow-1;row++){
      if(row<nrow-1){
	maxcol=ncol;
      }else{
	maxcol=ncol-1;
      }
      for(col=0;col<maxcol;col++){
	arr[row][col]=-arr[row][col];
      }
    }
  }

}


/* function: Get2DMem()
 * --------------------
 * Allocates memory for 2D array.
 * Dynamically allocates memory and returns pointer of
 * type void ** for an array of size nrow x ncol.  
 * First index is row number: array[row][col]
 * size is size of array element, psize is size of pointer
 * to array element (eg sizeof(float *)).
 */
void **Get2DMem(int nrow, int ncol, int psize, size_t size){

  int row;
  void **array;

  if((array=malloc(nrow*psize))==NULL){
    fprintf(sp0,"Out of Memory.\n"); 
    exit(ABNORMAL_EXIT);
  }
  for(row=0; row<nrow; row++){
    if((array[row]=malloc(ncol*size))==NULL){
      fprintf(sp0,"Out of Memory.\n");
      exit(ABNORMAL_EXIT);
    }
  }
  return(array);
}


/* function: Get2DRowColMem()
 * --------------------------
 * Allocates memory for 2D array.  The array will have 2*nrow-1 rows.
 * The first nrow-1 rows will have ncol columns, and the rest will
 * have ncol-1 columns.
 */
void **Get2DRowColMem(long nrow, long ncol, int psize, size_t size){

  long row;
  void **array;

  if((array=malloc((2*nrow-1)*psize))==NULL){
    fprintf(sp0,"Out of Memory.\n"); 
    exit(ABNORMAL_EXIT);
  }
  for(row=0; row<nrow-1; row++){
    if((array[row]=malloc(ncol*size))==NULL){
      fprintf(sp0,"Out of Memory.\n");
      exit(ABNORMAL_EXIT);
    }
  }
  for(row=nrow-1; row<2*nrow-1; row++){
    if((array[row]=malloc((ncol-1)*size))==NULL){
      fprintf(sp0,"Out of Memory.\n");
      exit(ABNORMAL_EXIT);
    }
  }
  return(array);
}


/* function: Get2DRowColZeroMem()
 * ------------------------------
 * Allocates memory for 2D array.  The array will have 2*nrow-1 rows.
 * The first nrow-1 rows will have ncol columns, and the rest will
 * have ncol-1 columns.  Memory is initialized to zero.
 */
void **Get2DRowColZeroMem(long nrow, long ncol, int psize, size_t size){

  long row;
  void **array;

  if((array=malloc((2*nrow-1)*psize))==NULL){
    fprintf(sp0,"Out of Memory.\n"); 
    exit(ABNORMAL_EXIT);
  }
  for(row=0; row<nrow-1; row++){
    if((array[row]=calloc(ncol,size))==NULL){
      fprintf(sp0,"Out of Memory.\n");
      exit(ABNORMAL_EXIT);
    }
  }
  for(row=nrow-1; row<2*nrow-1; row++){
    if((array[row]=calloc((ncol-1),size))==NULL){
      fprintf(sp0,"Out of Memory.\n");
      exit(ABNORMAL_EXIT);
    }
  }
  return(array);
}


/* function: MAlloc()
 * --------------------
 * Has same functionality as malloc(), but exits if out of memory.
 */
void *MAlloc(size_t size){

  void *ptr;

  if((ptr=malloc(size))==NULL){
    fprintf(sp0,"Out of Memory\n");
    exit(ABNORMAL_EXIT);
  }
  return(ptr);
}


/* function: CAlloc()
 * ------------------
 * Has same functionality as calloc(), but exits if out of memory.
 */
void *CAlloc(size_t nitems, size_t size){
  
  void *ptr;
  
  if((ptr=calloc(nitems,size))==NULL){
    fprintf(sp0,"Out of memory\n");
    exit(ABNORMAL_EXIT);
  }
  return(ptr);
}


/* function: ReAlloc()
 * -------------------
 * Has same functionality as realloc(), but exits if out of memory.
 */
void *ReAlloc(void *ptr, size_t size){
  
  void *ptr2;
  
  if((ptr2=realloc(ptr,size))==NULL){
    fprintf(sp0,"Out of Memory\n");
    exit(ABNORMAL_EXIT);
  }
  return(ptr2);
}


/* function: Free2DArray()
 * -----------------------
 * This function frees the dynamically allocated memory for a 2D
 * array.  Pass in a pointer to a pointer cast to a void **.
 * The function assumes the array is of the form arr[rows][cols]
 * so that nrow is the number of elements in the pointer array.
 */
void Free2DArray(void **array, unsigned int nrow){

  int row;

  for(row=0; row<nrow; row++){
    free(array[row]);
  }
  free(array);
}


/* function: FreeParamT()
 * ----------------------
 * Frees memory associated with pointer to paramT structure, including 
 * memory allocated for structure's own internal pointers.
 */
void FreeParamT(paramT *params){

  /* free string memory */
  if(params->costinfile!=NULL){
    free(params->costinfile);
  }
  if(params->costoutfile!=NULL){
    free(params->costoutfile);
  }
  if(params->initfile!=NULL){
    free(params->initfile);
  }
  if(params->flowfile!=NULL){
    free(params->flowfile);
  }
  if(params->eifile!=NULL){
    free(params->eifile);
  }
  if(params->rowcostfile!=NULL){
    free(params->rowcostfile);
  }
  if(params->colcostfile!=NULL){
    free(params->colcostfile);
  }
  if(params->mstrowcostfile!=NULL){
    free(params->mstrowcostfile);
  }
  if(params->mstcolcostfile!=NULL){
    free(params->mstcolcostfile);
  }
  if(params->mstcostsfile!=NULL){
    free(params->mstcostsfile);
  }
  if(params->corrdumpfile!=NULL){
    free(params->corrdumpfile);
  }

  /* free structre itself */
  free(params);
}


/* function: Set2DShortArray()
 * -------------------------
 * Sets all entries of a 2D array of shorts to the given value.  Assumes
 * that memory is already allocated.
 */
void Set2DShortArray(short **arr, long nrow, long ncol, long value){

  long row, col;

  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      arr[row][col]=value;
    }
  }
}


/* function: ValidDataArray()
 * --------------------------
 * Given a 2D floating point array, returns FALSE if any elements are NaN
 * or infinite, and TRUE otherwise (uses math library finite() function).
 */
signed char ValidDataArray(float **arr, long nrow, long ncol){

  long row, col;

  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      if(!IsFinite(arr[row][col])){
	return(FALSE);
      }
    }
  }
  return(TRUE);
}


/* function: IsFinite()
 * --------------------
 * This function takes a double and returns a nonzero value if 
 * the arguemnt is finite (not NaN and not infinite), and zero otherwise.
 * Different implementations are given here since not all machines have
 * these functions available.
 */
signed char IsFinite(double d){

  return(finite(d));
  /* return(isfinite(d)); */
  /* return(!(isnan(d) || isinf(d))); */
  /* return(TRUE) */
}


/* function: FRound()
 * ------------------
 * Rounds a floating point number to the nearest integer.  
 * The function takes a float and returns a long.
 */
long FRound(float a){

  return((long )rint(a));
}


/* function: Short2DRowColAbsMax()
 * -------------------------------
 * Given some initial long, replaces the value if the maximum element
 * absolute value in a two-dimensional short array is greater than 
 * the initial value.  The number of rows and columns should be passed in.
 */
void Short2DRowColAbsMax(short **arr, long nrow, long ncol, long *initval){

  long row, col;

  for(row=0;row<nrow-1;row++){
    for(col=0;col<ncol;col++){
      if(labs(arr[row][col])>(*initval)){
	*initval=labs(arr[row][col]);
      }
    }
  }
  for(row=nrow-1;row<2*nrow-1;row++){
    for(col=0;col<ncol-1;col++){
      if(labs(arr[row][col])>(*initval)){
	*initval=labs(arr[row][col]);
      }
    }
  }
}


/* function: Despeckle()
 * ---------------------
 * Filters magnitude data with adaptive geometric filter to get rid of 
 * speckle.  Allocates 2D memory for ei.
 */
void Despeckle(float **mag, float ***ei, long nrow, long ncol){

  float **intensity;
  float ratio, ratiomax, wfull, wstick, w[NARMS+1];
  long row, col, i, j, k, Irow, Icol;
  short jmin[5]={2,2,0,1,2};
  short jmax[5]={2,3,4,3,2};
  enum{ C=0, T, B, R, L, TR, BL, TL, BR};

  /* get memory (intensity array is padded) */
  if(*ei==NULL){
    (*ei)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
  }

  /* pad interferogram magnitude in new array */
  intensity=MirrorPad(mag,nrow,ncol,ARMLEN,ARMLEN);

  /* do the filtering */
  for(row=0;row<nrow;row++){
    Irow=row+ARMLEN;
    for(col=0;col<ncol;col++){
      Icol=col+ARMLEN;
      for(k=0;k<NARMS+1;k++){
        w[k]=0;
      }
      for(i=-1;i<=1;i++){
	for(j=-1;j<=1;j++){
          w[C]+=intensity[Irow+i][Icol+j];
	}
      }
      for(i=-1;i<=1;i++){
        for(j=2;j<ARMLEN+1;j++){
          w[T]+=intensity[Irow-j][Icol+i];
          w[B]+=intensity[Irow+j][Icol+i];
          w[L]+=intensity[Irow+i][Icol-j];
          w[R]+=intensity[Irow+i][Icol+j];
        }
      }
      for(i=0;i<=4;i++){
        for(j=jmin[i];j<=jmax[i];j++){
          w[TR]+=intensity[Irow-i][Icol+j];
          w[BR]+=intensity[Irow+i][Icol+j];
          w[BL]+=intensity[Irow+i][Icol-j];
          w[TL]+=intensity[Irow-i][Icol-j];
        }
      }
      wfull=w[C]+w[T]+w[R]+w[B]+w[L];
      for(i=2;i<5;i++){
        for(j=2;j<7-i;j++){
          wfull+=intensity[Irow+i][Icol+j];
          wfull+=intensity[Irow-i][Icol+j];
          wfull+=intensity[Irow+i][Icol-j];
          wfull+=intensity[Irow-i][Icol-j];
        }
      } 
      ratiomax=1;
      for(k=1;k<=NARMS;k+=2){
	wstick=w[0]+w[k]+w[k+1];
        if((ratio=wstick/(wfull-wstick))<1){
          ratio=1/ratio;
        }
        if(ratio>ratiomax){
          ratiomax=ratio;
          (*ei)[row][col]=wstick;
        }
      }
    }
  }   

  /* free memory */
  Free2DArray((void **)intensity,nrow+2*ARMLEN);
}



/* function: MirrorPad()
 * ---------------------
 * Returns pointer to 2D array where passed array is in center and
 * edges are padded by mirror reflections.
 */
float **MirrorPad(float **array1, long nrow, long ncol, long krow, long kcol){

  long row, col;
  float **array2;

  /* get memory */
  if((array2=(float **)Get2DMem(nrow+2*krow,ncol+2*kcol,
			   sizeof(float *),sizeof(float)))==NULL){
    fprintf(sp0,"Out of memory\n");
    exit(ABNORMAL_EXIT);
  }
  
  /* center array1 in new array */
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      array2[row+krow][col+kcol]=array1[row][col];
    }
  }

  /* mirror reflect edges */
  for(row=0;row<krow;row++){
    for(col=0;col<kcol;col++){
      array2[row][col]=array2[2*krow-row][2*kcol-col];
      array2[row][ncol+kcol+col]
        =array2[2*krow-row][ncol+kcol-2-col];
      array2[nrow+krow+row][col]
        =array2[nrow+krow-2-row][2*kcol-col];
      array2[nrow+krow+row][ncol+kcol+col]
        =array2[nrow+krow-2-row][ncol+kcol-2-col];
    }
  }
  for(row=krow;row<nrow+krow;row++){
    for(col=0;col<kcol;col++){
      array2[row][col]=array2[row][2*kcol-col];
      array2[row][ncol+kcol+col]
        =array2[row][ncol+kcol-2-col];
    }
  }
  for(col=kcol;col<ncol+kcol;col++){
    for(row=0;row<krow;row++){
      array2[row][col]=array2[2*krow-row][col];
      array2[nrow+krow+row][col]
        =array2[nrow+krow-2-row][col];
    }
  }
  
  /* return a pointer to the padded array */
  return(array2);

}


/* function: BoxCarAvg()
 * ---------------------
 * Takes in 2-D array, convolves with boxcar filter of size specified.
 */
void BoxCarAvg(float **avgarr, float **padarr, long nrow, long ncol, 
	       long krow, long kcol){

  long i, row, col, n;
  double window;

  /* calculate first cell */
  window=0;
  for(row=0;row<krow;row++){
    for(col=0;col<kcol;col++){
      window+=padarr[row][col];
    }
  }
  avgarr[0][0]=(float )window;

  /* loop over all rows */
  row=0;
  while(TRUE){

    /* calculate first cell */
    window=0;
    for(i=row;i<row+krow;i++){
      for(col=0;col<kcol;col++){
	window+=padarr[i][col];
      }
    }
    avgarr[row][0]=(float )window;

    /* convolve window with row */
    for(col=1;col<ncol;col++){
      for(i=row;i<row+krow;i++){
	window-=padarr[i][col-1];
	window+=padarr[i][col+kcol-1];
      }
      avgarr[row][col]=(float )window;
    }

    /* break if finished all rows */
    if(++row>=nrow){
      break;
    }
  }

  /* normalize */
  n=krow*kcol;
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      avgarr[row][col]/=n;
    }
  }
}


/* function: StrNCopy()
 * --------------------
 * Just like strncpy(), but puts terminates string by putting a null 
 * character at the end (may overwrite last character).
 */
char *StrNCopy(char *dest, const char *src, size_t n){
 
  char *s;

  s=strncpy(dest,src,n-1);
  dest[n-1]='\0';
  return(s);
}


/* function: FlattenWrappedPhase()
 * -------------------------------
 * Given a wrapped phase array and an unwrapped phase array, subtracts
 * the unwrapped data elementwise from the wrapped data and stores
 * the result, rewrapped to [0,2pi), in the wrapped array.
 */
void FlattenWrappedPhase(float **wrappedphase, float **unwrappedest, 
			 long nrow, long ncol){
 
  long row, col;

  /* loop to subtract, rewrap, store in wrapped array. */
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      wrappedphase[row][col]-=unwrappedest[row][col];
      wrappedphase[row][col]=fmod(wrappedphase[row][col],TWOPI);
      if(wrappedphase[row][col]<0){
	wrappedphase[row][col]+=TWOPI;
      }
    }
  }
}


/* function: Add2DFloatArrays()
 * ----------------------------
 * Addes the values of two 2-D arrays elementwise.
 */
void Add2DFloatArrays(float **arr1, float **arr2, long nrow, long ncol){

  long row, col;
 
  /* loop over all rows and columns, add and store result in first array */
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      arr1[row][col]+=arr2[row][col];
    }
  }

}


/* function: StringToDouble()
 * --------------------------
 * Uses strtod to convert a string to a double, but also does error checking.
 * If any part of the string is not converted, the function does not make
 * the assignment and returns TRUE.  Otherwise, returns FALSE.
 */
int StringToDouble(char *str, double *d){

  double tempdouble;
  char *endp;
  
  endp=str;
  tempdouble=strtod(str,&endp);
  if(strlen(endp) || tempdouble>=HUGE_VAL || tempdouble<=-HUGE_VAL){
    return(TRUE);
  }else{
    *d=tempdouble;
    return(FALSE);
  }
}


/* function: StringToLong()
 * ------------------------
 * Uses strtol to convert a string to a base-10 long, but also does error 
 * checking.  If any part of the string is not converted, the function does 
 * not make the assignment and returns TRUE.  Otherwise, returns FALSE.
 */
int StringToLong(char *str, long *l){

  long templong;
  char *endp;
  
  endp=str;
  templong=strtol(str,&endp,10);
  if(strlen(endp) || templong==LONG_MAX || templong==LONG_MIN){
    return(TRUE);
  }else{
    *l=templong;
    return(FALSE);
  }
}


/* function: SetDump()
 * -------------------
 * Set the global variable dumpresults_global to TRUE if SIGINT or SIGHUP
 * signals recieved.  Also sets requestedstop_global if SIGINT signal 
 * received.  This function should only be called via signal() when 
 * a signal is caught.
 */ 
void SetDump(int signum){

  if(signum==SIGINT){

    /* exit if we receive another interrupt */
    signal(SIGINT,exit);

    /* print nice message and set global variables so program knows to exit */
    fprintf(sp0,"\n\nSIGINT signal caught.  Please wait for graceful exit\n");
    fprintf(sp0,"(One more interrupt signal halts job)\n");
    dumpresults_global=TRUE;
    requestedstop_global=TRUE;

  }else if(signum==SIGHUP){

    /* make sure the hangup signal doesn't revert to default behavior */
    signal(SIGHUP,SetDump);

    /* print a nice message, and set the dump variable */
    fprintf(sp0,"\n\nSIGHUP signal caught.  Dumping results\n");
    dumpresults_global=TRUE;

  }else{
    fprintf(sp0,"WARNING: invalid signal (%d) passed to signal handler\n",
	    signum);
  }
}


/* function: StartTimers()
 * -----------------------
 * Starts the wall clock and CPU timers for use in conjunction with
 * DisplayElapsedTime().
 */
void StartTimers(time_t *tstart, double *cputimestart){
  
  struct rusage usagebuf;

  *tstart=time(NULL);
  *cputimestart=-1.0;
  if(!getrusage(RUSAGE_SELF,&usagebuf)){
    *cputimestart=(double )(usagebuf.ru_utime.tv_sec
			    +(usagebuf.ru_utime.tv_usec/(double )1000000)
			    +usagebuf.ru_stime.tv_sec
			    +(usagebuf.ru_stime.tv_usec/(double )1000000));
    if(!getrusage(RUSAGE_CHILDREN,&usagebuf)){
      *cputimestart+=(double )(usagebuf.ru_utime.tv_sec
			       +(usagebuf.ru_utime.tv_usec/(double )1000000)
			       +usagebuf.ru_stime.tv_sec
			       +(usagebuf.ru_stime.tv_usec/(double )1000000));
    }
  }
}


/* function: DisplayElapsedTime()
 * ------------------------------
 * Displays the elapsed wall clock and CPU times for the process and its
 * children.  Times should be initialized at the start of the program with
 * StartTimers().
 */
void DisplayElapsedTime(time_t tstart, double cputimestart){

  double cputime, walltime, seconds;
  long hours, minutes;
  time_t tstop;
  struct rusage usagebuf;

  cputime=-1.0;
  if(!getrusage(RUSAGE_CHILDREN,&usagebuf)){
    cputime=(double )(usagebuf.ru_utime.tv_sec
		       +(usagebuf.ru_utime.tv_usec/(double )1000000)
		       +usagebuf.ru_stime.tv_sec
		       +(usagebuf.ru_stime.tv_usec/(double )1000000));
    if(!getrusage(RUSAGE_SELF,&usagebuf)){
      cputime+=(double )(usagebuf.ru_utime.tv_sec
			 +(usagebuf.ru_utime.tv_usec/(double )1000000)
			 +usagebuf.ru_stime.tv_sec
			 +(usagebuf.ru_stime.tv_usec/(double )1000000));
    }
  }
  tstop=time(NULL);
  if(cputime>0 && cputimestart>=0){
    cputime-=cputimestart;
    hours=(long )floor(cputime/3600);
    minutes=(long )floor((cputime-3600*hours)/60);
    seconds=cputime-3600*hours-60*minutes;
    fprintf(sp1,"Elapsed processor time:   %ld:%02ld:%05.2f\n",
	    hours,minutes,seconds);
  }
  if(tstart>0 && tstop>0){
    walltime=tstop-tstart;
    hours=(long )floor(walltime/3600);
    minutes=(long )floor((walltime-3600*hours)/60);
    seconds=walltime-3600*hours-60*minutes;
    fprintf(sp1,"Elapsed wall clock time:  %ld:%02ld:%02ld\n",
	    hours,minutes,(long )seconds);
  }
}
