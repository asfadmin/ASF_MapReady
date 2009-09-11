/****************************************************************
FUNCTION NAME: imos2d

SYNTAX: void imos2d(v,vo,dim,os);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v           Complex *	input vector
    vo          Complex *       oversampling vector
    dim         int             size of 2d vector array
    os          int             oversampling factor

DESCRIPTION:
    Generalized 2D complex oversampling. 
    Destroys input vector v.

RETURN VALUE:
    Oversampled vector output.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Rob Fatland & Mike Shindle - original development

****************************************************************/
#include "asf.h"
#include "imsl.h"
#include "ifm.h"

/* function declarations */
int init_coeff(int);

/* imsl declarations */
extern f2t2d_();
extern f2t2b_();

/* global variables */
#define MAXSIZE		6
int num_alloc=0;
static float *wff1[MAXSIZE];
static float *wff2[MAXSIZE];
static float *cpy[MAXSIZE];
int sizes[MAXSIZE];
static complexFloat cwk[1];

/* macro declarations */
#define free_coeff(a,dim)   ( free_cpxmatrix(a,0,dim-1,0,dim-1) )

void imos2d(complexFloat *v, 
       complexFloat *vo, 
       int dim, 
       int os)
{
  int i,j;
  int forward_fft =  1;
  int inverse_fft = -1;
  int osDim, osArea;
  int id_dim, id_osdim;
  complexFloat zero;

  /* calculate total size of oversampled array */
  osDim = dim*os;
  osArea = SQR(osDim);
 
  /* init workspace */
  id_dim = init_coeff(dim);
  id_osdim = init_coeff(osDim);

  /* 2-D fft vector 'v' */
  f2t2d_(&dim,&dim,v,&dim,v,&dim,
	 wff1[id_dim],wff2[id_dim],cwk,cpy[id_dim]); 
  
  /* fft2d (v, dim, forward_fft); */

  /* make sure 'vo' = 0.0 */
  zero = Czero();
  for (i=0; i<osArea; i++) 
       vo[i] = zero;

  /* zero pad 'v' into 'vout' */
  zeroPad(v, vo, dim, os);

  /* inverse FFT 'vo' */
  f2t2b_(&osDim,&osDim,vo,&osDim,vo,&osDim,
	 wff1[id_osdim],wff2[id_osdim],cwk,cpy[id_osdim]);
  
  /* fft2d (vo, odim, inverse_fft); */

  return;
}

int
init_coeff(int dim)
{
  int id;

  if (num_alloc > MAXSIZE) 
     Exit("Not enough memory to allocate workspace.\n");

  if (num_alloc != 0) { 
    id = 0;
    while (id < num_alloc) {
      if (dim == sizes[id])
        return(id);
      id++;
    }
  }

  id = num_alloc;
  num_alloc++;

  sizes[id] = dim;
  wks2dAllocateAndInit(dim,
		     (wff1+id),
		     (wff2+id),
		     (cpy+id));
  return(id);
}

void
imos2d_free_wks(void)
{
  int i;

  for (i=0; i < num_alloc; i++) 
    wks2dFree((wff1+i),(wff2+i),(cpy+i));

}
