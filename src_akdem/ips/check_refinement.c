#include "asf.h"

typedef struct
{
	double Bn,dBn;
	double Bp,dBp;
} baseLine;

baseLine readBaseLine(char *fname)
{
	baseLine base;
	FILE *fp1;
	fp1 = FOPEN(fname,"r");
	if (4!=fscanf(fp1,"%lf%lf%lf%lf",&base.Bn,&base.dBn,&base.Bp,&base.dBp))
		{ printf("Unable to read baseline from file '%s'\n",fname); exit(1); }
	fclose(fp1);
	return base;
}

double baseLineDiff(const baseLine *a,const baseLine *b)
{
	return fabs(a->Bn-b->Bn)+
		fabs(a->dBn-b->dBn)+
		fabs(a->Bp-b->Bp)+
		fabs(a->dBp-b->dBp);
}

int check_refinement(char *base1, char *base2, char *base3)
{
    baseLine b1,b2,b3;
    b1=readBaseLine(base1);
    if (base2!=NULL)
    	b2=readBaseLine(base2);
    if (base3!=NULL)
    	b3=readBaseLine(base3);
    
    /*If the baseline's changed by less than 1 mm,*/
    if (base2!=NULL)
    	if (baseLineDiff(&b1,&b2) < 0.001) 
    		return(1); /*we've sucessfully converged.*/
    
    /*If the baseline's changed by less than 1 mm,*/
    if (base3!=NULL)
    	if (baseLineDiff(&b1,&b3) < 0.001) 
		return(1); /*we're cyclic, but (pretty close to) converged anyway.*/
    return(0);
}
