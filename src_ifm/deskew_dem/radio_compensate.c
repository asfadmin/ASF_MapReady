#include "deskew_dem.h"

void radio_compensate(float *grDEM, float *grDEMprev,float *inout,int ns)
{
	static double *cosineScale=NULL;
	int x;
	if (cosineScale==NULL)
	{
		int i;
		double cosAng;
		cosineScale=(double *)MALLOC(sizeof(double)*512);
		for (i=0;i<512;i++)
		{
			cosAng=i/511.0;
			cosineScale[(int)(cosAng*511)]=1.00-0.70*pow(cosAng,7.0);
		}
	}
	for (x=1;x<ns;x++)
	{
		double dx,dy,grX,vecLen,cosAng;
	  /*Find terrain normal.*/
	  	grX=grDEM[x];
		if ((grX==badDEMht)||
		    (grDEMprev[x]==badDEMht)||
		    (grDEM[x-1]==badDEMht)) 
			{inout[x]=0;continue;}
		dx=(grX-grDEM[x-1])/grPixelSize;
		dy=(grDEMprev[x]-grX)/grPixelSize;
	  /*Make the normal a unit vector.*/
		vecLen=sqrt(dx*dx+dy*dy+1);
	  /*Take dot product of this vector and the incidence vector.*/
		cosAng=(dx*sinIncidAng[x]+cosIncidAng[x])/vecLen;
		if (cosAng>0)/* Ordinary diffuse radar reflection */
			inout[x]*=cosineScale[(int)(cosAng*511)];/*=cosScaled*255;*/
	}
}

/*
Here's what it looked like before optimization:
		double dx,dy,dz,vecLen,ix,iy,iz,cosAng;
	  Find terrain normal.
		dx=(grDEM[x-1]-grDEM[x])/grPixelSize;
		dy=(grDEMprev[x]-grDEM[x])/grPixelSize;
		dz=1.0;
	  Make the normal a unit vector.
		vecLen=1.0/sqrt(dx*dx+dy*dy+dz*dz);
		dx*=vecLen;
		dy*=vecLen;
		dz*=vecLen;
	  Find the incidence angle normal.
	  	ix=-sin(incidAng[x]);
	  	iy=0;
	  	iz=cos(incidAng[x]);
	  Take dot product of these two vectors.
		cosAng=dx*ix+dy*iy+dz*iz;
		if (cosAng<0)
		{ 
	Shadowed area.
			inout[x]=(1&(x/10))?200:100;
		} else {
		Ordinary diffuse radar reflection--
	scale by phong equation.
			double cosScaled=pow(cosAng,10);
			double normFact=1.0-0.99*cosScaled;
			inout[x]*=normFact;
		}

*/
