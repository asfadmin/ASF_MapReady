#include <stdio.h>
#include <unistd.h>

void newtprop(double *statevec,double *t, double *pstep, double *propvec);

main(int argc,char *argv[])
{
	double stvec[6],propvec[6];
	int i;
	double time,pstep;
	if (argc!=9)
	{
		printf("Usage: newtprop <state vector> <step> <time>\n");
		exit(1);
	}
	for (i=1;i<7;i++)
		sscanf(argv[i],"%lf",&stvec[i-1]);
	sscanf(argv[7],"%lf",&pstep);
	sscanf(argv[8],"%lf",&time);
	
	newtprop(stvec,&time,&pstep,propvec);
	printf("Newt Pos: %f %f %f m\n",propvec[0],propvec[1],propvec[2]);
	printf("Newt Vel: %f %f %f m/s\n",propvec[3],propvec[4],propvec[5]);
	
	return 0;
}
#define SQR(x) ((x)*(x))
void newtprop(double *statevec,double *t, double *pstep, double *propvec)
{
	int i,idummy;
	double a[3];
	double tstep = *pstep,tlast,cnst;
	int nstep;
	for (i=0;i<6;i++)
		propvec[i]=statevec[i];    
	nstep=(int)(*t/tstep);
	tlast= *t - tstep*nstep;
	for (idummy=0;idummy<nstep;idummy++)
	{
	   double h2,v2;
	   h2=SQR(propvec[0]) + SQR(propvec[1])+ SQR(propvec[2]);
	   v2=SQR(propvec[3]) + SQR(propvec[4])+ SQR(propvec[5]);
	   cnst=v2/h2;
	   for (i=0;i<3;i++)     
	   {        
	      a[i]=-cnst*propvec[i];
	      propvec[i]=propvec[i]+propvec[i+3]*tstep+0.5*a[i]*SQR(tstep);
	      propvec[i+3]=propvec[i+3] + a[i]*tstep;
	   }
	   /*printf("Position: t=%f, %f %f %f\n",idummy*tstep,propvec[0],propvec[1],propvec[2]);*/
	}
	for (i=0;i<3;i++)
	{             
	   a[i]=-cnst*propvec[i];
	   propvec[i]=propvec[i]+propvec[i+3]*tlast+0.5*a[i]*SQR(tlast);
	   propvec[i+3]=propvec[i+3] + a[i]*tlast;
	}
}
