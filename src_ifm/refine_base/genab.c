#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"

/* for convenience in a diagnostic */
#define MAC1  (acos((re*re - H*H - Rnear*Rnear) / (-2.0*H*Rnear)))
#define MAC2  (asin(sin(MAC1)*Rnear/re))
#define MAC3  (acos((re*re - H*H - Rcenter*Rcenter) / (-2.0*H*Rcenter)))
#define MAC4  (asin(sin(MAC3)*Rcenter/re))
#define MAC5  (acos((re*re - H*H - Rfar*Rfar) / (-2.0*H*Rfar)))
#define MAC6  (asin(sin(MAC5)*Rfar/re))


int genab(char *datafile, char *basefile, char *metaName, char *matfile, char *vecfile)
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

	if (!quietflag) printf("   Top left Phase Rate=%f m/radian\n",meta_phase_rate(meta,base,0,0));
	
/*	printf("\nCalculating terms for matrix and vector...\n\n");*/
	/*Loop over seed points, calculating terms for matrix and vector.*/
	for (i = 1; i <= m; i++) {
		double flat,k;
		/*uwp[i] += meta_flat_phase(meta,base,y[i],x[i]);*/
		
		/*Calculate the expected output element b[i].  This is the quantity of deramping
		needed to move the satellite's phase back to the control point's phase-- that is,
		it's the satellite phase, compensated for the control point's height.
		It's measured in radians.*/
		b[i]=uwp[i]-(z[i]-z[1])/meta_phase_rate(meta,base,y[i],x[i]);
		
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
/*		printf("\nMatrix calculated....\n");*/
		
		/* write out matrix & vector*/
/*		printf("Writing output files...\n");*/
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

