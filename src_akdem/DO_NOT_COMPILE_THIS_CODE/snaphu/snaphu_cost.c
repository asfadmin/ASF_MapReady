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


/* function: BuildCostArrays()
 * ---------------------------
 * Builds cost arrays for arcs based on interferogram intensity 
 * and correlation, depending on options and passed parameters.  
 */
void BuildCostArrays(void ***costsptr, short ***mstcostsptr, 
		     float **mag, float **wrappedphase, 
		     float **unwrappedest, char *ampfile, 
		     char *ampfile2, char *weightfile, char *corrfile, 
		     long linelen, long nlines, long nrow, long ncol, 
		     paramT *params){
  
  long row, col, maxcol, iei, irho, nrho, tempcost, defomax;
  long kperpdpsi, kpardpsi, sigsqshortmin, poscost, negcost;
  double sintheta;
  double dzlay, dzei, dzr0, dzrcrit, dzeimin, dphilaypeak, *dzrhomax;
  double azdzfactor, dzeifactor, dzlayfactor;
  double avgei, eicrit, layminei, laywidth, slope1, const1, slope2, const2;
  double rho, rho0, rhomin, drho, rhopow, irhoconst;
  double sigsqrho, sigsqrhoconst, sigsqei, sigsqlay;
  double glay, costscale, ambiguityheight, ztoshort, ztoshortsq;
  double defocorrthresh, sigsqcorr, nshortcycle, nshortcyclesq; 
  float **pwr, **ei, **corr, **dpsi, **avgdpsi, **paddpsi;
  short **weights, **rowweight, **colweight;
  costT **costs, **rowcost, **colcost;
  signed char noshadow, nolayover;


  /* read weights */
  weights=NULL;
  ReadWeightsFile(&weights,weightfile,linelen,nlines,params);
  rowweight=weights;
  colweight=&weights[nrow-1];

  /* if we're only initializing and we don't want statistical weights */
  if(params->initonly && params->nostatcosts){
    *mstcostsptr=weights;
    return;
  }

  /* get memory and set pointers to functions for calculating arc distnaces */
  if(params->p>=0){
    *costsptr=(void **)Get2DRowColMem(nrow,ncol,sizeof(short *),sizeof(short));
    if(!params->nostatcosts){
      costs=(costT **)Get2DRowColMem(nrow,ncol,sizeof(costT *),sizeof(costT));
      rowcost=costs;
      colcost=(costT **)&costs[nrow-1];
    }
    if(params->p==0){
      CalcCost=CalcCostL0;
      EvalCost=EvalCostL0;
    }else if(params->p==1){
      CalcCost=CalcCostL1;
      EvalCost=EvalCostL1;
    }else if(params->p==2){
      CalcCost=CalcCostL2;
      EvalCost=EvalCostL2;
    }else{
      CalcCost=CalcCostLP;
      EvalCost=EvalCostLP;
    }
  }else{
    *costsptr=(void **)Get2DRowColMem(nrow,ncol,sizeof(costT *),sizeof(costT));
    costs=(costT **)(*costsptr);
    rowcost=costs;
    colcost=(costT **)&costs[nrow-1];
    if(params->defo){
      CalcCost=CalcCostDefo;
      EvalCost=EvalCostDefo;
    }else{
      CalcCost=CalcCostTopo;
      EvalCost=EvalCostTopo;
    }
  }

  /* build or read the statistical cost arrays unless we were told not to */
  if(params->costinfile!=NULL){
    fprintf(sp1,"Reading cost information from file %s\n",params->costinfile);
    Read2DRowColFile((void ***)&costs,params->costinfile,
		     linelen,nlines,params,sizeof(costT));
    
    /* by convention, costs[0][0].sigsq<0 in file if baseline is positive */
    if(((costT **)costs)[0][0].sigsq<0){
      
      /* set the special sigsq member back to a positive value */
      ((costT **)costs)[0][0].sigsq*=-1;

      /* if baseline isn't already positive */
      if(params->bperp<=0){
	
	/* set baseline positve and flip phase arrays */
	params->bperp=100.0;
	FlipPhaseArraySign(wrappedphase,params->bperp,nrow,ncol);
	WrapPhase(wrappedphase,nrow,ncol);
	if(unwrappedest!=NULL){
	  FlipPhaseArraySign(unwrappedest,params->bperp,nrow,ncol);
	}

      }
    }

  }else if(!params->nostatcosts){

    /* get intensity and correlation info */
    /* correlation generated from interferogram and amplitude if not given */
    GetIntensityAndCorrelation(mag,wrappedphase,&pwr,&corr,ampfile,ampfile2,
			       corrfile,linelen,nlines,nrow,ncol,params);

    /* print a nice message */
    if(params->defo){
      fprintf(sp1,"Calculating deformation-mode cost parameters\n");
    }else{
      fprintf(sp1,"Calculating topography-mode cost parameters\n");
    }

    /* correlation parameters */
    rho0=(params->rhosconst1)/(params->nlooks)+(params->rhosconst2);
    rhopow=2*(params->cstd1)+(params->cstd2)*log(params->nlooks)
      +(params->cstd3)*(params->nlooks);
    rhomin=params->rhominfactor*rho0;
    sigsqshortmin=params->sigsqshortmin;

    /* set some cost parameters */
    kperpdpsi=params->kperpdpsi;
    kpardpsi=params->kpardpsi;
    costscale=params->costscale; 
    nshortcycle=params->nshortcycle;
    nshortcyclesq=nshortcycle*nshortcycle;

    /* topography-specific stuff */
    if(!params->defo){

      /* despeckle the interferogram intensity */
      fprintf(sp2,"Despeckling intensity image\n");
      ei=NULL;
      Despeckle(pwr,&ei,nrow,ncol);
      Free2DArray((void **)pwr,nrow);

      /* remove large-area average intensity */
      fprintf(sp2,"Normalizing intensity\n");
      RemoveMean(ei,nrow,ncol,params->krowei,params->kcolei);

      /* dump normalized, despeckled intensity */
      if(params->eifile!=NULL){
	Write2DArray((void **)ei,params->eifile,nrow,ncol,sizeof(float));
      }

      /* SAR and geometry parameters */
      /* ambiguity height is always positive since if baseline is positive, */
      /*   we flip signs of phase arrays after reading and before writing */
      sintheta=sin(params->theta);
      dzr0=-(params->dr)*(cos(params->theta));
      ambiguityheight=fabs((params->lambda)*(params->nomrange)*sintheta
	/(2*(params->bperp)));
      costscale*=fabs(params->bperp/params->costscalebperp);

      /* scattering model parameters */
      dzrcrit=SolveDZRCrit(params,params->threshold);
      SolveEIModelParams(&slope1,&slope2,&const1,&const2,dzrcrit,params);
      eicrit=(dzrcrit-const1)/slope1;
      noshadow=!(params->shadow);
      dzeimin=params->dzeimin;
      dphilaypeak=params->dzlaypeak/ambiguityheight;

      /* pdf model parameters for f(dzr|EI) */
      layminei=params->layminei;
      laywidth=params->laywidth;
      azdzfactor=params->azdzfactor;
      dzeifactor=params->dzeifactor;
      dzlayfactor=params->dzlayfactor;
      glay=-costscale*log(params->layconst);
      sigsqrhoconst=2.0*ambiguityheight*ambiguityheight/12.0;  
      sigsqei=params->sigsqei;
      ztoshort=nshortcycle/ambiguityheight;
      ztoshortsq=ztoshort*ztoshort;
      sigsqlay=ambiguityheight*ambiguityheight*params->sigsqlayfactor;
      
      /* build lookup array for maximum dzr given correlation */
      fprintf(sp2,"Building correlation-slope lookup table\n");
      drho=params->drho;
      nrho=ceil((1-rhomin)/drho);
      dzrhomax=MAlloc(nrho*sizeof(double));
      rho=rhomin;
      for(irho=0;irho<nrho;irho++){
	rho+=drho;
	dzrhomax[irho]=CalcDzRhoMax(rho,params,params->threshold);
      }
      irhoconst=(rho-rhomin)/(nrho-1);

    }else{

      /* deformation mode: correlation cost model parameters */
      azdzfactor=params->defoazdzfactor;
      glay=-costscale*log(params->defolayconst);
      sigsqrhoconst=2.0/12.0;
      defomax=(long )ceil(params->defomax*nshortcycle);
      defocorrthresh=params->defothreshfactor*rho0;
      sigsqcorr=params->sigsqcorr;
    }

    /* get memory for wrapped difference arrays */
    dpsi=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    avgdpsi=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));

    /* build array of wrapped phase differences in range */
    fprintf(sp2,"Building range cost arrays\n");
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol-1;col++){
	dpsi[row][col]=(wrappedphase[row][col+1]-wrappedphase[row][col])/TWOPI;
	if(dpsi[row][col]>=0.5){
	  dpsi[row][col]-=1.0;
	}else if(dpsi[row][col]<-0.5){
	  dpsi[row][col]+=1.0;
	}
      }
    }

    /* simple average of phase differences in range */
    /* (biased, but mean phase differences usually near zero, */
    /*  so don't bother with complex averaging) */
    paddpsi=MirrorPad(dpsi,nrow,ncol-1,(kperpdpsi-1)/2,(kpardpsi-1)/2);
    BoxCarAvg(avgdpsi,paddpsi,nrow,ncol-1,kperpdpsi,kpardpsi);
    Free2DArray((void **)paddpsi,nrow+kperpdpsi-1);

    /* build colcost array (range slopes) */
    /* for the colcost array, there is no symmetry for positive and */
    /* negative flows, so we do not average ei[][] and corr[][] values */
    for(col=0;col<ncol-1;col++){
      for(row=0;row<nrow;row++){
	
	/* topography case */
	if(!params->defo){

	  /* calculate variance due to decorrelation */
	  /* topo: factor of 2 in sigsqrhoconst for pdf convolution */
	  rho=corr[row][col];
	  if(rho<rhomin){
	    rho=0;
	  }
	  sigsqrho=sigsqrhoconst*pow(1-rho,rhopow);

	  /* calculate dz expected from EI if no layover */
	  if(ei[row][col]>eicrit){
	    dzei=(slope2*ei[row][col]+const2)*dzeifactor;
	  }else{
	    dzei=(slope1*ei[row][col]+const1)*dzeifactor;
	  }
	  if(noshadow && dzei<dzeimin){
	    dzei=dzeimin;
	  }

	  /* calculate dz expected from EI if layover exists */
	  dzlay=0;
	  if(ei[row][col]>layminei){
	    for(iei=0;iei<laywidth;iei++){
	      if(ei[row][col+iei]>eicrit){
		dzlay+=slope2*ei[row][col+iei]+const2;
	      }else{
		dzlay+=slope1*ei[row][col+iei]+const1;
	      }
	      if(col+iei>ncol-2){
		break;
	      }
	    }
	  }
	  if(dzlay){
	    dzlay=(dzlay+iei*(-2.0*dzr0))*dzlayfactor;
	  }
	  
	  /* set maximum dz based on unbiased correlation and layover max */ 
	  if(rho>0){
	    irho=(rho-rhomin)*irhoconst;
	    if(irho<0){
	      irho=0;
	    }else if(irho>=nrho){
	      irho=nrho-1;
	    }
	    if(dzrhomax[irho]<dzlay){
	      dzlay=dzrhomax[irho];
	    }
	  }

	  /* set cost parameters in terms of flow, represented as shorts */
	  nolayover=TRUE;
	  if(dzlay){
	    if(rho>0){
	      colcost[row][col].offset=nshortcycle*
		(dpsi[row][col]-0.5*(avgdpsi[row][col]+dphilaypeak));
	    }else{
	      colcost[row][col].offset=nshortcycle*
		(dpsi[row][col]-0.25*avgdpsi[row][col]-0.75*dphilaypeak);
	    }
	    colcost[row][col].sigsq=(sigsqrho+sigsqei+sigsqlay)*ztoshortsq
	      /(costscale*colweight[row][col]);
	    if(colcost[row][col].sigsq<sigsqshortmin){
	      colcost[row][col].sigsq=sigsqshortmin;
	    }
	    colcost[row][col].dzmax=dzlay*ztoshort;
	    colcost[row][col].laycost=colweight[row][col]*glay;
	    if(colcost[row][col].dzmax>floor(sqrt(colcost[row][col].laycost
						  *colcost[row][col].sigsq))){
	      nolayover=FALSE;
	    }
	  }
	  if(nolayover){
	    colcost[row][col].sigsq=(sigsqrho+sigsqei)*ztoshortsq
	      /(costscale*colweight[row][col]);
	    if(colcost[row][col].sigsq<sigsqshortmin){
	      colcost[row][col].sigsq=sigsqshortmin;
	    }
	    if(rho>0){
	      colcost[row][col].offset=ztoshort*
		(ambiguityheight*(dpsi[row][col]-0.5*avgdpsi[row][col])
		 -0.5*dzei);
	    }else{
	      colcost[row][col].offset=ztoshort*
		(ambiguityheight*(dpsi[row][col]-0.25*avgdpsi[row][col])
		 -0.75*dzei);
	    }
	    colcost[row][col].laycost=NOCOSTSHELF;
	    colcost[row][col].dzmax=LARGESHORT;
	  }

	}else{

	  /* deformation mode */
	  /* calculate variance due to decorrelation */
	  /* need symmetry for range if deformation */
	  rho=(corr[row][col]+corr[row][col+1])/2.0;
	  if(rho<defocorrthresh){
	    rho=0;
	  }
	  sigsqrho=(sigsqrhoconst*pow(1-rho,rhopow)+sigsqcorr)*nshortcyclesq;

	  /* set cost paramaters in terms of flow, represented as shorts */
	  if(rho>0){
	    colcost[row][col].offset=nshortcycle*
	      (dpsi[row][col]-avgdpsi[row][col]);
	  }else{
	    colcost[row][col].offset=nshortcycle*
	      (dpsi[row][col]-0.5*avgdpsi[row][col]);	    
	  }
	  colcost[row][col].sigsq=sigsqrho/(costscale*colweight[row][col]);
	  if(colcost[row][col].sigsq<sigsqshortmin){
	    colcost[row][col].sigsq=sigsqshortmin;
	  }
	  if(rho<defocorrthresh){
	    colcost[row][col].dzmax=defomax;
	    colcost[row][col].laycost=colweight[row][col]*glay;
	    if(colcost[row][col].dzmax<floor(sqrt(colcost[row][col].laycost
						  *colcost[row][col].sigsq))){
	      colcost[row][col].laycost=NOCOSTSHELF;
	      colcost[row][col].dzmax=LARGESHORT;
	    }
	  }else{
	    colcost[row][col].laycost=NOCOSTSHELF;
	    colcost[row][col].dzmax=LARGESHORT;
	  }
	}

	/* shift PDF to account for flattening by coarse unwrapped estimate */
	if(unwrappedest!=NULL){
	  colcost[row][col].offset+=(nshortcycle/TWOPI*
				     (unwrappedest[row][col+1]
				      -unwrappedest[row][col]));
	}

      }
    } /* end of range gradient cost calculation */

    /* reset layover constant for row (azimuth) costs */
    glay+=(-costscale*log(azdzfactor)); 

    /* build array of wrapped phase differences in azimuth */
    fprintf(sp2,"Building azimuth cost arrays\n");
    for(row=0;row<nrow-1;row++){
      for(col=0;col<ncol;col++){
	dpsi[row][col]=(wrappedphase[row][col]-wrappedphase[row+1][col])/TWOPI;
	if(dpsi[row][col]>=0.5){
	  dpsi[row][col]-=1.0;
	}else if(dpsi[row][col]<-0.5){
	  dpsi[row][col]+=1.0;
	}
      }
    }

    /* simple average of phase differences in azimuth */
    /* (biased, but mean phase differences usually near zero, */
    /*  so don't bother with complex averaging) */
    paddpsi=MirrorPad(dpsi,nrow-1,ncol,(kpardpsi-1)/2,(kperpdpsi-1)/2);
    BoxCarAvg(avgdpsi,paddpsi,nrow-1,ncol,kpardpsi,kperpdpsi);
    Free2DArray((void **)paddpsi,nrow-1+kpardpsi-1);

    /* build rowcost array */
    /* for the rowcost array, there is symmetry between positive and */
    /* negative flows, so we need to average ei[][] and corr[][] values */
    for(col=0;col<ncol;col++){
      for(row=0;row<nrow-1;row++){
	
	/* calculate correlation for this pixel */
	rho=(corr[row][col]+corr[row+1][col])/2.0;
	
	/* topography case */
	if(!params->defo){


	  /* clip small correlations because of estimator bias */
	  if(rho<rhomin){
	    rho=0;
	  }

	  /* variance due to decorrelation */
	  sigsqrho=sigsqrhoconst*pow(1-rho,rhopow);

	  /* if no layover, the expected dz for azimuth will always be 0 */
	  dzei=0;

	  /* calculate dz expected from EI if layover exists */
	  dzlay=0;
	  avgei=(ei[row][col]+ei[row+1][col])/2.0;
	  if(avgei>layminei){
	    for(iei=0;iei<laywidth;iei++){
	      avgei=(ei[row][col+iei]+ei[row+1][col+iei])/2.0;
	      if(avgei>eicrit){
		dzlay+=slope2*avgei+const2;
	      }else{
		dzlay+=slope1*avgei+const1;
	      }
	      if(col+iei>ncol-2){
		break;
	      }
	    }
	  }
	  if(dzlay){
	    dzlay=(dzlay+iei*(-2.0*dzr0))*dzlayfactor;
	  }
	  
	  /* set maximum dz based on correlation max and layover max */ 
	  if(rho>0){
	    irho=(rho-rhomin)*irhoconst;
	    if(irho<0){
	      irho=0;
	    }else if(irho>=nrho){
	      irho=nrho-1;
	    }
	    if(dzrhomax[irho]<dzlay){
	      dzlay=dzrhomax[irho];
	    }
	  }

	  /* set cost parameters in terms of flow, represented as shorts */
	  if(rho>0){
	    rowcost[row][col].offset=nshortcycle*
	      (dpsi[row][col]-avgdpsi[row][col]);
	  }else{
	    rowcost[row][col].offset=nshortcycle*
	      (dpsi[row][col]-0.5*avgdpsi[row][col]);
	  }
	  nolayover=TRUE;
	  if(dzlay){
	    rowcost[row][col].sigsq=(sigsqrho+sigsqei+sigsqlay)*ztoshortsq
	      /(costscale*rowweight[row][col]);
	    if(rowcost[row][col].sigsq<sigsqshortmin){
	      rowcost[row][col].sigsq=sigsqshortmin;
	    }
	    rowcost[row][col].dzmax=dzlay*ztoshort;
	    rowcost[row][col].laycost=rowweight[row][col]*glay;
	    if(rowcost[row][col].dzmax>floor(sqrt(rowcost[row][col].laycost
						  *rowcost[row][col].sigsq))){
	      nolayover=FALSE;
	    }
	  }
	  if(nolayover){
	    rowcost[row][col].sigsq=(sigsqrho+sigsqei)*ztoshortsq
	      /(costscale*rowweight[row][col]);
	    if(rowcost[row][col].sigsq<sigsqshortmin){
	      rowcost[row][col].sigsq=sigsqshortmin;
	    }
	    rowcost[row][col].laycost=NOCOSTSHELF;
	    rowcost[row][col].dzmax=LARGESHORT;
	  }

	}else{

	  /* deformation mode */
	  /* clip correlation because of estimator bias */
	  if(rho<defocorrthresh){
	    rho=0;
	  }
	  
	  /* variance due to decorrelation */
	  sigsqrho=(sigsqrhoconst*pow(1-rho,rhopow)+sigsqcorr)*nshortcyclesq;

	  /* set cost paramaters in terms of flow, represented as shorts */
	  if(rho>0){
	    rowcost[row][col].offset=nshortcycle*
	      (dpsi[row][col]-avgdpsi[row][col]);
	  }else{
	    rowcost[row][col].offset=nshortcycle*
	      (dpsi[row][col]-0.5*avgdpsi[row][col]);
	  }
	  rowcost[row][col].sigsq=sigsqrho/(costscale*rowweight[row][col]);
	  if(rowcost[row][col].sigsq<sigsqshortmin){
	    rowcost[row][col].sigsq=sigsqshortmin;
	  }
	  if(rho<defocorrthresh){
	    rowcost[row][col].dzmax=defomax;
	    rowcost[row][col].laycost=rowweight[row][col]*glay;
	    if(rowcost[row][col].dzmax<floor(sqrt(rowcost[row][col].laycost
						  *rowcost[row][col].sigsq))){
	      rowcost[row][col].laycost=NOCOSTSHELF;
	      rowcost[row][col].dzmax=LARGESHORT;
	    }
	  }else{
	    rowcost[row][col].laycost=NOCOSTSHELF;
	    rowcost[row][col].dzmax=LARGESHORT;
	  }
	}

	/* shift PDF to account for flattening by coarse unwrapped estimate */
	if(unwrappedest!=NULL){
	  rowcost[row][col].offset+=(nshortcycle/TWOPI*
				     (unwrappedest[row+1][col]
				      -unwrappedest[row][col]));
	}

      }
    }  /* end of azimuth gradient cost calculation */

    /* free temporary arrays */
    if(!params->defo){
      Free2DArray((void **)ei,nrow);
      free(dzrhomax);
    }
    Free2DArray((void **)corr,nrow);
    Free2DArray((void **)dpsi,nrow);
    Free2DArray((void **)avgdpsi,nrow);

    /* dump cost arrays */
    if(params->bperp>0){
      ((costT **)costs)[0][0].sigsq*=-1;
    }
    if(params->costoutfile!=NULL){
      Write2DRowColArray((void **)costs,params->costoutfile,
			nrow,ncol,sizeof(costT));
    }else{
      if(params->rowcostfile!=NULL){
	Write2DArray((void **)rowcost,params->rowcostfile,
		     nrow-1,ncol,sizeof(costT));
      }
      if(params->colcostfile!=NULL){
	Write2DArray((void **)colcost,params->colcostfile,
		     nrow,ncol-1,sizeof(costT));
      }
    }
    if(params->bperp>0){
      ((costT **)costs)[0][0].sigsq*=-1;
    }

  }/* end if(!params->nostatcosts) */


  /* now, set scalar costs for MST initialization or optimization if needed */
  if(params->nostatcosts){    

    /* if in no-statistical-costs mode, set scalar void **costs array */
    if(!params->initonly){
      for(row=0;row<2*nrow-1;row++){
	if(row<nrow-1){
	  maxcol=ncol;
	}else{
	  maxcol=ncol-1;
	}
	for(col=0;col<maxcol;col++){
	  ((short **)*costsptr)[row][col]=weights[row][col];
	}
      }
    }

    /* unless input is already unwrapped, use weights memory for mstcosts */
    if(!params->unwrapped){
      (*mstcostsptr)=weights;
    }else{
      Free2DArray((void **)weights,2*nrow-1);
      (*mstcostsptr)=NULL;
    }

  }else if(!params->unwrapped || params->p>=0){

    /* if we got here, we had statistical costs and we need scalar weights */
    /*   from them for MST initialization or for Lp optimization */
    for(row=0;row<2*nrow-1;row++){
      if(row<nrow-1){
	maxcol=ncol;
      }else{
	maxcol=ncol-1;
      }
      for(col=0;col<maxcol;col++){

	/* calculate incremental costs for flow=0, nflow=1 */
	/* maybe in Lp mode, so call CalcCostTopo or CalcCostDefo explicitly */
	if(params->defo){
	  CalcCostDefo((void **)costs,0,row,col,1,
		       nrow,params,&poscost,&negcost);
	}else{
	  CalcCostTopo((void **)costs,0,row,col,1,
		       nrow,params,&poscost,&negcost);
	}

	/* take smaller of positive and negative incremental cost */
	if(poscost<negcost){
	  tempcost=poscost;
	}else{
	  tempcost=negcost;
	}

	/* clip scalar cost so it is between 1 and params->maxcost */
	if(tempcost<params->maxcost){
	  if(tempcost>MINSCALARCOST){
	    weights[row][col]=tempcost;
	  }else{
	    weights[row][col]=MINSCALARCOST;
	  }
	}else{
	  weights[row][col]=params->maxcost;
	}
	if(params->p>=0){
	  ((short **)*costsptr)[row][col]=weights[row][col];
	}
      }
    }

    /* set costs for corner arcs to prevent ambiguous flows */
    colweight[0][0]=LARGESHORT;
    colweight[0][ncol-2]=LARGESHORT;
    colweight[nrow-1][0]=LARGESHORT;
    colweight[nrow-1][ncol-2]=LARGESHORT;
    if(params->p>=0){
      ((short **)*costsptr)[nrow-1][0]=LARGESHORT;
      ((short **)*costsptr)[nrow-1][ncol-2]=LARGESHORT;
      ((short **)*costsptr)[2*nrow-2][0]=LARGESHORT;
      ((short **)*costsptr)[2*nrow-2][ncol-2]=LARGESHORT;
    }
    
    /* dump mst initialization costs */
    if(params->mstrowcostfile!=NULL){
      Write2DArray((void **)rowweight,params->mstrowcostfile,
		   nrow-1,ncol,sizeof(short));
    }
    if(params->mstcolcostfile!=NULL){
      Write2DArray((void **)colweight,params->mstcolcostfile,
		   nrow,ncol-1,sizeof(short)); 
    }
    if(params->mstcostsfile!=NULL){
      Write2DRowColArray((void **)rowweight,params->mstcostsfile,
			 nrow,ncol,sizeof(short));
    }

    /* unless input is unwrapped, calculate initialization max flow */
    if(params->initmaxflow==AUTOCALCSTATMAX && !params->unwrapped){
      CalcInitMaxFlow(params,(void **)costs,nrow,ncol);
    }

    /* free costs memory if in init-only or Lp mode */
    if(params->initonly || params->p>=0){
      Free2DArray((void **)costs,2*nrow-1);
    }

    /* use memory allocated for weights arrays for mstcosts if needed */
    if(!params->unwrapped){
      (*mstcostsptr)=weights;
    }else{
      Free2DArray((void **)weights,2*nrow-1);
    }
 
  }else{
    Free2DArray((void **)weights,2*nrow-1);
  }

}


/* function: GetIntensityAndCorrelation()
 * --------------------------------------
 * Reads amplitude and correlation info from files if specified.  If ampfile
 * not given, uses interferogram magnitude.  If correlation file not given,
 * generates correlatin info from interferogram and amplitude.
 */
void GetIntensityAndCorrelation(float **mag, float **wrappedphase, 
				float ***pwrptr, float ***corrptr, 
				char *ampfile, char *ampfile2, 
				char *corrfile, long linelen, long nlines,
				long nrow, long ncol, paramT *params){

  long row, col, krowcorr, kcolcorr;
  float **pwr, **corr;
  float **realcomp, **imagcomp;
  float **padreal, **padimag, **avgreal, **avgimag;
  float **pwr1, **pwr2, **padpwr1, **padpwr2, **avgpwr1, **avgpwr2; 
  double rho0, rhomin, biaseddefaultcorr;
  signed char printwarning;

  /* initialize */
  pwr=NULL;
  corr=NULL;
  pwr1=NULL;
  pwr2=NULL;
  
  /* read intensity, if specified */
  if(strlen(ampfile)){
    ReadIntensity(&pwr,&pwr1,&pwr2,ampfile,ampfile2,linelen,nlines,params);
  }else{
    fprintf(sp1,"No brightness file specified.  ");
    fprintf(sp1,"Using interferogram magnitude as intensity\n");
    pwr=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	pwr[row][col]=mag[row][col];
      }
    }
  }

  /* read corrfile, if specified */
  if(strlen(corrfile)){
    ReadCorrelation(&corr,corrfile,linelen,nlines,params); 
  }else if(pwr1!=NULL && pwr2!=NULL && params->havemagnitude){

    /* generate the correlation info from the interferogram and amplitude */
    fprintf(sp1,"Generating correlation from interferogram and intensity\n");

    /* get the correct number of looks, and make sure its odd */
    krowcorr=1+2*floor(params->ncorrlooksaz/(double )params->nlooksaz/2);
    kcolcorr=1+2*floor(params->ncorrlooksrange/(double )params->nlooksrange/2);

    /* calculate equivalent number of independent looks */
    params->nlooks=(kcolcorr*params->nlooksaz*krowcorr*params->nlooksrange)
      *(params->dr/params->nlooksrange/params->rangeres)
      *(params->da/params->nlooksaz/params->azres);
    fprintf(sp1,"   (%ld equivalent independent looks)\n",
	    params->nlooks);
    
    /* get real and imaginary parts of interferogram */
    realcomp=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    imagcomp=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	realcomp[row][col]=mag[row][col]*cos(wrappedphase[row][col]);
	imagcomp[row][col]=mag[row][col]*sin(wrappedphase[row][col]);
      }
    }

    /* do complex spatial averaging on the interferogram */
    padreal=MirrorPad(realcomp,nrow,ncol,(krowcorr-1)/2,(kcolcorr-1)/2);
    avgreal=realcomp;
    BoxCarAvg(avgreal,padreal,nrow,ncol,krowcorr,kcolcorr);
    padimag=MirrorPad(imagcomp,nrow,ncol,(krowcorr-1)/2,(kcolcorr-1)/2);
    avgimag=imagcomp;
    BoxCarAvg(avgimag,padimag,nrow,ncol,krowcorr,kcolcorr);
    Free2DArray((void **)padreal,nrow);
    Free2DArray((void **)padimag,nrow);

    /* spatially average individual SAR power images */
    padpwr1=MirrorPad(pwr1,nrow,ncol,(krowcorr-1)/2,(kcolcorr-1)/2);
    avgpwr1=pwr1;
    BoxCarAvg(avgpwr1,padpwr1,nrow,ncol,krowcorr,kcolcorr);
    padpwr2=MirrorPad(pwr2,nrow,ncol,(krowcorr-1)/2,(kcolcorr-1)/2);
    avgpwr2=pwr2;
    BoxCarAvg(avgpwr2,padpwr2,nrow,ncol,krowcorr,kcolcorr);
    Free2DArray((void **)padpwr1,nrow);
    Free2DArray((void **)padpwr2,nrow);

    /* build correlation data */
    corr=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	if(avgpwr1[row][col]<=0 || avgpwr2[row][col]<=0){
	  corr[row][col]=0.0;
	}else{
	  corr[row][col]=sqrt((avgreal[row][col]*avgreal[row][col]
			       +avgimag[row][col]*avgimag[row][col])
			      /(avgpwr1[row][col]*avgpwr2[row][col]));
	}
      }
    }

    /* free temporary arrays */
    Free2DArray((void **)avgreal,nrow);
    Free2DArray((void **)avgimag,nrow);
    Free2DArray((void **)avgpwr1,nrow);
    Free2DArray((void **)avgpwr2,nrow);
    pwr1=NULL;
    pwr2=NULL;

  }else{

    /* no file specified: set corr to default value */
    /* find biased default correlation using */
    /* inverse of unbias method used by BuildCostArrays() */
    corr=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    fprintf(sp1,"No correlation file specified.  Assuming correlation = %g\n",
	   params->defaultcorr);
    rho0=(params->rhosconst1)/(params->nlooks)+(params->rhosconst2);
    rhomin=params->rhominfactor*rho0;
    if(params->defaultcorr>rhomin){
      biaseddefaultcorr=params->defaultcorr;
    }else{
      biaseddefaultcorr=0.0;
    }
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	corr[row][col]=biaseddefaultcorr;
      }
    }
  }

  /* check correlation data validity */
  printwarning=FALSE;
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      if(!IsFinite(corr[row][col])){
	fprintf(sp0,"ERROR: NaN or infinity found in correlation data\n");
	exit(ABNORMAL_EXIT);
      }else if(corr[row][col]>1.0){
	if(corr[row][col]>1.01){
	  printwarning=TRUE;        /* don't warn for minor numerical erros */
	}
	corr[row][col]=1.0;
      }else if(corr[row][col]<0.0){
	if(corr[row][col]<-0.01){
	  printwarning=TRUE;        /* don't warn for minor numerical erros */ 
	}
	corr[row][col]=0.0;
      }
    }
  }
  if(printwarning){
    fprintf(sp0,"WARNING: illegal correlation values clipped to [0,1]\n");
  }
  
  /* dump correlation data if necessary */
  if(params->corrdumpfile!=NULL){
    Write2DArray((void **)corr,params->corrdumpfile,nrow,ncol,sizeof(float)); 
  }

  /* free memory and set output pointers */
  if(pwr1!=NULL){
    Free2DArray((void **)pwr1,nrow);
  }
  if(pwr2!=NULL){
    Free2DArray((void **)pwr2,nrow);
  }
  if(params->defo && pwr!=NULL){
    Free2DArray((void **)pwr,nrow);
    pwr=NULL;
  }
  *pwrptr=pwr;
  *corrptr=corr;

}


/* function: RemoveMean()
 * -------------------------
 * Divides intensity by average over sliding window.
 */
void RemoveMean(float **ei, long nrow, long ncol, 
		       long krowei, long kcolei){

  float **avgei, **padei;
  long row, col;

  /* make sure krowei, kcolei are odd */
  if(!(krowei % 2)){
    krowei++;
  }
  if(!(kcolei % 2)){
    kcolei++;
  }

  /* get memory */
  avgei=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));

  /* pad ei in new array */
  padei=MirrorPad(ei,nrow,ncol,(krowei-1)/2,(kcolei-1)/2);

  /* calculate average ei by using sliding window */
  BoxCarAvg(avgei,padei,nrow,ncol,krowei,kcolei);

  /* divide ei by avgei */
  for(row=0;row<nrow;row++){
    for(col=0;col<ncol;col++){
      if(avgei){
	ei[row][col]/=(avgei[row][col]);
      }
    }
  }

  /* free memory */
  Free2DArray((void **)padei,nrow+krowei-1);
  Free2DArray((void **)avgei,nrow);

}


/* function: SolveDZRCrit()
 * ------------------------
 * Numerically solve for the transition point of the linearized scattering 
 * model.
 */
double SolveDZRCrit(paramT *params, double threshold){

  double residual, thetai, kds, n, dr, dzr, dx;
  double costhetai, cos2thetai, sintheta, costheta, step;
  double dzrcritfactor, diffuse, specular;
  long i;

  /* get parameters */
  kds=params->kds;
  n=params->n;
  dr=params->dr;  
  sintheta=sin(params->theta);
  costheta=cos(params->theta);
  dzrcritfactor=params->dzrcritfactor;

  /* solve for critical incidence angle */
  thetai=PI/4;
  step=PI/4-1e-6;
  i=0;
  while(TRUE){
    if((cos2thetai=cos(2*thetai))<0){
      cos2thetai=0;
    }
    diffuse=dzrcritfactor*kds*cos(thetai);
    specular=pow(cos2thetai,n);
    if(fabs(residual=diffuse-specular)<threshold*diffuse){
      break;
    }
    if(residual<0){
      thetai+=step;
    }else{
      thetai-=step;
    }
    step/=2.0;
    if(++i>MAXITERATION){
      fprintf(sp0,"couldn't find critical incidence angle\n");
      fprintf(sp0,"(check scattering parameters)\n");
      exit(ABNORMAL_EXIT);
    }
  }

  /* solve for critical height change */
  costhetai=cos(thetai);
  dzr=params->initdzr;
  step=dzr+dr*costheta-1e-2;
  i=0;
  while(TRUE){
    dx=(dr+dzr*costheta)/sintheta;
    if(fabs(residual=costhetai-(dzr*sintheta+dx*costheta)/sqrt(dzr*dzr+dx*dx))
       <threshold*costhetai){
      return(dzr);
    }
    if(residual<0){
      dzr-=step;
    }else{
      dzr+=step;
    }
    step/=2.0;
    if(++i>MAXITERATION){
      fprintf(sp0,"couldn't find critical slope\n");
      fprintf(sp0,"(check geometry parameters)\n");
      exit(ABNORMAL_EXIT);
    }
  }
}


/* function: SolveEIModelParams()
 * ------------------------------
 * Calculates parameters for linearized model of EI vs. range slope
 * relationship.
 */
void SolveEIModelParams(double *slope1ptr, double *slope2ptr, 
			       double *const1ptr, double *const2ptr, 
			       double dzrcrit, paramT *params){

  double slope1, slope2, const1, const2, sloperatio, dzr0;
  double dzr3, ei3;
  
  /* set up */
  sloperatio=params->kds*params->sloperatiofactor;
  dzr0=-(params->dr)*(cos(params->theta));

  /* find normalized intensity at 15(dzrcrit-dzr0)+dzr0 */
  dzr3=15.0*(dzrcrit-dzr0)+dzr0;
  ei3=EIofDZR(dzr3,params)/EIofDZR(0,params);

  /* calculate parameters */
  const1=dzr0;
  slope2=(sloperatio*(dzrcrit-const1)-dzrcrit+dzr3)/ei3;
  slope1=slope2/sloperatio;
  const2=dzr3-slope2*ei3;

  /* set return values */
  *slope1ptr=slope1;
  *slope2ptr=slope2;
  *const1ptr=const1;
  *const2ptr=const2;

}


/* function: EIofDZR()
 * -------------------
 * Calculates expected value of intensity with arbitrary units for given
 * parameters.  Assumes azimuth slope is zero.
 */
double EIofDZR(double dzr, paramT *params){

  double theta, dr, da, dx, kds, n, dzr0, projarea;
  double costhetai, cos2thetai, sigma0;

  theta=params->theta;
  dr=params->dr;
  da=params->da;
  dx=dr/sin(theta)+dzr/tan(theta);
  kds=params->kds;
  n=params->n;
  dzr0=-dr*cos(theta);
  projarea=da*fabs((dzr-dzr0)/sin(theta));
  costhetai=projarea/sqrt(dzr*dzr*da*da + da*da*dx*dx);
  if(costhetai>SQRTHALF){
    cos2thetai=2*costhetai*costhetai-1;
    sigma0=kds*costhetai+pow(cos2thetai,n);
  }else{
    sigma0=kds*costhetai;
  }
  return(sigma0*projarea);

}


/* function: CalcDzRhoMax()
 * ------------------------
 * Calculates the maximum slope (in range) for the given unbiased correlation
 * using spatial decorrelation as an upper limit (Zebker & Villasenor,
 * 1992).
 */
double CalcDzRhoMax(double rho, paramT *params, double threshold){

  long i;
  double dx, dr, dz, dzstep, rhos, sintheta, costheta, numerator;
  double costhetairsq, rhosfactor, residual;


  /* set up */
  i=0;
  dr=params->dr;
  costheta=cos(params->theta);
  sintheta=sin(params->theta);
  dzstep=params->initdzstep;
  rhosfactor=2.0*fabs(params->bperp)*(params->rangeres)
    /((params->lambda)*(params->nomrange));

  /* take care of the extremes */
  if(rho>=1.0){
    return(-dr*costheta);
  }else if(rho<=0){
    return(LARGEFLOAT);
  }

  /* start with slope for unity correlation, step slope upwards */
  dz=-dr*costheta;
  rhos=1.0;
  while(rhos>rho){
    dz+=dzstep;
    dx=(dr+dz*costheta)/sintheta;
    numerator=dz*sintheta+dx*costheta;
    costhetairsq=numerator*numerator/(dz*dz+dx*dx);
    rhos=1-rhosfactor*sqrt(costhetairsq/(1-costhetairsq));
    if(rhos<0){
      rhos=0;
    }
    if(dz>BIGGESTDZRHOMAX){
      return(BIGGESTDZRHOMAX);
    }
  }

  /* now iteratively decrease step size and narrow in on correct slope */
  while(fabs(residual=rhos-rho)>threshold*rho){
    dzstep/=2.0;
    if(residual<0){
      dz-=dzstep;
    }else{
      dz+=dzstep;
    }
    dx=(dr+dz*costheta)/sintheta;
    numerator=dz*sintheta+dx*costheta;
    costhetairsq=numerator*numerator/(dz*dz+dx*dx);
    rhos=1-rhosfactor*sqrt(costhetairsq/(1-costhetairsq));
    if(rhos<0){
      rhos=0;
    }
    if(++i>MAXITERATION){
      fprintf(sp0,"ERROR: couldn't find slope for correlation of %f\n",rho);
      fprintf(sp0,"check geometry and spatial decorrelation parameters\n");
      exit(ABNORMAL_EXIT);
    }
  }

  return(dz);
}


/* function: CalcCostTopo()
 * ------------------------
 * Calculates topography arc distance given an array of cost data structures.
 */
void CalcCostTopo(void **costs, long flow, long arcrow, long arccol, 
			 long nflow, long nrow, paramT *params, 
			 long *poscostptr, long *negcostptr){

  long idz1, idz2pos, idz2neg, cost1, nflowsq, poscost, negcost;
  costT *cost;


  /* get arc info */
  cost=&((costT **)(costs))[arcrow][arccol];
  if(arcrow<nrow-1){

    /* row cost: dz symmetric with respect to origin */
    idz1=labs(flow*(params->nshortcycle)+cost->offset);
    idz2pos=labs((flow+nflow)*(params->nshortcycle)
		 +cost->offset);
    idz2neg=labs((flow-nflow)*(params->nshortcycle)
		 +cost->offset);

  }else{

    /* column cost: non-symmetric dz */
    idz1=flow*(params->nshortcycle)+cost->offset;
    idz2pos=(flow+nflow)*(params->nshortcycle)+cost->offset;
    idz2neg=(flow-nflow)*(params->nshortcycle)+cost->offset;

  }

  /* calculate cost1 */
  if(idz1>cost->dzmax){
    idz1-=cost->dzmax;
    cost1=(idz1*idz1)/((params->layfalloffconst)*(cost->sigsq))+cost->laycost; 
  }else{
    cost1=(idz1*idz1)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && idz1>0 && cost1>cost->laycost){
      cost1=cost->laycost;
    }
  }

  /* calculate positive cost increment */
  if(idz2pos>cost->dzmax){
    idz2pos-=cost->dzmax;
    poscost=(idz2pos*idz2pos)/((params->layfalloffconst)*(cost->sigsq))
      +cost->laycost-cost1;
  }else{
    poscost=(idz2pos*idz2pos)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && idz2pos>0 && poscost>cost->laycost){
      poscost=cost->laycost-cost1;
    }else{
      poscost-=cost1;
    }
  }

  /* calculate negative cost increment */
  if(idz2neg>cost->dzmax){
    idz2neg-=cost->dzmax;
    negcost=(idz2neg*idz2neg)/((params->layfalloffconst)*(cost->sigsq))
      +cost->laycost-cost1;
  }else{
    negcost=(idz2neg*idz2neg)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && idz2neg>0 && negcost>cost->laycost){
      negcost=cost->laycost-cost1;
    }else{
      negcost-=cost1;
    }
  }

  /* scale costs for this nflow */
  nflowsq=nflow*nflow;
  if(poscost>0){
    *poscostptr=(long )ceil((float )poscost/nflowsq);
  }else{
    *poscostptr=(long )floor((float )poscost/nflowsq);
  }
  if(negcost>0){
    *negcostptr=(long )ceil((float )negcost/nflowsq);
  }else{
    *negcostptr=(long )floor((float )negcost/nflowsq);
  }

}


/* function: CalcCostDefo()
 * ------------------------
 * Calculates deformation arc distance given an array of cost data structures.
 */
void CalcCostDefo(void **costs, long flow, long arcrow, long arccol, 
			 long nflow, long nrow, paramT *params, 
			 long *poscostptr, long *negcostptr){

  long idz1, idz2pos, idz2neg, cost1, nflowsq, poscost, negcost;
  costT *cost;


  /* get arc info */
  cost=&((costT **)(costs))[arcrow][arccol];
  idz1=labs(flow*(params->nshortcycle)+cost->offset);
  idz2pos=labs((flow+nflow)*(params->nshortcycle)
	       +cost->offset);
  idz2neg=labs((flow-nflow)*(params->nshortcycle)
	       +cost->offset);

  /* calculate cost1 */
  if(idz1>cost->dzmax){
    idz1-=cost->dzmax;
    cost1=(idz1*idz1)/((params->layfalloffconst)*(cost->sigsq))+cost->laycost; 
  }else{
    cost1=(idz1*idz1)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && cost1>cost->laycost){
      cost1=cost->laycost;
    }
  }

  /* calculate positive cost increment */
  if(idz2pos>cost->dzmax){
    idz2pos-=cost->dzmax;
    poscost=(idz2pos*idz2pos)/((params->layfalloffconst)*(cost->sigsq))
      +cost->laycost-cost1;
  }else{
    poscost=(idz2pos*idz2pos)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && poscost>cost->laycost){
      poscost=cost->laycost-cost1;
    }else{
      poscost-=cost1;
    }
  }

  /* calculate negative cost increment */
  if(idz2neg>cost->dzmax){
    idz2neg-=cost->dzmax;
    negcost=(idz2neg*idz2neg)/((params->layfalloffconst)*(cost->sigsq))
      +cost->laycost-cost1;
  }else{
    negcost=(idz2neg*idz2neg)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && negcost>cost->laycost){
      negcost=cost->laycost-cost1;
    }else{
      negcost-=cost1;
    }
  }

  /* scale costs for this nflow */
  nflowsq=nflow*nflow;
  if(poscost>0){
    *poscostptr=(long )ceil((float )poscost/nflowsq);
  }else{
    *poscostptr=(long )floor((float )poscost/nflowsq);
  }
  if(negcost>0){
    *negcostptr=(long )ceil((float )negcost/nflowsq);
  }else{
    *negcostptr=(long )floor((float )negcost/nflowsq);
  }

}


/* function: CalcCostL0()
 * ----------------------
 * Calculates the L0 arc distance given an array of cost data structures.
 */
void CalcCostL0(void **costs, long flow, long arcrow, long arccol, 
		       long nflow, long nrow, paramT *params, 
		       long *poscostptr, long *negcostptr){

  /* L0-norm */
  if(flow){
    if(flow+nflow){
      *poscostptr=0;
    }else{
      *poscostptr=-((short **)costs)[arcrow][arccol];
    }
    if(flow-nflow){
      *negcostptr=0;
    }else{
      *negcostptr=-((short **)costs)[arcrow][arccol];
    }
  }else{
    *poscostptr=((short **)costs)[arcrow][arccol];
    *negcostptr=((short **)costs)[arcrow][arccol];
  }
}


/* function: CalcCostL1()
 * ----------------------
 * Calculates the L1 arc distance given an array of cost data structures.
 */
void CalcCostL1(void **costs, long flow, long arcrow, long arccol, 
		       long nflow, long nrow, paramT *params, 
		       long *poscostptr, long *negcostptr){

  /* L1-norm */
  *poscostptr=((short **)costs)[arcrow][arccol]*(labs(flow+nflow)-labs(flow));
  *negcostptr=((short **)costs)[arcrow][arccol]*(labs(flow-nflow)-labs(flow));

}


/* function: CalcCostL2()
 * ----------------------
 * Calculates the L2 arc distance given an array of cost data structures.
 */
void CalcCostL2(void **costs, long flow, long arcrow, long arccol, 
		       long nflow, long nrow, paramT *params, 
		       long *poscostptr, long *negcostptr){

  long flow2, flowsq;

  /* L2-norm */
  flowsq=flow*flow;
  flow2=flow+nflow;
  *poscostptr=((short **)costs)[arcrow][arccol]*(flow2*flow2-flowsq);
  flow2=flow-nflow;
  *negcostptr=((short **)costs)[arcrow][arccol]*(flow2*flow2-flowsq);
}


/* function: CalcCostLP()
 * ----------------------
 * Calculates the Lp arc distance given an array of cost data structures.
 */
void CalcCostLP(void **costs, long flow, long arcrow, long arccol, 
		       long nflow, long nrow, paramT *params, 
		       long *poscostptr, long *negcostptr){

  short flow2;

  /* Lp-norm */
  flow2=flow+nflow;
  *poscostptr=FRound(((short **)costs)[arcrow][arccol]*
		 (pow(labs(flow2),params->p)-pow(labs(flow),params->p)));
  flow2=flow-nflow;
  *negcostptr=FRound(((short **)costs)[arcrow][arccol]*
		 (pow(labs(flow2),params->p)-pow(labs(flow),params->p)));
}


/* function: EvalCostTopo()
 * ------------------------
 * Calculates topography arc cost given an array of cost data structures.
 */
long EvalCostTopo(void **costs, short **flows, long arcrow, long arccol,
			 long nrow, paramT *params){

  long idz1, cost1;
  costT *cost;

  /* get arc info */
  cost=&((costT **)(costs))[arcrow][arccol];
  if(arcrow<nrow-1){

    /* row cost: dz symmetric with respect to origin */
    idz1=labs(flows[arcrow][arccol]*(params->nshortcycle)+cost->offset);

  }else{

    /* column cost: non-symmetric dz */
    idz1=flows[arcrow][arccol]*(params->nshortcycle)+cost->offset;

  }

  /* calculate and return cost */
  if(idz1>cost->dzmax){
    idz1-=cost->dzmax;
    cost1=(idz1*idz1)/((params->layfalloffconst)*(cost->sigsq))+cost->laycost;
  }else{
    cost1=(idz1*idz1)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && idz1>0 && cost1>cost->laycost){
      cost1=cost->laycost;
    }
  }
  return(cost1);
}


/* function: EvalCostDefo()
 * ------------------------
 * Calculates deformation arc cost given an array of cost data structures.
 */
long EvalCostDefo(void **costs, short **flows, long arcrow, long arccol,
			 long nrow, paramT *params){

  long idz1, cost1;
  costT *cost;

  /* get arc info */
  cost=&((costT **)(costs))[arcrow][arccol];
  idz1=labs(flows[arcrow][arccol]*(params->nshortcycle)+cost->offset);

  /* calculate cost1 */
  if(idz1>cost->dzmax){
    idz1-=cost->dzmax;
    cost1=(idz1*idz1)/((params->layfalloffconst)*(cost->sigsq))+cost->laycost; 
  }else{
    cost1=(idz1*idz1)/cost->sigsq;
    if(cost->laycost!=NOCOSTSHELF && cost1>cost->laycost){
      cost1=cost->laycost;
    }
  }
  return(cost1);
}


/* function: EvalCostL0()
 * ----------------------
 * Calculates the L0 arc cost given an array of cost data structures.
 */
long EvalCostL0(void **costs, short **flows, long arcrow, long arccol, 
		       long nrow, paramT *params){

  /* L0-norm */
  if(flows[arcrow][arccol]){
    return((long)((short **)costs)[arcrow][arccol]);
  }else{
    return(0);
  }
}


/* function: EvalCostL1()
 * ----------------------
 * Calculates the L1 arc cost given an array of cost data structures.
 */
long EvalCostL1(void **costs, short **flows, long arcrow, long arccol, 
		       long nrow, paramT *params){

  /* L1-norm */
  return( (((short **)costs)[arcrow][arccol]) * labs(flows[arcrow][arccol]) );
}


/* function: EvalCostL2()
 * ----------------------
 * Calculates the L2 arc cost given an array of cost data structures.
 */
long EvalCostL2(void **costs, short **flows, long arcrow, long arccol, 
		       long nrow, paramT *params){

  /* L2-norm */
  return( (((short **)costs)[arcrow][arccol]) * 
	  (flows[arcrow][arccol]*flows[arcrow][arccol]) );
}


/* function: EvalCostLP()
 * ----------------------
 * Calculates the Lp arc cost given an array of cost data structures.
 */
long EvalCostLP(void **costs, short **flows, long arcrow, long arccol, 
		       long nrow, paramT *params){

  /* L2-norm */
  return( (((short **)costs)[arcrow][arccol]) * 
	  pow(labs(flows[arcrow][arccol]),params->p) );
}


/* function: CalcInitMaxFlow()
 * ---------------------------
 * Calculates the maximum flow magnitude to allow in the initialization
 * by examining the dzmax members of arc statistical cost data structures.
 */
void CalcInitMaxFlow(paramT *params, void **costs, 
			    long nrow, long ncol){

  long row, col, maxcol, initmaxflow, arcmaxflow;

  if(params->initmaxflow<=0){
    if(params->nostatcosts){
      params->initmaxflow=NOSTATINITMAXFLOW;
    }else{
      initmaxflow=0;
      for(row=0;row<2*nrow-1;row++){
	if(row<nrow-1){
	  maxcol=ncol;
	}else{
	  maxcol=ncol-1;
	}
	for(col=0;col<maxcol;col++){
	  if(((costT **)costs)[row][col].dzmax!=LARGESHORT){
	    arcmaxflow=ceil(((costT **)costs)[row][col].dzmax/
			    (double )(params->nshortcycle)
			    +params->arcmaxflowconst);
	    if(arcmaxflow>initmaxflow){
	      initmaxflow=arcmaxflow;
	    }
	  }
	}
      }
      params->initmaxflow=initmaxflow;
    }
  }
}

