#include "asf.h"
#include "ardop_params.h"

struct INPUT_ARDOP_PARAMS *get_input_ardop_params_struct(char *in1, char *out)
{
    struct INPUT_ARDOP_PARAMS *ret;
    ret = MALLOC(sizeof(struct INPUT_ARDOP_PARAMS));

    strcpy(ret->in1, in1);
    strcpy(ret->out, out);
    strcpy(ret->CALPRMS, "NO");

    ret->pwrFlag = NULL;
    ret->sigmaFlag = NULL;
    ret->gammaFlag = NULL;
    ret->betaFlag = NULL;
    ret->hamFlag = NULL;
    ret->kaiFlag = NULL;
    ret->ifirstline = NULL;
    ret->npatches = NULL;
    ret->isave = NULL;
    ret->ifirst = NULL;
    ret->nla = NULL;
    ret->azres = NULL;
    ret->deskew = NULL;
    ret->na_valid = NULL;
    ret->sloper = NULL;
    ret->interr = NULL;
    ret->slopea = NULL;
    ret->intera = NULL;
    ret->dsloper = NULL;
    ret->dinterr = NULL;
    ret->dslopea = NULL;
    ret->dintera = NULL;
    ret->fd = NULL;
    ret->fdd = NULL;
    ret->fddd = NULL;
    ret->iflag = NULL;

    return ret;
}

void apply_in_ardop_params_to_ardop_params(struct INPUT_ARDOP_PARAMS *in,
                                           struct ARDOP_PARAMS *a)
{
    strcpy(a->in1, in->in1);
    strcpy(a->out, in->out);

    if (in->CALPRMS)
        strcpy(a->CALPRMS, in->CALPRMS);

#define ApplyField(field) if (in->field) a->field = *(in->field);

    ApplyField(pwrFlag);
    ApplyField(sigmaFlag);
    ApplyField(gammaFlag);
    ApplyField(betaFlag);
    ApplyField(hamFlag);
    ApplyField(kaiFlag);
    ApplyField(ifirstline);
    ApplyField(npatches);
    ApplyField(isave);
    ApplyField(ifirst);
    ApplyField(nla);
    ApplyField(azres);
    ApplyField(deskew);
    ApplyField(na_valid);
    ApplyField(sloper);
    ApplyField(interr);
    ApplyField(slopea);
    ApplyField(intera);
    ApplyField(dsloper);
    ApplyField(dinterr);
    ApplyField(dslopea);
    ApplyField(dintera);
    ApplyField(fd);
    ApplyField(fdd);
    ApplyField(fddd);
    ApplyField(iflag);

#undef ApplyField
}

void print_params(const char *in,struct ARDOP_PARAMS *a,const char *sourceProgram)
{
 FILE *fp; char out[256];

 create_name(out,in,".in"); 
 fp = FOPEN(out,"w");
 
 fprintf(fp,"ARDOP2.5 SAR Processing Parameter File (%s)\n",sourceProgram);
 fprintf(fp,"%i \t\t\t\t\t! Debug Flag                 \n", a->iflag);
 fprintf(fp,"%i \t\t\t\t\t! First line (from 0)        \n", a->ifirstline);
 fprintf(fp,"%i \t\t\t\t\t! Number of patches	       \n", a->npatches);
 fprintf(fp,"%i \t\t\t\t\t! i,q byte samples to skip   \n", a->ifirst);
 fprintf(fp,"%i \t\t\t\t\t! Output lines per patch     \n", a->na_valid);
 fprintf(fp,"%i \t\t\t\t\t! Deskew flag		       \n", a->deskew);
 fprintf(fp,"%i %i \t\t\t\t\t! 1st range sample, num samples \n",a->isave,a->nla);
 fprintf(fp,"%.8f %.10f %.10f\t\t\t! Dopp quad coefs(Hz/prf)\n",
         a->fd,a->fdd,a->fddd);
 fprintf(fp,"%9.1f \t\t\t\t! Earth Radius              \n",a->re);
 fprintf(fp,"%8.3f \t\t\t\t! Body fixed S/C velocity(m/s)\n",a->vel);
 fprintf(fp,"%8.3f \t\t\t\t! Spacecraft Height         \n",a->ht);
 fprintf(fp,"%8.3f \t\t\t\t! Range of first sample     \n",a->r00);
 fprintf(fp,"%8.3f \t\t\t\t! Pulse Repitition Freq.    \n",a->prf);
 fprintf(fp,"%f \t\t\t\t! Single look az. res.         \n", a->azres );
 fprintf(fp,"%i \t\t\t\t\t! Number of azimuth looks     \n", a->nlooks);
 fprintf(fp,"%E \t\t\t\t! Range sampling frequency (Hz) \n", a->fs);
 fprintf(fp,"%E \t\t\t\t! Chirp Slope (Hz/s)            \n", a->slope);
 fprintf(fp,"%E \t\t\t\t! Pulse Length (s)              \n", a->pulsedur);
 fprintf(fp,"%3.1f \t\t\t\t\t! Chirp extension         \n", a->nextend);
 fprintf(fp,"%f \t\t\t\t! Radar wavelength             \n", a->wavl);
 fprintf(fp,"%f \t\t\t\t! Range spectrum weight        \n", a->rhww);
 fprintf(fp,"%f %f \t\t\t! Bandwidth fractional trunc.\n", a->pctbw,a->pctbwaz);
 fprintf(fp,"%f %f %f %f     ! First patch slope,inter range,az\n",
 	 a->sloper, a->interr, a->slopea, a->intera);
 fprintf(fp,"%g %g %g %g     ! Delta per patch slope,inter range,az\n",
 	 a->dsloper, a->dinterr, a->dslopea, a->dintera);

 FCLOSE(fp);
 return;
}

/******************************************************************************
NAME:	     read_params - Creates input file for the ASF SAR processor
SYNOPSIS:    read_params(char *pfile,struct ARDOP_PARAMS *a)
DESCRIPTION: This program fills all of the values in asp_global.h by
             reading an input parameter file.
EXTERNAL ASSOCIATES: NONE
FILE REFERENCES: pfile - parameter file
PROGRAM HISTORY: 1.0    11/96  T. Logan		New Code for SAR Processor
******************************************************************************/
void err(void); 
void err(void) { printf("Unexpected end-of-file, while reading ARDOP parameter file.\n\n"); exit(1);}

#define FILL(A,B,C)	if (fgets((A),(B),(C))==NULL) err()


void read_params(const char *pfile,struct ARDOP_PARAMS *gbla)
{
  char versionStr[255];
  float version=1.0;
  char buf[255];
  FILE *fp;

/*Open Parameter file.*/
  create_name(buf,pfile,".in");
  fp = FOPEN(buf,"r");
  
/*Determine file version.*/
  FILL(buf,255,fp); sscanf(buf,"%s", versionStr);
  if (0==strncmp("ARDOP",versionStr,5))
  {
  	sscanf(versionStr,"ARDOP%f",&version);
/*  	if (!quietflag) printf("   Parsing ARDOP input file, version %.2f\n",version);*/
  }
  else if (0==strncmp("AISP",versionStr,4))    // handle old-style .in files
  {
    sscanf(versionStr,"AISP%f", &version);
/*  	if (!quietflag) printf("   Parsing ARDOP input file, version %.2f\n",version);*/
  }

  FILL(buf,255,fp);

/*Read in parameters.*/
  if (version<2.2)
  {/*File has encoded input and output names*/
	  sscanf(buf,"%s",gbla->in1);
	  FILL(buf,255,fp); sscanf(buf,"%s\n", gbla->out);
	  FILL(buf,255,fp); 
  }
  sscanf(buf,"%i\n", &gbla->iflag);
  
  if (version<2.0)
  {
  	FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->nbytes);
  	FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->ngood);
  }
  
  FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->ifirstline);
  FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->npatches);
  FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->ifirst);
  FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->na_valid);
  FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->deskew);
  
  if (version<2.0)
  {
  	FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->caltone);
  }
  
  FILL(buf,255,fp); sscanf(buf,"%i %i\n", &gbla->isave,&gbla->nla);
  FILL(buf,255,fp); sscanf(buf,"%f %f %f\n", &gbla->fd,&gbla->fdd,&gbla->fddd);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->re);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->vel);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->ht);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->r00);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->prf);
  
  if (version<2.0)
  {
  	FILL(buf,255,fp); sscanf(buf,"%f %f\n", &gbla->xmi,&gbla->xmq);
  	FILL(buf,255,fp); sscanf(buf,"%s\n", gbla->iqflip);
  }
  
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->azres);
  FILL(buf,255,fp); sscanf(buf,"%i\n", &gbla->nlooks);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->fs);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->slope);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->pulsedur);
  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->nextend);
  
  FILL(buf,255,fp);
  if (strstr(buf,"Secondary")!=NULL) /*Skip over secondary range migration section.*/
  	{ FILL(buf,255,fp); }
  sscanf(buf,"%f\n", &gbla->wavl);

  FILL(buf,255,fp); sscanf(buf,"%f\n", &gbla->rhww);
  FILL(buf,255,fp); sscanf(buf,"%f %f\n", &gbla->pctbw,&gbla->pctbwaz);
  FILL(buf,255,fp); sscanf(buf,"%f %f %f %f\n",
	 &gbla->sloper, &gbla->interr, &gbla->slopea, &gbla->intera);
  FILL(buf,255,fp); sscanf(buf,"%g %g %g %g\n",
	 &gbla->dsloper, &gbla->dinterr, &gbla->dslopea, &gbla->dintera);
  
  FCLOSE(fp);
  return;
}
