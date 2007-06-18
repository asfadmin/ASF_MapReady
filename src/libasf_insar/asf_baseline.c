#include "asf.h"
#include "asf_meta.h"
#include "asf_insar.h"

#define size 4
#define gridRes 100
#define boxRes 15
#define boxFrac ((int)(boxRes*boxRes*0.7))
#define VERSION 1.3

typedef struct {
  int x,y;
  float height;
  float slopeErr;
} seedRec;

seedRec *seeds[gridRes*gridRes];
int numSeeds=0;
seedRec *boxes[boxRes*boxRes];
int numBoxes=0;
int sortDirection=1;

static int qsorter(const seedRec **a,const seedRec **b);
static int qsorter(const seedRec **a,const seedRec **b)
{
  if (a[0]->slopeErr-b[0]->slopeErr<0)
    return -1*sortDirection;
  if (a[0]->slopeErr-b[0]->slopeErr>0)
    return 1*sortDirection;
  return 0;
}

static int dem2seeds(char *demFile, char *ampFile, char *seedsFile, int fft)
{
  int xCount, yCount, x, y, seedNo, line_count, sample_count;
  float *prev, *line, *aftr;
  FILE *fpin, *fpout;
  meta_parameters *metaIn, *metaAmp;

  // Open files and allocate memory.
  metaIn = meta_read(demFile);
  line_count = metaIn->general->line_count;
  sample_count = metaIn->general->sample_count;
  metaAmp = meta_read(ampFile);

  fpout = FOPEN(seedsFile,"w");
  fpin = fopenImage(demFile,"rb");
  line = (float *)MALLOC(sizeof(float)*sample_count);
  prev = (float *)MALLOC(sizeof(float)*sample_count);
  aftr = (float *)MALLOC(sizeof(float)*sample_count);
  // Loop through a gridRes x gridRes grid, creating a list of potential 
  // seed points & their errors.*/
  printf("Searching DEM for potential seed point locations...\n\n");
  for (yCount=0;yCount<gridRes;yCount++)
    {
      y=size+((float)(yCount))/(gridRes-1)*(line_count-1-2*size);
      get_float_line(fpin, metaIn, y, line);
      get_float_line(fpin, metaIn, y-size, prev);
      get_float_line(fpin, metaIn, y+size, aftr);
      for (xCount=0;xCount<gridRes;xCount++)
        {
          float cur,slopeErr;
          x=size+((float)(xCount))/(gridRes-1)*(sample_count-1-2*size);
          cur=line[x];
          if (cur>0.0) /*If the point is valid, add it to the seed list.*/
            {
              seedRec *seed=MALLOC(sizeof(seedRec));
              slopeErr=
                (fabs(prev[x-size]-cur) + fabs(prev[x-size/2]-cur) +
		 fabs(prev[x]-cur) +
                 fabs(prev[x+size/2]-cur) + fabs(prev[x+size]-cur) +
                 fabs(line[x-size]-cur) + fabs(line[x-size/2]-cur) +
                 fabs(line[x+size/2]-cur) + fabs(line[x+size]-cur) +
                 fabs(aftr[x-size]-cur) + fabs(aftr[x-size/2]-cur) +
                 fabs(aftr[x]-cur) + fabs(aftr[x+size/2]-cur) +
		 fabs(aftr[x+size]-cur));
              seed->x=x;
              seed->y=y;
              seed->height=cur;
              seed->slopeErr=slopeErr;
              seeds[numSeeds++]=seed;
            }
        }
    }
  // Now sort that list of potential seed points
  asfPrintStatus("   Potential seed points: %i\n",numSeeds);
  qsort(seeds,numSeeds,sizeof(seedRec *),
	(int (*)(const void *,const void *))qsorter);

  // Now file that list of seed points into the boxes, starting at the 
  // top of the list.
  // printf("Sifting the better seed points into boxes.\n");

  /*Set the boxes initially to all empty.*/
  for (y=0;y<boxRes;y++)
    for (x=0;x<boxRes;x++)
      boxes[y*boxRes+x]=NULL;
  seedNo=0;
  while (seedNo<numSeeds&&numBoxes<boxFrac)
    {
      seedRec *seed=seeds[seedNo];
      /*Try putting this seed point into it's corresponding box--
        if the box is empty, then this point is a good one (output it).
        if the box is already full, then discard this point. */
      int seedX=seed->x*boxRes/sample_count;
      int seedY=seed->y*boxRes/line_count;
      if (boxes[seedY*boxRes+seedX]==NULL)
        {
          boxes[seedY*boxRes+seedX]=seed;
          fprintf(fpout,"%i %i %f\n",
                  (int)(seed->x*metaAmp->sar->sample_increment +
                        metaAmp->general->start_sample),
                  (int)(seed->y*metaAmp->sar->line_increment +
                        metaAmp->general->start_line),
                  seed->height);
          numBoxes++;
        }
      seedNo++;
    }
  asfPrintStatus("Final number of seed points: %i\n\n", seedNo);
  asfPrintStatus("Seed point distribution:\n\n");
  for (y=0;y<boxRes;y++) {
    asfPrintStatus("   ");
    for (x=0;x<boxRes;x++)
      asfPrintStatus("%c",boxes[y*boxRes+x]?'X':' ');
    asfPrintStatus("\n");
  }
  asfPrintStatus("\n");

  return(0);
}

typedef struct
{
  double Bn,dBn;
  double Bp,dBp;
} baseLine;

static baseLine readBaseLine(char *fname)
{
  baseLine base;
  FILE *fp1;
  fp1 = FOPEN(fname,"r");
  if (4!=fscanf(fp1,"%lf%lf%lf%lf",&base.Bn,&base.dBn,&base.Bp,&base.dBp))
    asfPrintError("Unable to read baseline from file '%s'\n", fname);
  FCLOSE(fp1);

  return base;
}

static double baseLineDiff(const baseLine *a,const baseLine *b)
{
  return fabs(a->Bn-b->Bn)+
    fabs(a->dBn-b->dBn)+
    fabs(a->Bp-b->Bp)+
    fabs(a->dBp-b->dBp);
}

static int check_refinement(char *base1, char *base2, char *base3)
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


char *base2str(int baseNo, char *base)
{
  char *baseStr=(char *)MALLOC(sizeof(char)*50);
  
  if (baseNo<10) sprintf(baseStr,"%s.base.0%i", base, baseNo);
  else sprintf(baseStr,"%s.base.%i", base, baseNo);
  
  return baseStr;
}

int asf_baseline(char *baseName, char *interferogram, char *seeds, 
		 int max_iterations, int *iterations)
{
  char *veryoldBase=NULL, *oldBase=NULL, *newBase=NULL, tmp[255];
  int i;

  sprintf(tmp, "%s_ml_amp.img", interferogram);
  check_return(dem2seeds("dem_slant.img", tmp, seeds, 0),
	       "creating seed points (dem2seeds)");

  check_return(deramp("unwrap", base2str(0, baseName),
		      "unwrap_nod", 1),
	       "reramping unwrapped phase (deramp)");
  
  // Baseline refinement
  newBase = base2str(0, baseName);
  for (i=0; i<max_iterations; i++) {
    veryoldBase = oldBase;
    oldBase = newBase;
    newBase = base2str(i+1, baseName);
    
    check_return(refine_baseline("unwrap_nod_phase.img",
				 seeds, oldBase, newBase),
		 "baseline refinement (refine_baseline)");
    if (i>0)
      if (check_refinement(newBase,oldBase,veryoldBase)) break;
  }
  if (i == max_iterations)
    check_return(1, "Baseline iterations failed to converge");
  *iterations = i+1;
  
  check_return(deramp(interferogram,
		      base2str(*iterations, baseName),
		      "igramd", 0),
	       "deramping interferogram with refined baseline (deramp)");
  sprintf(tmp, "%s_ml", interferogram);
  check_return(multilook("igramd", tmp, "a_cpx.meta", NULL),
	       "multilooking refined interferogram (multilook)");

  return(0);
}
