



#include "2dmap.h"

void calcplane(refline,refsample,dline,dsample,npoints,acoef,bcoef,ccoef)
double refline[],
       refsample[],
       dline[],
       dsample[],
       acoef[],
       bcoef[],
       ccoef[];
unsigned int npoints;

{
   double minq,
          test1l, test2l, test1s, test2s,
          tv1lmag, tv2lmag, tv1smag, tv2smag,
          angl, angs, cosangl, cosangs,
          coef1l, coef2l, coef3l, coef1s, coef2s, coef3s,
          sumal, sumbl, sumcl, sumdl, sumas, sumbs, sumcs, sumds,
          vmagl, vmags, 
          vec1l[5], vec2l[5], vec3l[5], vec1s[5], vec2s[5], vec3s[5],
          tvec1l[5], tvec2l[5], tvec3l[5], tvec1s[5], tvec2s[5], tvec3s[5],
          nvec;

   int i, j, k, l;

minq = 50.0;
nvec = 0.0;
sumal = 0.0; sumbl = 0.0; sumcl = 0.0; sumdl = 0.0;
sumas = 0.0; sumbs = 0.0; sumcs = 0.0; sumds = 0.0;

for (i = 0 ; i < (int)npoints ; i++)
 {
   vec1l[1] = vec1s[1] = refline[i]; 
   vec1l[2] = vec1s[2] = refsample[i];
   vec1l[3] = dline[i];
   vec1s[3] = dsample[i];
   for (j = i + 1 ; j < (int)npoints ; j++)
    {
      vec2l[1] = vec2s[1] = refline[j];
      vec2l[2] = vec2s[2] = refsample[j];
      vec2l[3] = dline[j];
      vec2s[3] = dsample[j];
      for (k = j + 1 ; k < (int)npoints ; k++)
       {
         vec3l[1] = vec3s[1] = refline[k];
         vec3l[2] = vec3s[2] = refsample[k];
         vec3l[3] = dline[k];
         vec3s[3] = dsample[k];
         for (l = 1 ; l <= 3 ; l++)
          {
            tvec1l[l] = vec2l[l] - vec1l[l];
            tvec2l[l] = vec3l[l] - vec1l[l];
            tvec1s[l] = vec2s[l] - vec1s[l];
            tvec2s[l] = vec3s[l] - vec1s[l];
           }
         tv1lmag = mag(tvec1l);
         tv2lmag = mag(tvec2l);
         tv1smag = mag(tvec1s);
         tv2smag = mag(tvec2s);
         cosangl = dot(tvec1l,tvec2l) / (tv1lmag * tv2lmag);
         cosangs = dot(tvec1s,tvec2s) / (tv1smag * tv2smag);
         angl = acos(fabs(cosangl)) / RPD;
         angs = acos(fabs(cosangs)) / RPD;
         test1l = (1.0 - fabs(cosangl)) * tv1lmag;
         test2l = (1.0 - fabs(cosangl)) * tv2lmag;
         test1s = (1.0 - fabs(cosangs)) * tv1smag;
         test2s = (1.0 - fabs(cosangs)) * tv1smag;

         if (test1l > minq && test2l > minq && test1s > minq && test2s > minq)
           {
             cross(tvec1l,tvec2l,tvec3l);
             cross(tvec1s,tvec2s,tvec3s);
             
             if ((tvec3l[3] != 0) && (tvec3s[3] != 0))
              {
                nvec = nvec + 1.0;

                vmagl = dot(vec1l,tvec3l);
                vmags = dot(vec1s,tvec3s);

                coef1l = -tvec3l[1] / tvec3l[3];
                coef2l = -tvec3l[2] / tvec3l[3];
                coef3l = vmagl / tvec3l[3];

                coef1s = -tvec3s[1] / tvec3s[3];
                coef2s = -tvec3s[2] / tvec3s[3];
                coef3s = vmags / tvec3s[3];
/*
                printf("  %\n");
                printf("nvec = %lf\n",nvec);
                printf("i,j,k %d\t%d\t%d\n",i,j,k);
                printf("a,b,c,ang for line = %lf\t%lf\t%lf\t%lf\n",coef1l,
                          coef2l,coef3l,angl);
                printf("mag v1l, v2l for line = %lf\t%lf\n",tv1lmag,tv2lmag);
                printf("a,b,c,ang for sample = %lf\t%lf\t%lf\t%lf\n",coef1s,
                          coef2s,coef3s,angs);
                printf("mag v1s, v2s for line = %lf\t%lf\n",tv1smag,tv2smag); 
*/
                sumal += coef1l;
                sumbl += coef2l;
                sumcl += coef3l;

                sumas += coef1s;
                sumbs += coef2s;
                sumcs += coef3s;
             }

           }
       }
    }
 }

acoef[1] = sumal / nvec;
bcoef[1] = sumbl / nvec;
ccoef[1] = sumcl / nvec;

acoef[2] = sumas / nvec;
bcoef[2] = sumbs / nvec;
ccoef[2] = sumcs / nvec;

return;
}

