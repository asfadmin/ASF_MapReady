/* SccsId[]= @(#)final_combine.c	2.41 3/24/98 */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>
#include <sys/types.h>
#include <sys/mman.h>
#define MAX_P      40
#include "error.h"



int image_combine(istart_c,iptr_image,iptr_mask,overlay_f,max_p_c,curr_p_c,curr_num_p_c,in_image,in_mask,NX_c,NX_DIM_c,PAGE_BUFFSIZE_c,icheck_c,last_p_c,last_num_p_c,file_image_dump,file_mask_dump,ct_profile_f,c1_pxl_f,c2_pxl_f,fm_sizec2_in_f,ct_prof_len_f,l1_ct_pro_f,l2_ct_pro_f)
  int   *curr_p_c;
  int   *NX_c,*PAGE_BUFFSIZE_c; 
  int   *istart_c;
  int   *NX_DIM_c;
  float *in_image;
  char  *in_mask;
  int   *max_p_c;
  int   *icheck_c,*last_p_c,*last_num_p_c;
  int   *curr_num_p_c;
  char *file_image_dump,*file_mask_dump;
  int   *overlay_f;
  int *iptr_image,*iptr_mask;
/******************************************************/
  double     *ct_profile_f;
  double     *c1_pxl_f,*c2_pxl_f;
  double     *fm_sizec2_in_f;
  int        *ct_prof_len_f;
  int        *l1_ct_pro_f,*l2_ct_pro_f;
/******************************************************/

{
  int i;
  int curr_p,max_p,icheck,last_p,last_num_p,curr_num_p;
  int NX,PAGE_BUFFSIZE,NX_DIM; 
  int ibegin,iend,islot;
  char buff[100];
  double     *ct_profile;
  double     c1_pxl,c2_pxl;
  double     fm_sizec2_in;
  int        ct_prof_len;
  int        l1_ct_pro,l2_ct_pro;
  char       file_image_dump_c[60],file_mask_dump_c[60];
  float *IMAGE;
  char  *MASK;
  int istat;
  
  ct_profile  =  ct_profile_f;
  ct_prof_len = *ct_prof_len_f; 
  c1_pxl      = *c1_pxl_f;
  c2_pxl      = *c2_pxl_f;
  fm_sizec2_in= *fm_sizec2_in_f;
  l1_ct_pro   = *l1_ct_pro_f;
  l2_ct_pro   = *l2_ct_pro_f;
  
  

  NX           = *NX_c;
  NX_DIM       = *NX_DIM_c;
  PAGE_BUFFSIZE= *PAGE_BUFFSIZE_c;
  curr_p      =  *curr_p_c;
  max_p        = *max_p_c;
  icheck       = *icheck_c;
  last_p       = *last_p_c;
  last_num_p   = *last_num_p_c;
  curr_num_p   = *curr_num_p_c;

str_f_to_c(file_image_dump_c,file_image_dump,60);
str_f_to_c(file_mask_dump_c ,file_mask_dump,60);

   if (*istart_c==0)  
    {
     printf("*****NUMBER OF PAGES BEING ALLOCATED: %d*****\n",max_p);
     fflush(stdout);

     for (i=0;i<max_p;i++) overlay_f[i]=0;
       *iptr_image = (int) malloc(sizeof(float)*max_p*NX*PAGE_BUFFSIZE);    
       if (*iptr_image == 0)                                                        
        {                                                                  
         printf("memory requested %d bytes failed\n",sizeof(float)*max_p*NX*PAGE_BUFFSIZE);
         fflush(stdout);
         sprintf(buff,"memory allocation of %s bytes failed",sizeof(float)*max_p*NX*PAGE_BUFFSIZE); 
         printclog(3,buff);
         return(ierr_5);                                
        }     
    
       *iptr_mask  = (int) malloc(sizeof(char )*max_p*NX*PAGE_BUFFSIZE);     
       if (*iptr_mask == 0)                                                          
        {                                                                  
         printf("memory requested %d bytes failed\n",sizeof(char)*max_p*NX*PAGE_BUFFSIZE);    
         fflush(stdout);
         sprintf(buff,"memory allocation of %s bytes failed",sizeof(char)*max_p*NX*PAGE_BUFFSIZE); 
         printclog(3,buff);
         return(ierr_5);    
        }    
     *istart_c=1;
    }
/************************************************************************/
   IMAGE= (float *) *iptr_image;
   MASK = (char  *) *iptr_mask;
    printf("ADDR FOR IMAGE IS %d\n",IMAGE);
    printf("ADDR FOR MASK  IS %d\n",MASK);
    fflush(stdout);
    if (icheck==1)
     {
      iend  =curr_p-1;
      ibegin=last_p-(last_num_p-1);
      if ((ibegin>=0) && (iend>=0))  
       {
         for (i=ibegin;i<=iend;i++)
          {
           islot= i%max_p;
           istat=dump_slot(file_image_dump_c,file_mask_dump_c,IMAGE,MASK,i,islot,NX,NX_DIM,PAGE_BUFFSIZE,ct_profile,c1_pxl,c2_pxl,fm_sizec2_in,ct_prof_len,l1_ct_pro,l2_ct_pro);
           if (istat != iok) return(istat);
           *ct_prof_len_f = ct_prof_len;
           overlay_f[islot]=0;
           printf("COMPLETED PAGE # %d\n",i);
           fflush(stdout);
           sprintf(buff,"COMPLETED OVERLAYING PAGE # %d",i);
           printclog(3,buff);
          }
       }
     }
  printf("WORKING ON ***************** THE PAGE # %d\n",curr_p);

  OVERLAY_IMAGES(IMAGE,MASK,in_image,in_mask,curr_p%max_p,NX,NX_DIM,PAGE_BUFFSIZE,overlay_f);

    if (icheck==2)
     {
       ibegin=  curr_p-curr_num_p+1; 
       iend  =  curr_p;
       for (i=ibegin;i<=iend;i++)
         {
           islot= i%max_p;
           istat=dump_slot(file_image_dump_c,file_mask_dump_c,IMAGE,MASK,i,islot,NX,NX_DIM,PAGE_BUFFSIZE,ct_profile,c1_pxl,c2_pxl,fm_sizec2_in,ct_prof_len,l1_ct_pro,l2_ct_pro);
           if (istat != iok) return(istat);
           *ct_prof_len_f = ct_prof_len;
           printf("COMPLETED PAGE # %d\n",i);
           fflush(stdout);
           sprintf(buff,"COMPLETED OVERLAYING PAGE # %d",i);
           printclog(3,buff);
         }
         free((float *) *iptr_image);   
         free((char  *) *iptr_mask);    
     }


 return(iok);
}
/**********************************************************/
int dump_slot(file_image_dump_c,file_mask_dump_c,IMAGE,MASK,ipage,islot,
NX,NX_DIM,PAGE_BUFFSIZE,ct_profile,c1_pxl,c2_pxl,fm_sizec2_in,ct_prof_len,
l1_ct_pro,l2_ct_pro)
char *file_image_dump_c,*file_mask_dump_c;
float *IMAGE;
char  *MASK;
int islot;
int NX,NX_DIM,PAGE_BUFFSIZE;
int ipage;
double     *ct_profile;
double       c1_pxl,c2_pxl;
double     fm_sizec2_in;
int        ct_prof_len;
int        l1_ct_pro,l2_ct_pro;
{
int ifd_image,ifd_mask;
 int istart,jstart,bytes;
 int i,j,k,index;
 int index_IMAGE,index_ac;
 int ipst;
 float *buff;
 float *array_data;
 char  *array_mask;

 array_data=(float *)malloc(sizeof(float)*NX); 
 array_mask=(char  *)malloc(sizeof(char )*NX); 


istart=islot*NX    *PAGE_BUFFSIZE;



 for (i=0;i<PAGE_BUFFSIZE;i++)
  {
    for (k=0;k<NX;k++)
     {
       index=istart+i*NX+k;
       if (MASK[index]!=0)
        IMAGE[index]= sqrt(IMAGE[index]/MASK[index]);
     }
    j=(ipage*NX*PAGE_BUFFSIZE)/NX+i;
    if ((j>l1_ct_pro) && (j<l2_ct_pro))
      {
       memcpy(array_data,&IMAGE[istart+i*NX],NX*sizeof(float));
       memcpy(array_mask,&MASK[istart+i*NX] ,NX);
       gen_ct_profile(NX,fm_sizec2_in,c1_pxl,c2_pxl,ct_profile,ct_prof_len,
array_data,array_mask);
      }
  }


/* DUMPING ISLOT TO DISK */
     if( (ifd_image=open(file_image_dump_c,2)) <= 0 )
      {
       printf(" Can not open the input file IMAGE_MAP\n");
       fflush(stdout);
       return(ierr_2);
      }
     if( (ifd_mask=open(file_mask_dump_c,2)) <= 0 )
      {
       printf(" Can not open the input file MASK_MAP\n");
       fflush(stdout);
       return(ierr_2);
      }

lseek (ifd_image, (long)(ipage*NX*PAGE_BUFFSIZE*4), 0);
lseek (ifd_mask , (long)(ipage*NX*PAGE_BUFFSIZE)  , 0);
 
write(ifd_image,&IMAGE[istart],NX*PAGE_BUFFSIZE*4);
write(ifd_mask ,&MASK [istart],NX*PAGE_BUFFSIZE);

close(ifd_image);
close(ifd_mask);

free(array_data);
free(array_mask);


 
 return(iok);

  

}
/**********************************************************/
int DO_MASKING(IMAGE,MASK,ip,temp_image,temp_mask,iflag,NX,PAGE_BUFFSIZE)
float *IMAGE;
char  *MASK;
float *temp_image;
char  *temp_mask;
int ip,iflag;
int NX,PAGE_BUFFSIZE;
{

    if (iflag==0)
      overwrite(IMAGE,MASK,ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE);
    else
      overlaying(IMAGE,MASK,ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE);

   return(1);
}

/**********************************************************/
int  overwrite(IMAGE,MASK,ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE)
float *IMAGE;
char  *MASK;
int ip;
float *temp_image;
char  *temp_mask;
int NX,PAGE_BUFFSIZE;
{
 int i,istart,j;
 

 istart=ip*NX*PAGE_BUFFSIZE;

/* for (i=0;i<NX*PAGE_BUFFSIZE;i++) */
/*   {                              */
/*     j=i+istart;  */
/*     IMAGE[j]=  temp_image[i]; */
/*      MASK[j]=  temp_mask[i];*/
/*   }                          */

  memcpy(&IMAGE[istart],temp_image,NX*PAGE_BUFFSIZE*sizeof(float));
  memcpy(&MASK[istart] ,temp_mask ,NX*PAGE_BUFFSIZE);



printf("done overwriting page # %d\n",ip);
 return(1);
}
int  overlaying(IMAGE,MASK,ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE)
float *IMAGE;
char  *MASK;
int ip;
float *temp_image;
char  *temp_mask;
int NX,PAGE_BUFFSIZE;
{
 int i,istart,j;
 int total_mask;
 float v;
 

 istart=ip*NX*PAGE_BUFFSIZE;

 for (i=0;i<NX*PAGE_BUFFSIZE;i++)
   {
    j=i+istart;
    if ((MASK[j]>0) && (temp_mask[i]>0))   
    {
    
     total_mask=(int) MASK[j]+(int) temp_mask[i];
     IMAGE[j]=IMAGE[j]+temp_image[i];
     MASK[j]=total_mask;
/*     IMAGE[j]=(IMAGE[j]+temp_image[i])/2;*/
/*     MASK[j]=( MASK[j]+temp_mask[i] )/2; */
    }
/*    if ((MASK[j]==0) && (temp_mask[i]==0))   
    {
     IMAGE[j]=(IMAGE[j]+temp_image[i])/2;
     MASK[j]=( MASK[j]+temp_mask[i] )/2;
    } */
/*    if ((MASK[j]>0) && (temp_mask[i]==0))   
    {
     IMAGE[j]=IMAGE[j];
      MASK[j]= MASK[j];
    } */
    if ((MASK[j]==0) && (temp_mask[i]>0))   
    {
     IMAGE[j]=temp_image[i];
      MASK[j]=temp_mask[i]; 
    }
   } 

printf("**********************done overlaying page # %d\n",ip);
 return(1);
}


int  OVERLAY_IMAGES(IMAGE,MASK,in_image,in_mask,start_p,NX,NX_DIM,PAGE_BUFFSIZE,overlay_f)
int *overlay_f;
float *IMAGE;
char  *MASK;
float *in_image;
char  *in_mask;
int start_p;
int NX,PAGE_BUFFSIZE;
int NX_DIM;
{
float *temp_image;
char  *temp_mask;
int ip,istart;
int i,j,k;
int jstart,ki_start;

   temp_image=(float *)malloc(sizeof(float)*NX*PAGE_BUFFSIZE);
   temp_mask =(char  *)malloc(sizeof(char) *NX*PAGE_BUFFSIZE);
     for (i=0;i<PAGE_BUFFSIZE;i++)
      {
       istart=i*NX_DIM;
       jstart=i*NX;
  memcpy(&temp_image[jstart] ,&in_image[istart] ,NX*sizeof(float));
  memcpy(&temp_mask [jstart] ,&in_mask [istart] ,NX              );
/*  temp_image[jstart+i]=in_image[istart+i]; */
/*  temp_mask [jstart+i]=in_mask [istart+i]; */
       }


     ip=start_p;
/*      printf("doing page #%d \n",ip); */
      DO_MASKING(IMAGE,MASK,ip,temp_image,temp_mask,overlay_f[ip],NX,PAGE_BUFFSIZE);
      overlay_f[ip]++;


   free(temp_image);
   free(temp_mask);

  return(1);
}
int gen_ct_profile(sam_ac2,fm_sizec2_in,c1_pxl,c2_pxl,ct_profile,ct_prof_len,array_data,array_mask)
          double     *ct_profile;
          int         sam_ac2;
          double       c1_pxl,c2_pxl;
          double     fm_sizec2_in;
          int        ct_prof_len;
          float *array_data;
          char  *array_mask;

{
          int    ist,k,nlooks,iend,i;


              ist= 0;
              for (k=0;k<(sam_ac2/8);k++)
               {
                nlooks = array_mask[k];
                if(nlooks!=0)
                 {
                  ist = k;
                  break;
                 }
               }
              iend= ist+fm_sizec2_in/c2_pxl-2;

              if (ist!=0)
               {
                if (c1_pxl==100.0)
                 {
                  ct_prof_len = iend-ist-1;
                  for (i=0;i<ct_prof_len;i++)
                   ct_profile[i]=ct_profile[i]+array_data[i+ist];
                 }
                else
                 {
                  ct_prof_len = (iend-ist)/2-2;
                  for (i=0;i<ct_prof_len;i++)
                   ct_profile[i]=ct_profile[i]+array_data[i*2+ist];
                 }
                }

        return(1);
}
