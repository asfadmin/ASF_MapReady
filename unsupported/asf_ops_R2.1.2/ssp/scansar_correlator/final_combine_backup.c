/* SccsId[]= @(#)final_combine_backup.c	2.41 3/24/98 */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <memory.h>
#include "error.h"
#define MAX_P     40
float *IMAGE;
char  *MASK;
int overlay_f[MAX_P];
extern float *IMAGE;
extern char  *MASK;
extern int overlay_f[MAX_P];


int image_combine_backup(istart_c,start_p_c,in_image,in_mask,NX_c,NX_DIM_c,PAGE_BUFFSIZE_c,file_image_dump,file_mask_dump)
  int   *start_p_c;
  int   *NX_c,*PAGE_BUFFSIZE_c; 
  int   *istart_c;
  int   *NX_DIM_c;
  float *in_image;
  char  *in_mask;
  char  *file_image_dump;
  char  *file_mask_dump;
{
  int ireturn;
  int i;
  int start_p;
  int NX,PAGE_BUFFSIZE,NX_DIM; 
  int ifd_image;
  int ifd_mask;

  NX           = *NX_c;
  NX_DIM       = *NX_DIM_c;
  PAGE_BUFFSIZE= *PAGE_BUFFSIZE_c;
  start_p      = *start_p_c;



   if (*istart_c==0)  
    {
     for (i = 0; i < strlen(file_image_dump); i++)
      if (*(file_image_dump + i) == ' ') *(file_image_dump + i) = '\0';
     for (i = 0; i < strlen(file_mask_dump); i++)
      if (*(file_mask_dump + i) == ' ') *(file_mask_dump + i) = '\0';
/*     unlink(file_image_dump);   */
/*     unlink(file_mask_dump);    */
/*     ifd_image = creat(file_image_dump     , 00755 ); */
/*     ifd_mask  = creat(file_mask_dump      , 00755 ); */
     printf("%s %s\n",file_image_dump,file_mask_dump);
     for (i=0;i<MAX_P;i++) overlay_f[i]=0;
     if( (ifd_image=open(file_image_dump,2)) <= 0 ) 
      {
       printf(" Can not open the input file IMAGE_MAP\n");
       fflush(stdout);
       return(ierr_2);
      }
     if( (ifd_mask=open(file_mask_dump,2)) <= 0 ) 
      {
       printf(" Can not open the input file MASK_MAP\n");
       fflush(stdout);
       return(ierr_2);
      }
      printf("%d %d %d %d\n",NX,NX_DIM,PAGE_BUFFSIZE,MAX_P);
      if( (IMAGE = (float *) mmap(0,NX*PAGE_BUFFSIZE*MAX_P*4,
                   PROT_READ | PROT_WRITE, MAP_FILE, ifd_image, 0)) == NULL)
      {
         printf(" Can not memory mapped the IMAGE file \n");
         fflush(stdout);
         return(ierr_16);
       }
     if( (MASK = (char *) mmap(0,NX*PAGE_BUFFSIZE*MAX_P,
                   PROT_READ | PROT_WRITE, MAP_FILE, ifd_mask, 0)) == NULL)
      {
         printf(" Can not memory mapped the MASK file \n");
         fflush(stdout);
         return(ierr_16);
       }
      *istart_c=1;
      close(ifd_image);
      close(ifd_mask);
     }
/************************************************************************/
  OVERLAY_IMAGES(in_image,in_mask,start_p,NX,NX_DIM,PAGE_BUFFSIZE);


 return(iok);
}
/**********************************************************/
int DO_MASKING(ip,temp_image,temp_mask,iflag,NX,PAGE_BUFFSIZE)
float *temp_image;
char  *temp_mask;
int ip,iflag;
int NX,PAGE_BUFFSIZE;
{

    if (iflag==0)
      overwrite(ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE);
    else
      overlaying(ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE);

   return(1);
}

/**********************************************************/
int  overwrite(ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE)
int ip;
float *temp_image;
char  *temp_mask;
int NX,PAGE_BUFFSIZE;
{
 int i,istart,j;
 

 istart=ip*NX*PAGE_BUFFSIZE;

/*   for (i=0;i<NX*PAGE_BUFFSIZE;i++)   
     {                                
       j=i+istart;    
       IMAGE[j]=  temp_image[i];
        MASK[j]=  temp_mask[i];
     }                             */

  memcpy(&MASK[istart] ,temp_mask ,NX*PAGE_BUFFSIZE);
  memcpy(&IMAGE[istart],temp_image,NX*PAGE_BUFFSIZE*sizeof(float));
printf("done overwriting page # %d\n",ip);
fflush(stdout);
 return(1);
}
int  overlaying(ip,temp_image,temp_mask,NX,PAGE_BUFFSIZE)
int ip;
float *temp_image;
char  *temp_mask;
int NX,PAGE_BUFFSIZE;
{
 int i,istart,j;
 

 istart=ip*NX*PAGE_BUFFSIZE;

 for (i=0;i<NX*PAGE_BUFFSIZE;i++)
   {
    j=i+istart;
    if ((MASK[j]>0) && (temp_mask[i]>0))   
    {
     IMAGE[j]=(IMAGE[j]+temp_image[i])/2;
     MASK[j]=( MASK[j]+temp_mask[i] )/2;
    }
    if ((MASK[j]==0) && (temp_mask[i]==0))   
    {
     IMAGE[j]=(IMAGE[j]+temp_image[i])/2;
     MASK[j]=( MASK[j]+temp_mask[i] )/2;
    }
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

printf("done overlaying page # %d\n",ip);
 return(1);
}


int  OVERLAY_IMAGES(in_image,in_mask,start_p,NX,NX_DIM,PAGE_BUFFSIZE)
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


 
 
     ip=start_p;
     for (i=0;i<PAGE_BUFFSIZE;i++)
      {
       istart=i*NX_DIM;
       jstart=i*NX;
       temp_image[jstart+i]=  in_image[istart+i];
       temp_mask [jstart+i]=  in_mask [istart+i];
/*  memcpy(&temp_image[jstart] ,&in_image[istart] ,NX*sizeof(float));
  memcpy(&temp_mask [jstart] ,&in_mask [istart] ,NX              ); */
       }
      printf("doing page #%d \n",ip);
      fflush(stdout);
      DO_MASKING(ip,temp_image,temp_mask,overlay_f[ip],NX,PAGE_BUFFSIZE);
      overlay_f[ip]++;


   free(temp_image);
   free(temp_mask);

  return(1);
}
