/* SccsId[]= @(#)cio.c	2.41 3/24/98 */
static char sccsid_cio[]= "@(#)PPcio.c:2.41";

/*   
	PROGRAM NAME: cio.c


	Abstract:

	   i/o routines callable by fortran77

	   int str_cat(char *fnm)
	   int ccreate(char *fnm)
	   int copen(char *fnm)
	   int copen_w(char *fnm)
	   int copen_r(char *fnm)
	   int cread(int *fd, char *buf, int *nb, int *fb)
	   int cwrite(int *fd, char *buf, int *nb, int *fb)
	   int cwrite_seq(int *fd, char *buf, int *nb)
	   int cread_seq (int *fd, char *buf, int *nb)
           int c_load_config()
	   void cclose(int *fd)

	where 'fnm' is a pointer to a filename  
	      'fd' is a pointer to a file descriptor
	      'buf' is a pointer to a data buffer
	      'nb' is a pointer to a value = number of bytes to read or write
	      'fb' is a pointer to a value = first byte to read or write
*/

#include <stdio.h>
#include <time.h>
#include <fcntl.h> 
/* #include <sys/file.h> */
#include <errno.h>
#include "error.h"
#define PERMS 2
extern int errno;

int ccreate(fnm)
char *fnm;
{
    int i, fd,istat;
    int userid;
    userid = getuid();
    (void) setreuid(0, userid);
    for (i = 0; i < strlen(fnm); i++)
      if (*(fnm + i) == ' ') *(fnm + i) = '\0';
      istat= unlink(fnm);
      if ( ( fd = creat( fnm, 0755 ) ) <= 0 ) 
      {
        perror( "  ***   Error creating  ***\n" );
        printf( " %s\n", fnm );
        printf( " %i\n", fd );
        (void) setreuid(userid,0);
        return( -1 );
      }
      else
      {
       (void) setreuid(userid,0);
       return(fd); /* (creat (fnm, 0755)); 4/5/92*/
      }
}

int str_cat(fnm,num)
char *fnm;
int *num;
{
char str_num[10];
    int i, fd;
    for (i = 0; i < strlen(fnm); i++)
      if (*(fnm + i) == ' ') *(fnm + i) = '\0';
    sprintf(str_num,"%d",*num); 
    strcat(fnm,".");
    strcat(fnm,str_num);
    for (i=strlen(fnm);i<60;i++)
     *(fnm+i)= ' ';

    return(1);
}

int copen_r(fnm)
char *fnm;
{
    int i, fd;
    for (i = 0; i < strlen(fnm); i++)
      if (*(fnm + i) == ' ') *(fnm + i) = '\0';
      if ( ( fd = open( fnm, 0 ) ) <= 0 )
      {
        perror( "  ***   Error opening  ***\n" );
        printf( " %s\n", fnm );
        printf( " %d\n", fd );
        return( -1 );
      }
      else
      {
       return(fd); /* (open (fnm, O_RDONLY)); 4/5/92*/
      }
}
int copen_w(fnm)
char *fnm;
{
    int i, fd;
    for (i = 0; i < strlen(fnm); i++)
      if (*(fnm + i) == ' ') *(fnm + i) = '\0';
      if ( ( fd = open( fnm, 2 ) ) <= 0 )
      {
        perror( "  ***   Error opening  ***\n" );
        printf( " %s\n", fnm );
        printf( " %d\n", fd );
        return( -1 );
      }
      else
      {
       return(fd); /* (open (fnm, O_RDONLY)); 4/5/92*/
      }
}

int cwrite(fd, var, nb, fb)
int *fd, *nb, *fb;
char *var;
{
    int i2_status;
    if (*fb>=0){			/* 4/9/93 */
        lseek (*fd, (long)(*fb), 0);
    }
      if ( ( i2_status=write(*fd,var,*nb) ) != *nb )
      {
        perror( "  ***   Error cWriting  ***\n" );
        printf( " %i\n", i2_status );
        return( -1 );
      }
      else
      {
       return(i2_status); 
      }
}
int cwrite_seq(fd, var, nb)
int *fd, *nb;
char *var;
{
    int i2_status;
      if ( ( i2_status=write(*fd,var,*nb) ) != *nb )
      {
        perror( "  ***   Error cWriting  ***\n" );
        printf( " %i\n", i2_status );
        return( -1 );
      }
      else
      {
       return(i2_status); 
      }
}

int cread(fd, var, nb, fb)
int *fd, *nb, *fb;
char *var;
{
    int i2_status;
    if (*fb>=0){	               /* 4/9/93 */
        lseek (*fd, (long)(*fb), 0);
    }
      if ( ( i2_status=read(*fd,var,*nb) ) != *nb )
      {
        perror( "  ***   Error cReading  ***\n" );
        printf( " %i\n", i2_status );
        return( -1 );
      }
      else
      {
       return(i2_status); 
      }
/*    return (read(*fd, var, *nb)); */
}
int cread_seq(fd, var, nb)
int *fd, *nb;
char *var;
{
    int i2_status;
      if ( ( i2_status=read(*fd,var,*nb) ) != *nb )
      {
        perror( "  ***   Error cReading  ***\n" );
        printf( " %i\n", i2_status );
        return( -1 );
      }
      else
      {
       return(i2_status); 
      }
/*    return (read(*fd, var, *nb)); */
}

void cclose(fd)
int *fd;
{
    close(*fd);
}

int copen(fnm)
char *fnm;
{
    int i, fd;
    for (i = 0; i < strlen(fnm); i++)
      if (*(fnm + i) == ' ') *(fnm + i) = '\0';
      if ( ( fd = open( fnm, 2 ) ) <= 0 )
      {
        perror( "  ***   Error opening  ***\n" );
        printf( " %s\n", fnm );
        printf( " %d\n", fd );
        return( -1 );
      }
      else
      {
       return(fd); /* (open (fnm, O_RDONLY)); 4/5/92*/
      }
}

int str_f_to_c (str1,str2,len)
char *str1;
char *str2;
int len;
{
  int i;
  char *dummy_char;

  dummy_char= (char *)malloc(sizeof(char)*(len+1));
 
  for (i=0;i<len;i++)
  {
    if (*(str2 + i) == ' ')
    { 
     *(dummy_char +i)= '\0';
     break;
    }
  *(dummy_char + i) = *(str2 +i);
  }

   *(dummy_char + i)= '\0';
  

  strcpy(str1,dummy_char);



 free(dummy_char);



return(1);
}

int str_c_to_f(str_f,str_c,len)
char *str_f;
char *str_c;
int len;

{
 int i;

 *str_f='\0';
 strcpy(str_f,str_c);

 for (i=strlen(str_f);i<len;i++)
 *(str_f+i)= ' ';

 return(1);

}
int int_to_str(num,str,len)
int  *num;
char *str;
int  *len;
{
 char buff[10];
 int i;

 sprintf(buff,"%d",*num);
 strcpy(str,buff);
 for (i=strlen(str);i<(*len);i++)
  *(str+i)=' ';

 return(1);
 

}

char* status_str(str)
char *str;
{
  char *dummy,*str1;

  str1=(char *)malloc(sizeof(char)*strlen(str));
  strcpy(str1,str);

/*  printf("%s\n",str1);*/
  dummy=(char *)strtok(str1,";");
  

 return(dummy);
}
char* description_str(str)
char *str;
{
  char *dummy,*str1;

  str1=(char *)malloc(sizeof(char)*strlen(str));
  strcpy(str1,str);

/*  printf("%s\n",str1);*/
  dummy=(char *)strtok(str1,";");
  dummy=(char *)strtok(NULL,";");

 return(dummy);
}

int printcstat(file_status,istage,ipercent)
int istage;
float ipercent;
char *file_status;
{
FILE *DUMMY;

 DUMMY=fopen(file_status,"a");
 fprintf(DUMMY,"%d %3.2f\n",istage,ipercent);
 fflush(DUMMY);
 fclose(DUMMY);
 return(1);
}
int printfstat(file_status,istage,ipercent)
int *istage;
float *ipercent;
char *file_status;
{
FILE *DUMMY;
char file_status_c[256];

 str_f_to_c(file_status_c,file_status,256);
 DUMMY=fopen(file_status_c,"a");
 fprintf(DUMMY,"%d %3.2f\n",*istage,*ipercent);
 fflush(DUMMY);
 fclose(DUMMY);
 return(1);
}
char * get_host_name(str,file_config)
char *str;
char *file_config;
{
 int i,j;
 char* str_dummy; 
 FILE *fp;

 char *test_str;
 test_str=(char *)strstr(str,":");
 if (test_str==NULL) return(NULL);
   

 str_dummy=(char *)malloc(sizeof(char)*strlen(str) + 20);
 for (i=0;i<strcspn(str,":");i++)
   str_dummy[i]=str[i];
 str_dummy[i]=NULL; 
 strcat(str_dummy, "-fddi");

/*
 fp=fopen(file_config,"r"); 
 fscanf(fp,"%s",str_dummy);
 printf("HOST NAME IS %s ***\n",str_dummy);
 fclose(fp);
*/
 

 return(str_dummy);
}
char * get_file_name(str)
char *str;
{
 int i,j;
 char* str_dummy; 
 char* test_str;
 test_str=(char *)strstr(str,":");
 if (test_str==NULL) return(NULL);

 str_dummy=(char *)malloc(sizeof(char)*strlen(str));
 j=0;
 for (i=strcspn(str,":")+1;i<strlen(str);i++)
  {
   str_dummy[j]=str[i];
   j++;
  }
 str_dummy[j]=NULL;
 return(str_dummy);
}
int c_load_config(filename,num_node,perf_flag,processor_savedir,control_savedir,sourcedir,ire_sim,fileodl,filestatus,filetime,filesyserror,burst_start,burst_end,sim_eam_nsp,time_file,dump_file,ant_flag)
char *filename,*fileodl,*filestatus,*filetime,*sourcedir,*filesyserror;
int *burst_start,*burst_end,*ire_sim;
int sim_eam_nsp[4];
char *processor_savedir,*control_savedir;
char *perf_flag;
int *num_node;
char *time_file,*dump_file;
int *ant_flag;
{
FILE *fp;
int ireturn,i;
char host[300];
char dump_s[300];

 if ((fp=fopen(filename,"r"))== NULL)
    {
     printf("cannot open %s\n",filename);
     fflush(stdout);
     exit(-1);
    }
fscanf(fp,"%s",host);
fscanf(fp,"%d",num_node);
fscanf(fp,"%s",perf_flag);
fscanf(fp,"%s",processor_savedir);
fscanf(fp,"%s",control_savedir);
fscanf(fp,"%s",sourcedir);
fscanf(fp,"%s",fileodl);
fscanf(fp,"%s",filestatus);
fscanf(fp,"%s",filetime);
fscanf(fp,"%s",filesyserror);
fscanf(fp,"%d",ire_sim);
fscanf(fp,"%d",burst_start);
fscanf(fp,"%d",burst_end);
for (i=0;i<4;i++)
fscanf(fp,"%d",&sim_eam_nsp[i]);
fscanf(fp,"%s",time_file);
fscanf(fp,"%s",dump_file);
fscanf(fp,"%s%d",dump_s,ant_flag);
  /* overide the ant_flag to be 1 */
  *ant_flag = 1; 
fscanf(fp,"%s",dump_s);
fclose(fp);
return(1);
}
int f_load_config(filename,sourcedir,ire_sim,fileodl,filestatus,burst_start,burst_end,sim_eam_nsp,ant_flag)
char *filename,*fileodl,*filestatus,*sourcedir;
int *burst_start,*burst_end,*ire_sim;
int sim_eam_nsp[4];
int *ant_flag;
{
FILE *fp;
char filename_c[256],fileodl_c[256],filestatus_c[256],sourcedir_c[256],filetime_c[256];
char processor_savedir_c[256],control_savedir_c[256],filesyserror_c[256],perf_flag_c[10];
char time_file_c[256],dump_file_c[256];
int ireturn,i;
int num_node;

 str_f_to_c(filename_c,filename,256);
 c_load_config(filename_c,&num_node,perf_flag_c,processor_savedir_c,control_savedir_c,sourcedir_c,ire_sim,fileodl_c,filestatus_c,filetime_c,filesyserror_c,burst_start,burst_end,sim_eam_nsp,time_file_c,dump_file_c,ant_flag);
 str_c_to_f(sourcedir,sourcedir_c,256);
 str_c_to_f(fileodl,fileodl_c,256);
 str_c_to_f(filestatus,filestatus_c,256);

 

 return(1);
}

int    c_transfer_back(pmf_file,ceos_leader_file,image_file,
                      file_pmf,file_ceos_leader,file_image,
                      file_framelet_leader,file_framelet,
                      prod_type,file_config,nbeams,proc_id,file_pmflet)
char *pmf_file,*ceos_leader_file,*image_file;
char *file_pmf,*file_ceos_leader,*file_image,file_framelet_leader[4][60],file_framelet[4][60];
int *prod_type,*nbeams,*proc_id;
char *file_config;
char file_pmflet[4][60];
{
  char pmf_file_c[256],ceos_leader_file_c[256],image_file_c[256];
  char framelet_leader_file_c[4][256],framelet_file_c[4][256];
  char pmflet_file_c[4][256];
/*************/
  char file_pmflet_c[4][60];
  char file_pmf_c[60],file_ceos_leader_c[60],file_image_c[60];
  char file_framelet_leader_c[4][60],file_framelet_c[4][60];
/*************/
  char file_config_c[256]; 
  int i,icode;
  char *host,*input,*output,cmd[500];
  char file_copy_and_cleanup_c[256];
  FILE *fp;
  char dummy_char[10];
  int istat;


  str_f_to_c(file_config_c,file_config,256);
  build_script_name("/home/tmpdisk/",*proc_id,"COPY",file_copy_and_cleanup_c);
  printf("WHAT IS FILENAME %s\n",file_copy_and_cleanup_c);



  istat= unlink(file_copy_and_cleanup_c);
  fp=fopen(file_copy_and_cleanup_c,"w");
/************************************/
  str_f_to_c(file_pmf_c,file_pmf,60);
  str_f_to_c(file_ceos_leader_c,file_ceos_leader,60);
  str_f_to_c(file_image_c,file_image,60);
  str_f_to_c(pmf_file_c,pmf_file,256);
  str_f_to_c(ceos_leader_file_c,ceos_leader_file,256);
  str_f_to_c(image_file_c,image_file,256);
/************************************/
  for (i=0;i<*nbeams;i++)
   {
    str_f_to_c(file_framelet_leader_c[i],file_framelet_leader[i],60);
    str_f_to_c(file_framelet_c[i],file_framelet[i],60);
    str_f_to_c(file_pmflet_c[i],file_pmflet[i],60);
   }

/*  printf("INPUT FILE ******\n");
  printf("%s\n",file_config_c);
  printf("%s\n",file_pmf_c);
  printf("%s\n",file_image_c);
  printf("%s\n",file_ceos_leader_c); */
  for (i=0;i<*nbeams;i++)
   {
/*    printf("%s\n",file_framelet_c[i]);
    printf("%s\n",file_framelet_leader_c[i]);*/
   }
/*  printf("OUTPUT FILE ******\n");
  printf("%s\n",pmf_file_c);
  printf("%s\n",image_file_c);
  printf("%s\n",ceos_leader_file_c); */
   for (i=0;i<*nbeams;i++)
    {
      create_frame_name      (i+1,pmflet_file_c[i],          pmf_file_c);
      create_frame_name      (i+1,framelet_file_c[i],          image_file_c);
      create_frame_name      (i+1,framelet_leader_file_c[i],   ceos_leader_file_c);
/*        printf("%s\n",framelet_file_c[i]);
        printf("%s\n",framelet_leader_file_c[i]);  */
    }

   
   host=get_host_name(pmf_file_c,file_config_c);

   input =file_pmf_c;
   output=get_file_name(pmf_file_c);
   build_rcp_cmd(cmd,host,input,output);
   fprintf(fp,"%s\n",cmd);
   input =file_image_c;
   output=get_file_name(image_file_c);
   build_rcp_cmd(cmd,host,input,output);
   fprintf(fp,"%s\n",cmd);
   input =file_ceos_leader_c;
   output=get_file_name(ceos_leader_file_c);
   build_rcp_cmd(cmd,host,input,output);
   fprintf(fp,"%s\n",cmd);
if (*prod_type!=1)
 {
   for (i=0;i<*nbeams;i++)
    {
     input =(char *)file_framelet_c[i];
     output=get_file_name(framelet_file_c[i]);
   build_rcp_cmd(cmd,host,input,output);
   fprintf(fp,"%s\n",cmd);
     input =(char *)file_framelet_leader_c[i];
     output=get_file_name(framelet_leader_file_c[i]);
   build_rcp_cmd(cmd,host,input,output);
   fprintf(fp,"%s\n",cmd);
     input =(char *)file_pmflet_c[i];
     output=get_file_name(pmflet_file_c[i]);
   build_rcp_cmd(cmd,host,input,output);
   fprintf(fp,"%s\n",cmd);
    }
 }
   
  fclose(fp);

  

 return(1);
}
int      create_frame_name(id,to,from)
int id;
char *to,*from;
{
 char str_num[10];
 int ilast;
    
    sprintf(str_num,"%d",id);
    strcpy(to,from);
    ilast=strlen(to)-1;
    while (to[ilast]!='.')
       ilast--;
    to[ilast-4]  = 'F';
    to[ilast-4-2]= str_num[0];


 return(1);
}

int build_rcp_cmd(cmd,host,input,output)
char* cmd;
char*host,*input,*output;
{
 strcpy(cmd,"rcp ");
 strcat(cmd,input);
 strcat(cmd," ");
 strcat(cmd,host);
 strcat(cmd,":");
 strcat(cmd,output);
 return(1);
}
int build_script_name(path,proc_id,extension,file_name)
char* path;
int proc_id;
char *extension;
char *file_name;
{
 int i;
 char str_num[10];

 strcpy(file_name,path);
 sprintf(str_num,"%d",proc_id);
 strcat(file_name,extension);
 strcat(file_name,".");
 strcat(file_name,str_num);

return(1);


}
int iprintfLog(str)
char *str;
{
 printf("%s\n");
 fflush(stdout);

}
int gen_frameletnames(nbeams,framelet_file,framelet_leader_file,image_file,ceos_leader_file)
int *nbeams;
char *image_file,*ceos_leader_file;
char framelet_file[4][256];
char framelet_leader_file[4][256];
{
 int i;
 char image_file_c[256],ceos_leader_file_c[256];
 char framelet_file_c[4][256];
 char framelet_leader_file_c[4][256];

 str_f_to_c(image_file_c,image_file,256);
 str_f_to_c(ceos_leader_file_c,ceos_leader_file,256);

    for (i=0;i<*nbeams;i++)
    {
      create_frame_name      (i+1,framelet_file_c[i],          image_file_c);
      create_frame_name      (i+1,framelet_leader_file_c[i],   ceos_leader_file_c);
      str_c_to_f(framelet_file[i],framelet_file_c[i],256);
      str_c_to_f(framelet_leader_file[i],framelet_leader_file_c[i],256);
    }



}
