/* SccsId[]= @(#)get_node.c	2.41 3/24/98 */
#include<stdio.h>
#include<fcntl.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include "error.h"
#define MAX_NUM_PROCESSOR  16
#define MAX_NUM_BURST      1600
int userid;

int get_node(in_i_file_type,echo_file,aux_file,rep_file,eph_file,burst_loc_file,topo_file,cali_file,scan_results_file,
file_raw,file_aux,file_rep,file_eph,file_dk,file_dk_temp,file_topo,file_cali,file_scan_results,
file_status_f,file_config_f,in_ire_sim,in_numtask,in_taskid)

char echo_file[256],aux_file[256],burst_loc_file[256],topo_file[256];
char rep_file[256],eph_file[256],cali_file[256],scan_results_file[256];
char file_raw[60],file_aux[60],file_dk[60],file_dk_temp[60],file_topo[60];
char file_rep[60],file_eph[60],file_cali[60],file_scan_results[60];
int* in_numtask,*in_taskid;
char file_status_f[256];
char file_config_f[256];
int *in_ire_sim;
int *in_i_file_type;
{
  int i_file_type;
  int total_byte,nbeam;
  int dk_ptr[MAX_NUM_BURST],dk_size[MAX_NUM_BURST],dk_size_old[MAX_NUM_BURST];
  int dk_ptr_skip;
  int ibegin[MAX_NUM_PROCESSOR];
  int iend[MAX_NUM_PROCESSOR];
  int total_size[MAX_NUM_PROCESSOR];
  int head_size[MAX_NUM_PROCESSOR];
  int icode;
  char *host,*src,*targ;
  char *get_host_name(),*get_file_name();
  int of;
  char dk_file[256];
  int ifd,nbyte,i,byte,ibyte;
  int nbuf[4],nelem,qtype,dontcare,allmsg,nulltask,allgrp;
  char host_name[60],src_dk_loc[256],src_echo[256],src_aux[256];
  char src_topo[256],tar_dk_loc1[60],tar_dk_loc2[60],tar_echo[60];
  char src_eph[256],src_rep[256],src_cali[256],src_scan_results[256];
  char tar_aux[60],tar_topo[60];
  char tar_eph[60],tar_rep[60],tar_cali[60],tar_scan_results[60];
  int istat,iistat,isuccess;
  int numtask,taskid,ire_sim;
  char buff[500];
  char buff1[500];
  char file_status[256];
  char file_config[256];
  int sz,rc,idatatype;
  MPI_Status mpi_status;

  numtask= *in_numtask;
  taskid = *in_taskid;
  ire_sim= *in_ire_sim;
  i_file_type= *in_i_file_type;


  printf(" %d %d \n",numtask,taskid);

  userid = getuid();
  
  str_f_to_c(src_dk_loc ,burst_loc_file,256);
  str_f_to_c(src_echo   ,echo_file,256);
  str_f_to_c(src_aux    ,aux_file,256);
  str_f_to_c(src_topo   ,topo_file,256);
  str_f_to_c(src_rep   , rep_file,256);
  str_f_to_c(src_eph   , eph_file,256);
  str_f_to_c(src_cali   , cali_file,256);
  str_f_to_c(src_scan_results   , scan_results_file,256);

  str_f_to_c(tar_dk_loc1,file_dk_temp,60);
  str_f_to_c(tar_dk_loc2,file_dk,60);
  str_f_to_c(tar_echo   ,file_raw,60);
  str_f_to_c(tar_aux    ,file_aux,60);
  str_f_to_c(tar_topo   ,file_topo,60);
  str_f_to_c(tar_rep   ,file_rep,60);
  str_f_to_c(tar_eph   ,file_eph,60);
  str_f_to_c(tar_cali   ,file_cali,60);
  str_f_to_c(tar_scan_results   ,file_scan_results,60);
  str_f_to_c(file_status   ,file_status_f,256);
  str_f_to_c(file_config   ,file_config_f,256);



  allgrp  = MPI_COMM_WORLD;


/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
if (i_file_type == 1)
{
  dk_ptr_skip=0;
  host=get_host_name(src_eph,file_config);
  src=get_file_name(src_eph);
  targ=tar_eph;
  if ((host==NULL) || (src==NULL)) 
   {
    printerr(src_eph);
    return(ierr_8);
   }
  sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
  fflush(stdout);
  if (taskid==0)
   {
    /* transferring */
    printf(" Open the file %s on the host %s\n",src,host);
    printclog(3,buff);
    istat=transfer(host,src,targ,numtask,total_size,allgrp,2,file_status,dk_ptr_skip);
    printf("***transferring returns %d for eph_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper)) 
      {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
      }
   }
  else
   {
     (void) setreuid(0, userid);
     istat= unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0) 
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(-1,of,allgrp);
     if ((istat>=ierr_lower) && (istat<=ierr_upper)) printerr(targ);
     close(of); 
     (void) setreuid(userid, 0);
   }
/*******************************************/
  if (taskid==0) 
   {
    iistat = istat;
    for (i=1;i<numtask;i++)
     {
         MPI_Recv(&istat,1,MPI_INT,i,0,MPI_COMM_WORLD,&mpi_status);
         if ((istat>=ierr_lower) && (istat<=ierr_upper))
         iistat = istat;
       
     }
   }
  else
   {
          MPI_Send(&istat,1,MPI_INT,0,0,MPI_COMM_WORLD);
   }

  MPI_Bcast(&iistat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((iistat>=ierr_lower) && (iistat<=ierr_upper)) return(iistat);
/*********************************************/
  MPI_Barrier(MPI_COMM_WORLD);
  free(host);
  free(src) ;
}
/***********************************************************************/
if (i_file_type == 2)
{
  dk_ptr_skip=0;
  host=get_host_name(src_rep,file_config);
  src=get_file_name(src_rep);
  targ=tar_rep;
  if ((host==NULL) || (src==NULL))
   {
    printerr(src_rep);
    return(ierr_8);
   }

  sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
  if (taskid==0)
   {
    /* transferring */
    printf(" Open the file %s on the host %s\n",src,host);
    printclog(3,buff);
    istat=transfer(host,src,targ,numtask,total_size,allgrp,2,file_status,dk_ptr_skip);
    printf("***transferring returns %d for rep_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper))
      {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
      }
   }
  else
   {
     (void) setreuid(0, userid);
     istat= unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0) 
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(-1,of,allgrp);
     if ((istat>=ierr_lower) && (istat<=ierr_upper)) printerr(targ);
     close(of); 
     (void) setreuid(userid, 0);
   }
/*******************************************/
  if (taskid==0) 
   {
    iistat = istat;
    for (i=1;i<numtask;i++)
     {
         MPI_Recv(&istat,1,MPI_INT,i,0,MPI_COMM_WORLD,&mpi_status);
         if ((istat>=ierr_lower) && (istat<=ierr_upper))
         iistat = istat;
       
     }
   }
  else
   {
          MPI_Send(&istat,1,MPI_INT,0,0,MPI_COMM_WORLD);
   }

  MPI_Bcast(&iistat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((iistat>=ierr_lower) && (iistat<=ierr_upper)) return(iistat);
/*********************************************/
  MPI_Barrier(MPI_COMM_WORLD);
  free(host);
  free(src) ;
}
/***********************************************************************/
if (i_file_type == 3)
{

  dk_ptr_skip=0;
  host=get_host_name(src_aux,file_config);
  src =get_file_name(src_aux);
  targ=tar_aux;
  if ((host==NULL) || (src==NULL)) 
   {
    printerr(src_aux);
    return(ierr_8);
   }

  sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
  if (taskid==0)
   {
    /* transferring */
    printf(" Open the file %s on the host %s\n",src,host);
    printclog(3,buff);
    istat=transfer(host,src,targ,numtask,total_size,allgrp,2,file_status,dk_ptr_skip);
    printf("***transferring returns %d for aux_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper))
      {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
      }


   }
  else
   {
     (void) setreuid(0, userid);
     istat= unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0) 
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(-1,of,allgrp);
     if ((istat>=ierr_lower) && (istat<=ierr_upper)) printerr(targ);
     close(of); 
     (void) setreuid(userid, 0);
   }
/*******************************************/
  if (taskid==0) 
   {
    iistat = istat;
    for (i=1;i<numtask;i++)
     {
         MPI_Recv(&istat,1,MPI_INT,i,0,MPI_COMM_WORLD,&mpi_status);
         if ((istat>=ierr_lower) && (istat<=ierr_upper))
         iistat = istat;
       
     }
   }
  else
   {
          MPI_Send(&istat,1,MPI_INT,0,0,MPI_COMM_WORLD);
   }

  MPI_Bcast(&iistat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((iistat>=ierr_lower) && (iistat<=ierr_upper)) return(iistat);
/*********************************************/
  MPI_Barrier(MPI_COMM_WORLD);
  free(host);
  free(src) ;
}
/***********************************************************************/
/***********************************************************************/
if (i_file_type == 4)
{

  dk_ptr_skip=0;
  host=get_host_name(src_scan_results,file_config);
  src =get_file_name(src_scan_results);
  targ=tar_scan_results;
  if ((host==NULL) || (src==NULL))
   {
    printerr(src_scan_results);
    return(ierr_8);
   }

  printf("%s %s %s\n",host,src,targ);
  sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
  if (taskid==0)
   {
    /* transferring */
    printf(" Open the file %s on the host %s\n",src,host);
    printclog(3,buff);
    istat=transfer(host,src,targ,numtask,total_size,allgrp,2,file_status,dk_ptr_skip);
    printf("***transferring returns %d for scan_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper))
      {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
      }


   }
  else
   {
     (void) setreuid(0, userid);
     istat= unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0)
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(-1,of,allgrp);
     if ((istat>=ierr_lower) && (istat<=ierr_upper)) printerr(targ);
     close(of);
     (void) setreuid(userid, 0);

   }
/*******************************************/
  if (taskid==0) 
   {
    iistat = istat;
    for (i=1;i<numtask;i++)
     {
         MPI_Recv(&istat,1,MPI_INT,i,0,MPI_COMM_WORLD,&mpi_status);
         if ((istat>=ierr_lower) && (istat<=ierr_upper))
         iistat = istat;
       
     }
   }
  else
   {
          MPI_Send(&istat,1,MPI_INT,0,0,MPI_COMM_WORLD);
   }

  MPI_Bcast(&iistat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((iistat>=ierr_lower) && (iistat<=ierr_upper)) return(iistat);
/*********************************************/
  MPI_Barrier(MPI_COMM_WORLD);
  free(host);
  free(src) ;
}
/***********************************************************************/
if (i_file_type == 5)
{

    dk_ptr_skip=0;
    host=get_host_name(src_cali,file_config);   
    src=get_file_name(src_cali);  
    targ=tar_cali;  
  if ((host==NULL) || (src==NULL))
   {
    printerr(src_cali);
    return(ierr_8);
   }

    sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);  
    if (taskid==0)    
     {     
     /*  transferring   */
      printf(" Open the file %s on the host %s\n",src,host);   
      printclog(3,buff);   
    istat=transfer(host,src,targ,numtask,total_size,allgrp,2,file_status,dk_ptr_skip);
    printf("***transferring returns %d for cali_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper))
      {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
      }


     }    
    else   
     {    
     (void) setreuid(0, userid);
     istat= unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0)
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(-1,of,allgrp);
     if ((istat>=ierr_lower) && (istat<=ierr_upper)) printerr(targ);
     close(of);
     (void) setreuid(userid, 0);
     }    
/*******************************************/
  if (taskid==0) 
   {
    iistat = istat;
    for (i=1;i<numtask;i++)
     {
         MPI_Recv(&istat,1,MPI_INT,i,0,MPI_COMM_WORLD,&mpi_status);
         if ((istat>=ierr_lower) && (istat<=ierr_upper))
         iistat = istat;
       
     }
   }
  else
   {
          MPI_Send(&istat,1,MPI_INT,0,0,MPI_COMM_WORLD);
   }

  MPI_Bcast(&iistat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((iistat>=ierr_lower) && (iistat<=ierr_upper)) return(iistat);
/*********************************************/
  MPI_Barrier(MPI_COMM_WORLD);
  free(host);
  free(src) ;
}
/******************************************************************************/
/***********************************************************************/
if (i_file_type == 6)
{

  dk_ptr_skip=0;
  host=get_host_name(src_dk_loc,file_config);
  src =get_file_name(src_dk_loc);
  targ=tar_dk_loc1;
  if ((host==NULL) || (src==NULL))
   {
    printerr(src_dk_loc);
    return(ierr_8);
   }

  sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
  if (taskid==0)
   {
    /* transferring */
    printf(" Open the file %s on the host %s\n",src,host);
    printclog(3,buff);
    istat=transfer(host,src,targ,numtask,total_size,allgrp,2,file_status,dk_ptr_skip);
    printf("***transferring returns %d for dk_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper))
      {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
      }

   }
  else
   {
     (void) setreuid(0, userid);
     istat= unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0) 
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(-1,of,allgrp);
     close(of);
     (void) setreuid(userid, 0);
   }
  MPI_Bcast(&istat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((istat>=ierr_lower) && (istat<=ierr_upper)) return(istat);
}
/******************************************************************************/
/***********************************************************************/
if (i_file_type == 7)
{

  targ=tar_dk_loc1;
if (ire_sim==0)
  istat=read_dk_sim(targ,numtask,&total_byte,&nbeam,ibegin,iend,total_size,dk_ptr,dk_size,&dk_ptr_skip,dk_size_old);
else
  istat=read_dk_real(targ,numtask,&total_byte,&nbeam,ibegin,iend,total_size,dk_ptr,dk_size,&dk_ptr_skip,dk_size_old);
  if ((istat>=ierr_lower) && (istat<=ierr_upper)) return(istat);

  printf("BURSTS %d to %d\n",ibegin[taskid],iend[taskid]);
  fflush(stdout);


  strcpy(dk_file,tar_dk_loc2);
  printf("Create %s file for each node\n",dk_file);
  (void) setreuid(0, userid);
  istat= unlink(dk_file);
  ifd = open(dk_file, O_WRONLY|O_CREAT|O_TRUNC, 0755);
  if (ifd < 0) 
   {
    sprintf(buff1,"failed to open %s",dk_file);
    printclog(3,buff1);
    printerr(dk_file);
    return(ierr_2);
   }
  byte=(iend[taskid]-ibegin[taskid]+1)*4;
  ibyte=write(ifd, &dk_ptr[ibegin[taskid]-1], byte);
  if (ibyte < 0) 
   {
    sprintf(buff1,"failed to write to %s",dk_file);
    printclog(3,buff1);
    printerr(dk_file);
    return(ierr_3);
   }
/* dump_more_dk(ifd,dk_ptr,ibegin,iend,taskid,numtask,nbeam,dk_size_old,head_size);*/
  close(ifd);
  (void) setreuid(userid, 0);
  MPI_Barrier(MPI_COMM_WORLD);
/***************/
  host=get_host_name(src_echo,file_config);
  src =get_file_name(src_echo);
  targ=tar_echo;
  if ((host==NULL) || (src==NULL))
   {
    printerr(src_echo);
    return(ierr_8);
   }

  sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
printf("TOTAL BYTE FOR ECHO IS %d\n",total_byte);
  fflush(stdout);

  if (taskid==0)
   {
    /* transferring */
    printf(" Open the file %s on the host %s\n",src,host);
    printclog(3,buff);
    istat=transfer(host,src,targ,numtask,total_size,allgrp,1,file_status,dk_ptr_skip);
    printf("***transferring returns %d for echo_file\n",istat);
    fflush(stdout);
    if ((istat>=ierr_lower) && (istat<=ierr_upper))
     {
      idatatype=1;
      for (i=1;i<numtask;i++)
       {
            sz=0;
            MPI_Send(&sz,1,MPI_INT,i,idatatype,MPI_COMM_WORLD);
       }
      }
   }
  else
   {
     (void) setreuid(0, userid);
     istat=unlink(targ);
     of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755);
     if (of < 0) 
      {
       sprintf(buff1,"failed to open %s",targ);
       printclog(3,buff1);
       printerr(targ);
       return(ierr_2);
      }
     printclog(3,buff);
     istat=receive(taskid,of,allgrp);
     if ((istat>=ierr_lower) && (istat<=ierr_upper)) printerr(targ);
     close(of);
     (void) setreuid(userid, 0);
   }

/*******************************************/
  if (taskid==0) 
   {
    iistat = istat;
    for (i=1;i<numtask;i++)
     {
         MPI_Recv(&istat,1,MPI_INT,i,0,MPI_COMM_WORLD,&mpi_status);
         if ((istat>=ierr_lower) && (istat<=ierr_upper))
         iistat = istat;
       
     }
   }
  else
   {
          MPI_Send(&istat,1,MPI_INT,0,0,MPI_COMM_WORLD);
   }

  MPI_Bcast(&iistat,1,MPI_INT,0,MPI_COMM_WORLD);
  if ((iistat>=ierr_lower) && (iistat<=ierr_upper)) return(iistat);
/*********************************************/

  MPI_Barrier(MPI_COMM_WORLD);
/*  dump_more_echo(targ,taskid,numtask,head_size);*/
  free(host);
  free(src) ;
}
/***********************************************************************/
 /*** do host*/
/*strcpy(host,get_host_name(src_topo,file_config));*/
/*strcpy(src,get_file_name(src_topo));*/
/*strcpy(targ,tar_topo);*/
/*sprintf(buff,"Tranfering %s from %s to %s on the processing node",&src,&host,&targ);*/

/*if (taskid==0)*/
/*{*/
/*istat=transfer(host,src,targ,NULL,NULL,allgrp,0,file_status,dk_ptr_skip);*/
/*}*/
/*if (taskid==0)*/
/* {  */
    /* transferring */
/*  printf(" Open the file %s on the host %s\n",src,host);*/
/*  printclog(3,buff); */
/*  istat=transfer(host,src,targ,NULL,NULL,allgrp,2,file_status,dk_ptr_skip);*/

/* }*/
/*else */
/* {  */
/*   (void) setreuid(0, userid);  */
/*   istat= unlink(targ);        */
/*   of = open(targ, O_WRONLY|O_CREAT|O_TRUNC, 0755); */
/*   if (of < 0)   */
/*    {           */
/*     sprintf(buff1,"failed to open %s",targ);*/
/*     printclog(3,buff1); */
/*     return(ierr_2);  */
/*    }                  */
/*   printclog(3,buff);  */
/*   istat=receive(-1,of,allgrp); */
/*   close(of);                   */
/*   (void) setreuid(userid, 0);   */
/* }                               */
/*if ((istat>=ierr_lower) && (istat<=ierr_upper)) return(istat);*/
/***********************************************************************/



  return(iok);

}
/*********************************************************************/
int read_dk_real(dk_name,numtask,total_byte,nbeam_return,ibegin,iend,total_size,new_dk_ptr,new_dk_size,dk_ptr_skip,dk_size)
char *dk_name;
int numtask,*total_byte,*nbeam_return,*dk_ptr_skip;
int *ibegin,*iend,*total_size;
int *new_dk_ptr,*new_dk_size;
int dk_size[1600];
{
int input;
int isuccess,nbyte,nset,ptr,i,ii,ifirst;
int nset_cpu[16],nset_left,burst_left;
char buff1[300];
int total_b;
int nbeam;

  isuccess=ssp_get_bof(dk_name,&total_b,&nbeam,dk_ptr_skip,dk_size);

  *nbeam_return=nbeam;

  nset=total_b/(nbeam*2);
  burst_left=total_b%(nbeam*2);
  for (i=0;i<numtask;i++)
   {
    nset_cpu[i]=nset/numtask;
   }
  nset_left=nset%numtask;

  if (nset_left != 0)
   {
    for (i=0;i<nset_left;i++)
     nset_cpu[i]=nset_cpu[i]+1;
   }

  ibegin[0]=1;
  iend  [0]=nset_cpu[0];
  for (i=1;i<numtask;i++)
   {
    ibegin[i]=iend[i-1]+1;
    iend  [i]=ibegin[i]+nset_cpu[i]-1;
   }
  for (i=0;i<numtask;i++)
   {
    ibegin[i]=(ibegin[i]-1)*(nbeam*2)+1;
    iend  [i]= iend[i]     *(nbeam*2);
    if (i==(numtask-1)) iend[i]= iend[i]+burst_left;
   }
  


 /***************************************************/ 
  *total_byte=0;
  for (ii=0;ii<numtask;ii++)
   {
    total_size[ii]=0;
    for (i=ibegin[ii];i<=iend[ii];i++)
    {
     total_size[ii]= total_size[ii] + dk_size[i-1];
    }
   *total_byte= *total_byte + total_size[ii];
/*    printf("%d %d %d %d\n",ii,ibegin[ii],iend[ii],total_size[ii]);*/
   }
  
 /***************************************************/ 

  for (ii=0;ii<total_b;ii++)
   new_dk_ptr[ii]=0;
  for (ii=0;ii<numtask;ii++)
   {
    ifirst=1;
    for (i=(ibegin[ii]-1);i<=(iend[ii]-1);i++)
    {
     if (ifirst==1) 
      {
       new_dk_ptr[i]=32;
       new_dk_size[i]=dk_size[i]-32;
       ifirst=0;
      }
     else
      {
       new_dk_ptr[i]=(new_dk_ptr[i-1]-32) +  dk_size[i-1] +32;
       new_dk_size[i]=dk_size[i]-32;
      }
    }
   }
  


  
 return(iok);

}
/********************************************************************/
int read_dk_sim(dk_name,numtask,total_byte,nbeam_return,ibegin,iend,total_size,new_dk_ptr,new_dk_size,dk_ptr_skip,dk_size)
char *dk_name;
int numtask,*total_byte,*nbeam_return,*dk_ptr_skip;
int *ibegin,*iend,*total_size;
int *new_dk_ptr,*new_dk_size;
int *dk_size;
{
int input;
int isuccess,nbyte,nset,ptr,i,ii,ifirst;
int nset_cpu[16],nset_left,burst_left;
char buff1[300];
int total_b;
int nbeam;

  *dk_ptr_skip=0;
  input  =copen_r(dk_name);
  if (input < 0)
   {
    sprintf(buff1,"failed to open %s",dk_name);
    printclog(3,buff1);
    printerr(dk_name);
    return(ierr_2);
   }
  ptr=0;
  nbyte=sizeof(int);
  isuccess=cread(&input,&total_b,&nbyte,&ptr);
  if (isuccess < 0) 
   {
    sprintf(buff1,"failed to read from %s",dk_name);
    printclog(3,buff1);
    printerr(dk_name);
    return(ierr_4);
   }
  ptr=4;
  isuccess=cread(&input,&nbeam  ,&nbyte,&ptr);
  if (isuccess < 0) 
   {
    sprintf(buff1,"failed to read from %s",dk_name);
    printclog(3,buff1);
    printerr(dk_name);
    return(ierr_4);
   }
  *nbeam_return=nbeam;
  ptr=8;
  nbyte=sizeof(int)*total_b;
  dk_size=(int *)malloc(sizeof(int)*total_b);
  isuccess=cread(&input,dk_size,&nbyte,&ptr);
  if (isuccess < 0) 
   {
    sprintf(buff1,"failed to read from %s",dk_name);
    printclog(3,buff1);
    printerr(dk_name);
    return(ierr_4);
   }

  nset=total_b/(nbeam*2);
  burst_left=total_b%(nbeam*2);
  for (i=0;i<numtask;i++)
   {
    nset_cpu[i]=nset/numtask;
   }
  nset_left=nset%numtask;

  if (nset_left != 0)
   {
    for (i=0;i<nset_left;i++)
     nset_cpu[i]=nset_cpu[i]+1;
   }

  ibegin[0]=1;
  iend  [0]=nset_cpu[0];
  for (i=1;i<numtask;i++)
   {
    ibegin[i]=iend[i-1]+1;
    iend  [i]=ibegin[i]+nset_cpu[i]-1;
   }
  for (i=0;i<numtask;i++)
   {
    ibegin[i]=(ibegin[i]-1)*(nbeam*2)+1;
    iend  [i]= iend[i]     *(nbeam*2);
    if (i==(numtask-1)) iend[i]= iend[i]+burst_left;
   }


 /***************************************************/ 
  *total_byte=0;
  for (ii=0;ii<numtask;ii++)
   {
    total_size[ii]=0;
    for (i=ibegin[ii];i<=iend[ii];i++)
    {
     total_size[ii]= total_size[ii] + dk_size[i-1];
    }
    *total_byte= *total_byte+total_size[ii];
    printf("%d %d %d %d\n",ii,ibegin[ii],iend[ii],total_size[ii]);
   }
  
 /***************************************************/ 

  for (ii=0;ii<total_b;ii++)
   new_dk_ptr[ii]=0;
  for (ii=0;ii<numtask;ii++)
   {
    ifirst=1;
    for (i=(ibegin[ii]-1);i<=(iend[ii]-1);i++)
    {
     if (ifirst==1) 
      {
       new_dk_ptr[i]=0;
       new_dk_size[i]=dk_size[i];
       ifirst=0;
      }
     else
      {
       new_dk_ptr[i]=new_dk_ptr[i-1]+  dk_size[i-1];
       new_dk_size[i]=dk_size[i];
      }
    }
   }
  


  
 return(iok);

}
int  dump_more_dk(ifd,dk_ptr,ibegin,iend,taskid,numtask,nbeam,dk_size,head_size)
int ifd;
int *dk_ptr;
int *dk_size;
int *iend;
int *ibegin;
int taskid,numtask,nbeam;
int *head_size;
{
 int ii,i;
 int iptr,iburst;
 int sum;
 int ibyte,isize;

 if (taskid!=(numtask-1))
  {
        iburst= iend[taskid]-1;
        printf("AT BURST %d %d\n",dk_ptr[iburst],iburst);
        iptr=dk_ptr[iburst];
        for (i=1;i<=nbeam;i++)
        {
          iptr=  iptr-32+ dk_size[iburst+i-1] +32;
          ibyte=write(ifd, &iptr, 4);
          printf("ADDING %d %d %d\n",iptr,iburst+i,ibyte);
         
        }
  }

 for (ii=0;ii<numtask;ii++)
 {
  iburst= ibegin[ii]-1;
  sum=0;
  for (i=1;i<=nbeam;i++)
   {
    sum=sum+dk_size[iburst+i-1];
   }
  head_size[ii]=sum;
 }



 return(1);
}
int  dump_more_echo(targ,taskid,numtask,head_size)
 int taskid,numtask;
 int *head_size;
 char *targ;
{
 char *file_name[8];
 char str_num[10];
 int of,inf,i;
 char *data;

 for (i=0;i<8;i++)
 {
  file_name[i]=(char *)malloc(sizeof(char)*100);
  strcpy(file_name[i],"/spdata/people/jwtun/HEAD");
  sprintf(str_num,"%d",i);
  strcat(file_name[i],".");
  strcat(file_name[i],str_num);
 }

 of = open(file_name[taskid], O_WRONLY|O_CREAT|O_TRUNC, 0755);
 inf = open(targ     , 0);
 data=(char *)malloc(sizeof(char)*head_size[taskid]);

 read (inf, data, head_size[taskid]);
 write(of , data, head_size[taskid]);


 free(data);
 close(of);
 close(inf);


 /************************appending**********/

 if (taskid!=(numtask-1))
  {
   printf("appending to %s from %s %d bytes\n",targ,file_name[taskid+1],head_size[taskid+1]);
   of  = open(targ, 2);
   lseek(of,0,2);
   inf = open(file_name[taskid+1], 0);
   data=(char *)malloc(sizeof(char)*head_size[taskid+1]);
   read (inf, data, head_size[taskid+1]);
   write(of , data, head_size[taskid+1]);
 
  }

 

  
 return(1);

}
