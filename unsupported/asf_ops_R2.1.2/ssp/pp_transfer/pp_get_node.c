/* SccsId[]= @(#)pp_get_node.c	2.41 3/24/98 */
static char sccsid_pp_get_node[]= "@(#)PPpp_get_node.c:2.41";

#include<stdio.h>
#include<fcntl.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include "error.h"
#include "mpi.h"
int userid;

int pp_get_node(in_i_file_type,echo_file,aux_file,rep_file,eph_file,burst_loc_file,topo_file,cali_file,scan_results_file,
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
  int icode;
  char *host,*src,*targ;
  char *get_host_name(),*get_file_name();
  int nbuf[4],nelem,qtype,dontcare,allmsg,nulltask,allgrp;
  char host_name[60],src_dk_loc[256],src_echo[256],src_aux[256];
  char src_topo[256],tar_dk_loc1[60],tar_dk_loc2[60],tar_echo[60];
  char src_eph[256],src_rep[256],src_cali[256],src_scan_results[256];
  char tar_aux[60],tar_topo[60];
  char tar_eph[60],tar_rep[60],tar_cali[60],tar_scan_results[60];
  int istatus,iistat,isuccess;
  int numtask,taskid,ire_sim;
  char buff[500];
  char file_status[256];
  char file_config[256];
  int sz,rc,idatatype;
  MPI_Status mpi_status;

  numtask= *in_numtask;
  taskid = *in_taskid;
  ire_sim= *in_ire_sim;
  i_file_type= *in_i_file_type;


  printf(" %d %d \n",numtask,taskid);
  fflush(stdout);

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
/***********************************************************************/
switch(i_file_type)
{
	case 1: host = get_host_name(src_eph,file_config);
		src = get_file_name(src_eph);
		targ=tar_eph;
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_eph);
    		return(ierr_8);
		}
		break; 
	case 2: host = get_host_name(src_rep,file_config);
		src = get_file_name(src_rep);
		targ = tar_rep;
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_rep);
    		return(ierr_8);
		}
		break;
	case 3: host = get_host_name(src_aux,file_config);
		src = get_file_name(src_aux);
		targ = tar_aux;
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_aux);
    		return(ierr_8);
		}
		break;
	case 4: host = get_host_name(src_scan_results,file_config);
		src = get_file_name(src_scan_results);
		targ = tar_scan_results;
    		/* printerr(src); */
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_scan_results);
    		return(ierr_8);
		}
		break; 
	case 5: host = get_host_name(src_cali,file_config);
		src = get_file_name(src_cali);
		targ = tar_cali;
/*
    		printerr(src_cali);
    		printerr(tar_cali);
*/
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_cali);
    		return(ierr_8);
		}
		break;
/*
	case 6: host = get_host_name(src_dk_loc,file_config);
		src = get_file_name(src_dk_loc);
		targ = tar_dk_loc1;
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_dk_loc);
    		return(ierr_8);
		}
		break;
*/
	case 6: host = get_host_name(src_echo,file_config);
		src = get_file_name(src_echo);
		targ = tar_echo;
  		if ((host==NULL) || (src==NULL)) {
    		printerr(src_echo);
    		return(ierr_8);
		}
		break;
	default:
		break;
}

 sprintf(buff,"Transfering %s from %s to %s on the processing node",src,host,targ);
 printf("src = %s targ = %s\n", src, targ);
 fflush(stdout);
 istatus=transfer(host,src,targ,numtask,taskid, file_status, i_file_type);
 printf("after calling transfer istatus = %d\n", istatus);
 fflush(stdout);
 free(host);
 free(src) ;
 printf("before return istatus = %d\n", istatus);
 return(istatus);
}
