/* SccsId[]= @(#)rcp_ssp2.c	2.41 3/24/98 */
/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rcp.c	5.11 (Berkeley) 9/22/88";
#endif /* not lint */

/*
 * rcp
 */
#include <fcntl.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <ctype.h>
#include <netdb.h>
#include <errno.h>
#include "error.h"
/*#include <unistd.h>*/

int	rem;
char	*colon(), *index(), *rindex(), *malloc(), *strcpy();
int	errs;
int	lostconn();
int	errno;
char	*sys_errlist[];
int	iamremote, targetshouldbedirectory;
int	iamrecursive;
int	pflag;
struct	passwd *pwd;
struct	passwd *getpwuid();
int	userid;
int	port;

struct buffer {
	int	cnt;
	char	*buf;
} *allocbuf();

/*VARARGS*/
int	error();

#define	ga()	 	(void) write(rem, "", 1)


int transfer(host,src,targ,numtask,ipsize,allgrp,iflag,file_status,bytes_skip)
	char *targ, *host, *src;
        int numtask,allgrp;
        int *ipsize;
        int iflag;
        char *file_status;
        int bytes_skip;
{
	char *suser, *tuser, *thost;
	int i;
	char buf[BUFSIZ], cmd[16];
	struct servent *sp;
        int istatus;
        char buff[300];
        int isocket_open,itry;

        
        printf("%s %s %s %s\n",host,src,targ,file_status);
        fflush(stdout);

       
	sp = getservbyname("shell", "tcp");
	if (sp == NULL) {
		sprintf(buff, "rcp: shell/tcp: unknown service");
                printclog(3,buff);
		return(ierr_6);
	}
	port = sp->s_port;
	pwd = getpwuid(userid = getuid());
	if (pwd == 0) {
		sprintf(buff, "user id is not valid");
                printclog(3,buff);
		return(ierr_6);
	}


	rem = -1;

        iamremote= 0;
        iamrecursive = 0;
        targetshouldbedirectory = 0;
        pflag = 0;
	(void) sprintf(cmd, "rcp%s%s%s",
	    iamrecursive ? " -r" : "", pflag ? " -p" : "", 
	    targetshouldbedirectory ? " -d" : "");

  	(void) signal(SIGPIPE, lostconn);

        suser = pwd->pw_name;


        (void) sprintf(buf, "%s -f %s", cmd, src);

        printf("the host=%s ***\n",host);
        printf("the cmd=%s ***\n",cmd);
        printf("the src=%s ***\n",src);
        printf("the buf=%s ***\n",buf);
        printf(" the port=%d ***\n",port);
        printf(" the pw_name=%s ***\n",pwd->pw_name);
        printf(" the remote user=%s ***\n",suser);

        isocket_open=0;
        itry        =0;
        while (isocket_open==0)
         {
           printf("^^^^^^^^^^^ real id is %d; effective is %d\n",getuid(),geteuid());
           fflush(stdout);
           printf("BEFORE RCMD %s %d %s %s\n",host,port,pwd->pw_name,suser);
           printf("%s %d\n",buf,strlen(buf));
           rem = rcmd(&host, port, pwd->pw_name, suser, buf, 0);
           printf("AFTER RCMD %s %d %s %s\n",host,port,pwd->pw_name,suser);
           printf("%s %d\n",buf,strlen(buf));
           if (rem== -1)
            { 
                sprintf(buff,"%s",description_str(ssp2_msg[ierr_6]));
                printclog(3,buff);
                return(ierr_6);
            }
           else
            {
             isocket_open=1;
             printf("SOCKET OPENED %d\n",rem);
             fflush(stdout);
            }
         }
        (void) setreuid(0, userid);
        istatus=sink(targ,numtask,ipsize,allgrp,iflag,file_status,bytes_skip,src);
        (void) setreuid(userid, 0);
        close(rem);
          printf("SOCKET CLOSED;DONE TRANSFERRING\n");
          fflush(stdout);
          rem= -1;
        
   
        return(istatus);
}

verifydir(cp)
	char *cp;
{
	struct stat stb;

	if (stat(cp, &stb) >= 0) {
		if ((stb.st_mode & S_IFMT) == S_IFDIR)
			return;
		errno = ENOTDIR;
	}
	error("rcp: %s: %s.\n", cp, sys_errlist[errno]);
	exit(1);
}

char *
colon(cp)
	char *cp;
{

	while (*cp) {
		if (*cp == ':')
			return (cp);
		if (*cp == '/')
			return (0);
		cp++;
	}
	return (0);
}




response()
{
	char resp, c, rbuf[BUFSIZ], *cp = rbuf;

	if (read(rem, &resp, 1) != 1)
		lostconn();
	switch (resp) {

	case 0:				/* ok */
		return (0);

	default:
		*cp++ = resp;
		/* fall into... */
	case 1:				/* error, followed by err msg */
	case 2:				/* fatal error, "" */
		do {
			if (read(rem, &c, 1) != 1)
				lostconn();
			*cp++ = c;
		} while (cp < &rbuf[BUFSIZ] && c != '\n');
		if (iamremote == 0)
			(void) write(2, rbuf, cp - rbuf);
		errs++;
		if (resp == 1)
			return (-1);
		exit(1);
	}
	/*NOTREACHED*/
}

lostconn()
{
  char buff[300];

	if (iamremote == 0)
		sprintf(buff, "rcp: lost connection");
        printclog(3,buff); 
	exit(ierr_7);
}

int sink(output_name,numtask,ipsize,allgrp,iflag,file_status,bytes_skip,input_name)
	char *output_name;
	char *input_name;
        int numtask;
        int *ipsize;
        int allgrp;
        int iflag;
        char *file_status;
        int bytes_skip;
{
        char buff[300];
        int ipsize_return;
        int ip;
        int  k;
	off_t i, j;
	char *targ, *whopp, *cp;
	int of, mode, wrerr, exists, first, count, amt, size;
	char *bp;
	static struct buffer buffer;
	struct stat stb;
	int targisdir = 0;
	int mask = umask(0);
	char *myargv[1];
	char cmdbuf[BUFSIZ], nambuf[BUFSIZ];
	int setimes = 0;
	struct timeval tv[2];
        int istat;
        int total_byte,bytes_cum;
#define atime	tv[0]
#define mtime	tv[1]
#define	SCREWUP(str)	{ whopp = str; goto screwup; }
        int iremain_size;


        printf("SOCKET OPENED;TRANSFERRING\n");
        fflush(stdout);

	if (!pflag)
		(void) umask(mask);
        targ = output_name;
	ga();
        targisdir=0; 


	cp = cmdbuf;
	if (read(rem, cp, 1) <= 0){
          printf("NOTHING COME OVER TO THE SOCKET: DONE\n");
          fflush(stdout);
	   return(iok);
        }
	if (*cp++ == '\n')
           SCREWUP("unexpected '\\n'");
        do {
           if (read(rem, cp, 1) != 1)
              SCREWUP("lost connection");
	} while (*cp++ != '\n');
	*cp = 0;
	if (cmdbuf[0] == '\01' || cmdbuf[0] == '\02') {
     	   if (iamremote == 0)
              (void) write(2, cmdbuf+1, strlen(cmdbuf+1));
           if (cmdbuf[0] == '\02')
             {
              sprintf(buff,"%s",description_str(ssp2_msg[ierr_16]));
              printclog(3,buff);
	      return(ierr_16);
             }
           errs++;
        }
	*--cp = 0;
	cp = cmdbuf;
        if (*cp == 'E') {
                        ga();
                        return;
                }


#define getnum(t) (t) = 0; while (isdigit(*cp)) (t) = (t) * 10 + (*cp++ - '0');
	if (*cp != 'C' && *cp != 'D') {
		/*
		 * Check for the case "rcp remote:foo\* local:bar".
		 * In this case, the line "No match." can be returned
		 * by the shell before the rcp command on the remote is
		 * executed so the ^Aerror_message convention isn't
		 * followed.
		 */
                sprintf(buff,"no such file or directory: %s",input_name);
                printclog(3,buff);
                printerr(input_name);
		return(ierr_8);
/*		if (first) {                       */
/*			error("%s\n", cp);         */
/*			return(ierr_8);            */
/*		}                                  */
/*		SCREWUP("expected control record");*/
	}
	cp++;
	mode = 0;
	for (; cp < cmdbuf+5; cp++) {
		if (*cp < '0' || *cp > '7')
			SCREWUP("bad mode");
		mode = (mode << 3) | (*cp - '0');
	}
	if (*cp++ != ' ')
		SCREWUP("mode not delimited");
	size = 0;
	while (isdigit(*cp))
		size = size * 10 + (*cp++ - '0');
	if (*cp++ != ' ')
		SCREWUP("size not delimited");
        /* the name for local files */

	(void) strcpy(nambuf, targ);
	exists = stat(nambuf, &stb) == 0;
/*****************************************************************************/

	ga();
        printf(" Open the file %s on the respective nodes\n",nambuf);
        istat= unlink(nambuf);
        if ((of = open(nambuf, O_WRONLY | O_CREAT | O_TRUNC, 0755)) < 0) {
/*	if ((of = open(nambuf, O_WRONLY|O_CREAT, mode)) < 0) {       */
                sprintf(buff,"trouble opening the %s",nambuf);
                printclog(3,buff);
		error("rcp: %s: %s\n", nambuf, sys_errlist[errno]);
                printerr(nambuf);
                return(ierr_2);
	}
	if (exists && pflag)
		(void) fchmod(of, mode);
        bp = (char *)malloc(sizeof(char)*(BUFSIZ+1));
        if (bp == NULL) 
          {
                        sprintf(buff,"rcp: malloc: out of memory");
                        printclog(3,buff);
                        return(ierr_5);
           }
    wrerr=0;

 if (bytes_skip != 0)
  {
     cp = cmdbuf;
     read(rem, cp, bytes_skip);
     printf("SKIPPING DATA TRANSFER %d number of bytes\n",bytes_skip);
     fflush(stdout);
  }
 if (iflag==1)
  {
    total_byte=0;
    for(ip=0;ip<numtask;ip++)
     total_byte=total_byte+ipsize[ip];
    bytes_cum=0;
    for(ip=0;ip<numtask;ip++)
      {
        sprintf(buff,"Moving %d bytes to node %d",ipsize[ip],ip);
        printclog(3,buff);
        ipsize_return=load(rem,bp,ip,ipsize[ip],of,BUFSIZ,wrerr,allgrp);
        bytes_cum=bytes_cum+ipsize[ip];
        printcstat(file_status,1,(float) ((1.0*bytes_cum/total_byte)*100.0  ));
	 if (ip==0) close(of);
        if (ipsize_return!=ipsize[ip])
         {
          sprintf(buff,"trouble loading processor %d's segment of the data through the socket",ip);
          printclog(3,buff);
          return(ierr_9);
         }
      }
    total_byte=total_byte+bytes_skip;
    printf("total bytes read %d; total bytes available %d\n",total_byte,size);
   fflush(stdout);
    if (total_byte < size)
      {
       cp = cmdbuf;
       iremain_size= size-total_byte;
       sprintf(buff,"WARNING:total bytes read %d; total bytes available %d",total_byte,size);
       printclog(3,buff);
       printf("READING THE REMAING BYTES %d\n",iremain_size);
       fflush(stdout);
       for (i = 0; i < iremain_size; i += BUFSIZ)
          {
                amt = BUFSIZ;
                if (i + amt > iremain_size)
                        amt = iremain_size - i;
                do {
                        j = read(rem, cp, amt);
                        amt -= j;
                        cp += j;
                    } while (amt > 0);
                cp=cmdbuf;
            }
       }
   }
 if (iflag==2)
  {
        sprintf(buff,"Moving %d bytes to all nodes",size);
        printclog(3,buff);
        ipsize_return=load(rem,bp,-1,size,of,BUFSIZ,wrerr,allgrp);
/*        bytes_cum=bytes_cum+size;*/
/*        printcstat(file_status,1,(float) ((bytes_cum/2000000000.0)*100.0  )); */
        close(of);
        if (ipsize_return!=size)
         { 
          sprintf(buff,"trouble loading full segment of the data through the socket");
          printclog(3,buff);
          return(ierr_9);
         }
  }

/*****************************************************************************/
 free(bp);
	(void) response();
        if(wrerr)
         {
            sprintf(buff,"rcp: %s: %s\n",nambuf,sys_errlist[errno]);
            printclog(3,buff);
            return(ierr_9);
         }
        else
         {
            ga();
         }
        return(iok);

screwup:
	sprintf(buff,"rcp: protocol screwup: %s", whopp);
        printclog(3,buff);
	return(ierr_16);
}

struct buffer *
allocbuf(bp, fd, blksize)
	struct buffer *bp;
	int fd, blksize;
{
	struct stat stb;
	int size;

	if (fstat(fd, &stb) < 0) {
		error("rcp: fstat: %s\n", sys_errlist[errno]);
		return ((struct buffer *)0);
	}
	size = roundup(stb.st_blksize, blksize);
	if (size == 0)
		size = blksize;
	if (bp->cnt < size) {
		if (bp->buf != 0)
			free(bp->buf);
		bp->buf = (char *)malloc((unsigned) size);
		if (bp->buf == 0) {
			error("rcp: malloc: out of memory\n");
			return ((struct buffer *)0);
		}
	}
	bp->cnt = size;
	return (bp);
}

/*VARARGS1*/
error(fmt, a1, a2, a3, a4, a5)
	char *fmt;
	int a1, a2, a3, a4, a5;
{
	char buf[BUFSIZ], *cp = buf;

	errs++;
	*cp++ = 1;
  	(void) sprintf(cp, fmt, a1, a2, a3, a4, a5);
	(void) write(rem, buf, strlen(buf));
	if (iamremote == 0)
		(void) write(2, buf+1, strlen(buf+1));  
}

usage()
{
	fputs("usage: rcp  remote:f1 f2\n", stderr);
	exit(1);
}
/***********************************************/
int broadcast_read(ip,of, bp, allgrp,istat) 
char *bp;
int of;
int ip;
int allgrp;
int *istat;
 {
int rc,source;
int type;
int nbytes;
int numwrote;
int sz; 
MPI_Status   mpi_status;

source=0;
type =1;

 if (ip== -1)
  {
   MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
   if (sz==0) return(1);
   MPI_Bcast(bp,sz,MPI_CHAR,0,MPI_COMM_WORLD);
   numwrote=write(of,bp,sz); 
   if (numwrote!=sz)
    {
      *istat=  ierr_3;
    }
  }
 else
  {
    MPI_Recv(&sz,1,MPI_INT,source,type,MPI_COMM_WORLD,&mpi_status);
    if (sz==0) return(1);
    MPI_Recv(bp,sz,MPI_CHAR,source,type,MPI_COMM_WORLD,&mpi_status);
    numwrote=write(of,bp,sz); 
    if (numwrote!=sz)
    {
      *istat=  ierr_3;
    }
  }

  return(0);
 }
int     broadcast_write(ip,of, ptr, count,allgrp)
char *ptr;
int count;
int ip;
int of;
int allgrp;
   {
int numsend;
int idatatype;
int rc;
int sz;

     idatatype=1;

     if (ip== -1)
      {
         sz     =count;
         MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
         numsend=write(of,ptr,sz); 
         numsend=sz;
         MPI_Bcast(ptr,sz,MPI_CHAR,0,MPI_COMM_WORLD);
      }
     else
      {
       
       if (ip==0)
        {
         numsend=write(of,ptr,count); 
        }
       else
        {
         sz     =count;
         MPI_Send(&sz,1,MPI_INT,ip,idatatype,MPI_COMM_WORLD);
         MPI_Send(ptr,sz,MPI_CHAR,ip,idatatype,MPI_COMM_WORLD);
         numsend=count;
        }
      }



    return(numsend);
   
   }


int receive(ip,of,allgrp)
int ip;
int of;
int allgrp;
{
int i;
int amt,amt_return;
char *bp;
char buff[300];
int done;
int istat;


      bp = (char *)malloc((unsigned) BUFSIZ);
      if (bp == 0)
          {
                        sprintf(buff,"rcp: malloc: out of memory");
                        printclog(3,buff);
                        return(ierr_5);
           }

      istat=iok;
      done=0;
      while (done==0)
       {
        done=broadcast_read(ip,of,bp,allgrp,&istat);
       }
     printf("GETTING OUT RECEIVING\n");
     fflush(stdout);



     free(bp);


 return(istat); 
}


int load(isocket,bp,ip,isize,of,buffsize,wrerr,allgrp)
int isocket;
char *bp;
int ip;
int isize;
int of;
int buffsize;
int wrerr;
int allgrp;
{
char *cp;
int count;
int i;
int amt;
int j;
int ireturn_total;
int ireturn_segment;
char buff[300];
int sz,rc,idatatype;


        ireturn_total=0;
        printf("processing process %d  and size = %d\n",ip,isize);
        fflush(stdout);
        cp = bp;
        count = 0;
        for (i = 0; i < isize; i += buffsize)
          {
                amt = buffsize;
                if (i + amt > isize)
                        amt = isize - i;
                count += amt;
                do {
                        j = read(isocket, cp, amt);
                        if (j <= 0)
                         {
                                if (j == 0)
                                    sprintf(buff,"rcp: dropped connection");
                                else
                                    sprintf(buff,"rcp: %s",
                                        sys_errlist[errno]);
                                printclog(3,buff);
                                wrerr=1;
                                return(ierr_7);
                         }
                        amt -= j;
                        cp += j;
                    } while (amt > 0);
                if (count == buffsize)
                   {
                        ireturn_segment=broadcast_write(ip,of, bp, count,allgrp);
                        if (wrerr == 0 &&
                            ireturn_segment != count)
                                wrerr++;
                        ireturn_total=ireturn_total+ireturn_segment;
                        count = 0;
                        cp = bp;
                   }
          }
        ireturn_segment=0;
        if (count !=0)
         ireturn_segment=broadcast_write(ip,of, bp, count,allgrp);
        if (count != 0 && wrerr == 0 &&
            ireturn_segment != count)
                wrerr++;
        ireturn_total=ireturn_total+ireturn_segment;


        idatatype=1;
        if (ip== -1)
         {
          sz=0;
          MPI_Bcast(&sz,1,MPI_INT,0,MPI_COMM_WORLD);
         }
        else
         {
          if (ip!=0)
           {
            sz=0;
            MPI_Send(&sz,1,MPI_INT,ip,idatatype,MPI_COMM_WORLD);
           }
         }



 return(ireturn_total); 
}
