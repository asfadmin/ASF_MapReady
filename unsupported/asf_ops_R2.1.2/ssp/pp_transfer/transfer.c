/* SccsId[]= @(#)transfer.c	2.41 3/24/98 */
static char sccsid_transfer[]= "@(#)PPtransfer.c:2.41";

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

/*
 * rcp
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/statfs.h>
/* #include <sys/vnode.h> */

/* #include <netinet/in.h> */

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <ctype.h>
#include <netdb.h> 
#include <errno.h> 
#include <mpi.h>

#include "error.h"

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

/*
int number_of_tasks;
int my_task_number;
*/
char status_file[256];
int file_type;

#define	ga()	 	(void) write(rem, "", 1)

int set_status(int istage, float ipercent);

/******************************************************************************/
int transfer(host,src,targ,number_of_tasks,my_task_number, file_status, i_file_type)
char *host, *src, *targ;
int number_of_tasks;
int my_task_number;
char *file_status;
int i_file_type;
{
	char *suser, *tuser, *thost;
	int i;
	char buf[BUFSIZ], cmd[16];
	struct servent *sp;
	int istatus;
	char buff[300];

	printf("start transfer\n");
	fflush(stdout);
	errs = 0;
	if (my_task_number == 0) {
	strcpy(status_file, file_status);
	file_type = i_file_type;
	if (file_type == 6) set_status(1, 0.01);
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
		printclog(3, buff);
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
        printf(" cmd=%s\n",cmd);
	fflush(stdout);

	(void) signal(SIGPIPE, lostconn);

	suser = pwd->pw_name;


        (void) sprintf(buf, "%s -f %s", cmd, src);

        printf("the host=%s\n",host);
        printf("the cmd=%s\n",cmd);
        printf("the src=%s\n",src);
        printf("the buf=%s\n",buf);
        printf(" the port=%d\n",port);
        printf(" the pw_name=%s\n",pwd->pw_name);
        printf(" the remote user=%s\n",suser);
	fflush(stdout);
        
 	rem = rcmd(&host, port, pwd->pw_name, suser, buf, 0);

	if (rem== -1)
            {
                sprintf(buff,"%s",description_str(ssp2_msg[ierr_6]));
                printclog(3,buff);
                return(ierr_6);
            }

	} /* end if task is 0 */

	MPI_Bcast(&userid,1,MPI_INT,0,MPI_COMM_WORLD);

        (void) setreuid(0, userid);
        istatus = sink(targ,number_of_tasks,my_task_number,src);
        (void) setreuid(userid, 0);
	if (my_task_number == 0) 
        	(void) close(rem);

   
        printf(" Just before exiting the big program\n");
	return(istatus); 
	
}

/******************************************************************************/
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

/******************************************************************************/
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


/******************************************************************************/
response(my_task_number)
int my_task_number;
{
	char resp, c, rbuf[BUFSIZ], *cp = rbuf;

	if (my_task_number == 0) {
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
	}
	/*NOTREACHED*/
}

/******************************************************************************/
lostconn()
{
	char buff[300];  
	if (iamremote == 0)
		sprintf(buff, "rcp: lost connection\n");
	printf("%s", buff);
	fflush(stdout);
	printclog(3, buff);
	exit(ierr_7); 
}

/******************************************************************************/

sink(targ, number_of_tasks, my_task_number,src)
char *targ;
int number_of_tasks;
int my_task_number;
char *src;
{
   int  k;
	int  m;
	off_t i, j;
	char *whopp, *cp;
	int of, mode, wrerr, exists, first, count, amt, size;
	struct buffer *bp;
	static struct buffer buffer;
	struct stat stb;
	int targisdir = 0;
	int mask = umask(0);
	char cmdbuf[BUFSIZ], nambuf[BUFSIZ];
	int setimes = 0;
	struct timeval tv[2];
	char buff[300];
	int total_bytes;
	int counter;

	MPI_Status mpi_status;
#define atime	tv[0]
#define mtime	tv[1]
#define	SCREWUP(str)	{ whopp = str; goto screwup; }


	if (!pflag)
		(void) umask(mask);
	ga();
        targisdir=0; 

	if (my_task_number == 0) {
	cp = cmdbuf;
	if (read(rem, cp, 1) <= 0){
           printf("try to read from rem and nothing back\n");
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
        printf("sink: cp=cmdbuf =%s\n",cp);

#define getnum(t) (t) = 0; while (isdigit(*cp)) (t) = (t) * 10 + (*cp++ - '0');
	if (*cp != 'C' ) {
		/*
		 * Check for the case "rcp remote:foo\* local:bar".
		 * In this case, the line "No match." can be returned
		 * by the shell before the rcp command on the remote is
		 * executed so the ^Aerror_message convention isn't
		 * followed.
		 */
		sprintf(buff,"no such file or directory: %s",src);
                printclog(3,buff);
                printerr(src);
		return(ierr_8);

                /* printf("sink: cp is not C \n"); */
		if (first) {
			error("%s\n", cp);
			return(ierr_8);
		}
		SCREWUP("expected control record");
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
	} /* end if task == 0 */

	MPI_Bcast(&size,1,MPI_INT,0,MPI_COMM_WORLD);
	MPI_Bcast(&mode,1,MPI_INT,0,MPI_COMM_WORLD); 

	(void) strcpy(nambuf, targ);
        printf("sink: nambuf=%s\n",nambuf);
	exists = stat(nambuf, &stb) == 0;

        printf(" Open the file %s\n",nambuf);
	unlink(nambuf);
	if ((of = open(nambuf, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		sprintf(buff,"Can't open %s", nambuf);
		printclog(3,buff);
		error("rcp: %s: %s\n", nambuf, sys_errlist[errno]);
		printerr(nambuf);
		return(ierr_2);
	}
	if (exists && pflag)
		(void) fchmod(of, mode);
	if (my_task_number == 0)
		ga();
	printf("BUFSIZ = %d\n", BUFSIZ);
	if ((bp = allocbuf(&buffer, of, BUFSIZ)) == 0) {
	       (void) close(of);
		sprintf(buff,"rcp: malloc: out of memory");
                printclog(3,buff);
                return(ierr_5);
	}
        printf("sink: after alloc buf\n");
        printf("sink: size = %d\n",size);
	cp = bp->buf;
	count = 0;
	wrerr = 0;

	total_bytes = 0;
	counter = 0;
	for (i = 0; i < size; i += BUFSIZ) {
		if (my_task_number == 0) {  
			amt = BUFSIZ;
			if (i + amt > size)
				amt = size - i;
			count += amt;
			do {
				j = read(rem, cp, amt);
				if (j <= 0) {
					if (j == 0)
				    		error("rcp: dropped connection");
					else
				    		error("rcp: %s\n",
							sys_errlist[errno]);
            				sprintf(buff,"rcp: %s: %s\n",nambuf,sys_errlist[errno]);
            				printclog(3,buff);
            				return(ierr_7);
				}
				amt -= j;
				cp += j;
			} while (amt > 0);

  	  	for (m=1;m<number_of_tasks;m++)
            		MPI_Send(&count,1,MPI_INT,m,1,MPI_COMM_WORLD);
		} /* end if task is 0 */

		if (my_task_number > 0)
  	    		MPI_Recv(&count,1,MPI_INT,0,1,MPI_COMM_WORLD,&mpi_status);

		if (count == bp->cnt) {
			MPI_Bcast(bp->buf,count,MPI_CHAR,0,MPI_COMM_WORLD);
			if (wrerr == 0 &&
			   write(of, bp->buf, count) != count)
				wrerr++;

			if (wrerr == 0) {
				if ((my_task_number == 0) && (file_type == 6)) {
					total_bytes += count;
					counter++;
					if (counter == 1000) {
						set_status(1, (float)total_bytes/(float)size);
						counter = 0;
					}
				}
			}

			count = 0;
			cp = bp->buf;
		} /* end if */
	} /* end for */


        printf("sink: get out the do-loop and count=%d\n",count);

	if (count !=0 && wrerr == 0)
		MPI_Bcast(bp->buf,count,MPI_CHAR,0,MPI_COMM_WORLD); 
	if (count != 0 && wrerr == 0 &&
	    write(of, bp->buf, count) != count)
		wrerr++;

	if (wrerr == 0) {
		if ((my_task_number == 0) && (file_type == 6)) {
			total_bytes += count;
			set_status(1, (float)total_bytes/(float)size);
		}
	}


	if (ftruncate(of, size))
	{
            sprintf(buff,"rcp: %s: %s\n",nambuf,sys_errlist[errno]);
            printclog(3,buff);
            return(ierr_9);
    }

   printf("sink: close the of file\n");
	(void) close(of);
   printf("sink: call response\n");
	(void) response(my_task_number);
        if(wrerr)
	{
            sprintf(buff,"rcp: %s: %s\n",nambuf,sys_errlist[errno]);
            printclog(3,buff);
            return(ierr_9);
        }
        else
            ga();
        return(iok);
screwup:
	sprintf(buff,"rcp: protocol screwup: %s", whopp);
	printclog(3,buff);
	return(ierr_16);
}

/******************************************************************************/
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

/******************************************************************************/
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

/******************************************************************************/
usage()
{
	fputs("usage: rcp  remote:f1 f2\n", stderr);
	/* exit(1); */
	/* MPI_Abort(MPI_COMM_WORLD, -1); */ /* stop all other tasks */
}
/******************************************************************************/
int set_status(int istage, float ipercent)
{
        FILE *fp;
	char buff[300];

        if ((fp=fopen(status_file,"a"))== NULL) {
		sprintf(buff,"Can't open %s", status_file);
		printclog(3,buff);
		error("rcp: %s: %s\n", status_file, sys_errlist[errno]);
		printerr(status_file);
		exit(ierr_2);
	}
        ipercent = -ipercent;
        fprintf(fp,"%d %.2f\n",istage,ipercent);
        fclose(fp);

        return 0;
}


