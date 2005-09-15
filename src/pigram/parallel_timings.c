/****************************************************************
FUNCTION NAME: elapse - an elapsed time wall clock timer
PARAMETER:   fnc  int   start (0) / stop (!0) switch
DESCRIPTION:
    The input parameter is the start/stop button.  If mode = 0, the timer
    is started.  If mode != 0, the timer is stopped and the elapsed time
    is displayed.
HISTORY: 1.0 - Tom Logan   4/97  Modified from stopwatch functions
****************************************************************/
#include <sys/time.h>
#include "asf.h"

static float wait_time=0.0, work_time=0.0, io_time=0.0;

static struct timeval work_tp1;
static struct timeval wait_tp1;
static struct timeval io_tp1;

start_work()
 {
    struct timezone tzp1;
    gettimeofday(&work_tp1,&tzp1);    
 }

stop_work()
 { 
    struct timeval tp2; 
    struct timezone tzp2;

    gettimeofday(&tp2,&tzp2);
    work_time += tp2.tv_sec-work_tp1.tv_sec;
 }

start_wait()
 {
    struct timezone tzp1;
    gettimeofday(&wait_tp1,&tzp1);    
 }

stop_wait()
 { 
    struct timeval tp2; 
    struct timezone tzp2;

    gettimeofday(&tp2,&tzp2);
    wait_time += tp2.tv_sec-wait_tp1.tv_sec;
 }

start_io()
 {
    struct timezone tzp1;
    gettimeofday(&io_tp1,&tzp1);    
 }

stop_io()
 { 
    struct timeval tp2; 
    struct timezone tzp2;

    gettimeofday(&tp2,&tzp2);
    io_time += tp2.tv_sec-io_tp1.tv_sec;
 }

report_timings(my_pe)
 {
   if (!quietflag && (my_pe==0))
     printf("   PE\t:  WORK\t:  IO\t:  WAIT\n");

   if (!quietflag) printf("%i\t:  %f\t:  %f\t:  %f\n",my_pe,work_time,io_time,wait_time);
  
 }



